use crate::sea_of_nodes::nodes::node::{Cast, Not, Phi, Region, Scope, ScopeMin};
use crate::sea_of_nodes::nodes::{Node, Nodes, Op};
use crate::sea_of_nodes::parser::Parser;
use crate::sea_of_nodes::types::{Ty, Types};
use std::collections::HashMap;
use std::fmt;
//
// ScopeMinNode:
//

#[derive(Clone, Debug)]
struct Var<'t> {
    /// index in containing scope
    pub index: usize,
    /// Declared name
    pub name: &'t str,
    /// Declared type
    pub ty: Ty<'t>,
    /// Final field
    pub final_field: bool,
}

impl<'t> Var<'t> {
    fn ty(&mut self, name_to_type: HashMap<&'t str, Ty<'t>>, types: &Types<'t>) -> Ty<'t> {
        if self.ty.is_fref() {
            // Update self to no longer use the forward ref type
            let def = *name_to_type
                .get(self.ty.to_mem_ptr().unwrap().data().to.name())
                .unwrap();
            self.ty = types.meet(self.ty, def);
        }
        self.ty
    }

    fn lazy_glb(&mut self, name_to_type: HashMap<&'t str, Ty<'t>>, types: &Types<'t>) -> Ty<'t> {
        let t = self.ty(name_to_type, types);
        t.to_mem_ptr().map(|m| *m).unwrap_or_else(|| types.glb(t))
    }
}

impl<'t> fmt::Display for Var<'t> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ", self.ty)?;
        if !self.final_field {
            write!(f, "!")?;
        }
        write!(f, "{}", self.name)
    }
}

impl ScopeMin {
    fn alias(self, alias: usize, sea: &Nodes) -> Option<Node> {
        self.inputs(sea)
            .get(alias)
            .cloned()
            .flatten()
            .or_else(|| self.inputs(sea)[1])
    }

    fn alias_st(self, alias: usize, st: Option<Node>, sea: &mut Nodes) -> Option<Node> {
        while alias >= self.inputs(sea).len() {
            self.add_def(None, sea);
        }
        self.set_def(alias, st, sea);
        st
    }

    /// Read or update from memory.
    /// A shared implementation allows us to create lazy phis both during
    /// lookups and updates; the lazy phi creation is part of chapter 8.
    fn _mem(self, alias: usize, st: Option<Node>, sea: &mut Nodes) -> Option<Node> {
        // Memory projections are made lazily; if one does not exist
        // then it must be START.proj(1)
        let mut old = self.alias(alias, sea);
        if let Some(loop_) = old.and_then(|o| o.to_scope(sea)) {
            let loopmem = loop_.mem(sea);
            let memdef = loopmem.alias(alias, sea);
            // Lazy phi!
            old = if memdef.is_some_and(|m| {
                m.to_phi(sea)
                    .is_some_and(|p| loop_.ctrl(sea) == Some(*p.region(sea)))
            }) {
                // Loop already has a real Phi, use it
                memdef
            } else {
                // Set real Phi in the loop head
                // The phi takes its one input (no backedge yet) from a recursive
                // lookup, which might have insert a Phi in every loop nest.
                let name = sea.types.get_str(&Parser::mem_name(alias as u32));
                let phi = Phi::new(
                    name,
                    sea.types.mem_bot,
                    vec![loop_.ctrl(sea), loopmem._mem(alias, None, sea), None],
                    sea,
                )
                .peephole(sea);
                loopmem.alias_st(alias, Some(phi), sea)
            };
            self.alias_st(alias, old, sea);
        }
        // Memory projections are made lazily; expand as needed
        if st.is_none() {
            old
        } else {
            self.alias_st(alias, st, sea) // Not lazy, so this is the answer
        }
    }

    fn _merge_min(self, that: ScopeMin, r: Region, sea: &mut Nodes) {
        let len = self.inputs(sea).len().max(that.inputs(sea).len());
        for i in 2..len {
            // No need for redundant Phis
            if self.alias(i, sea) != that.alias(i, sea) {
                // If we are in lazy phi mode we need to a lookup
                // by name as it will trigger a phi creation
                //Var v = _vars.at(i);
                let lhs = self._mem(i, None, sea);
                let rhs = that._mem(i, None, sea);

                let name = sea.types.get_str(&Parser::mem_name(i as u32));
                let phi =
                    Phi::new(name, sea.types.mem_bot, vec![Some(**r), lhs, rhs], sea).peephole(sea);
                self.alias_st(i, Some(phi), sea);
            }
        }
    }

    /// Fill in the backedge of any inserted Phis
    fn _end_loop_mem(self, scope: Scope, back: ScopeMin, exit: ScopeMin, sea: &mut Nodes) {
        for i in 2..back.inputs(sea).len() {
            if back.inputs(sea)[i] != Some(scope.to_node()) {
                let phi = self.inputs(sea)[i].unwrap().to_phi(sea).unwrap();
                debug_assert!(
                    Some(phi.region(sea).to_node()) == scope.ctrl(sea)
                        && phi.inputs(sea)[2].is_none()
                );
                phi.set_def(2, back.inputs(sea)[i], sea); // Fill backedge
            }
            if exit.alias(i, sea) == Some(scope.to_node()) {
                // Replace a lazy-phi on the exit path also
                exit.alias_st(i, self.inputs(sea)[i], sea);
            }
        }
    }

    /// Now one-time do a useless-phi removal
    fn _useless(self, sea: &mut Nodes) {
        for i in 2..self.inputs(sea).len() {
            if let Some(phi) = self.inputs(sea)[i].and_then(|i| i.to_phi(sea)) {
                // Do an eager useless-phi removal
                let inp = phi.peephole(sea);
                for &o in &sea.outputs[phi] {
                    sea.iter_peeps.add(o);
                }
                phi.move_deps_to_worklist(sea);
                if inp != *phi {
                    if !phi.is_keep(sea) {
                        // Keeping phi around for parser elsewhere
                        phi.subsume(inp, sea);
                    }
                    self.set_def(i, inp, sea); // Set the update back into Scope
                }
            }
        }
    }
}

//
// ScopeNode:
//

#[derive(Clone, Debug)]
pub struct ScopeOp<'t> {
    /// All active/live variables in all nested scopes, all run together
    pub vars: Vec<Var<'t>>,

    /// Since of each nested lexical scope
    pub lex_size: Vec<usize>,

    /// True if parsing inside of a constructor
    pub in_cons: Vec<bool>,

    /// Extra guards; tested predicates and casted results
    pub guards: Vec<Node>,
}

impl Scope {
    pub const CTRL: &'static str = "$ctrl";
    pub const ARG0: &'static str = "arg";
    pub const MEM0: &'static str = "$mem";

    fn ctrl(self, sea: &Nodes) -> Option<Node> {
        self.inputs(sea)[0]
    }
    fn mem(self, sea: &Nodes) -> ScopeMin {
        self.inputs(sea)[1].unwrap().to_scope_min(sea).unwrap()
    }

    /// The ctrl of a ScopeNode is always bound to the currently active
    /// control node in the graph, via a special name '$ctrl' that is not
    /// a valid identifier in the language grammar and hence cannot be
    /// referenced in Simple code.
    fn set_ctrl(self, n: Option<Node>, sea: &mut Nodes) {
        self.set_def(0, n, sea);
    }

    pub fn push(self, in_constructor: bool, sea: &mut Nodes) {
        let data = &mut sea[self];
        debug_assert_eq!(data.lex_size.len(), data.in_cons.len());
        data.lex_size.push(data.vars.len());
        data.in_cons.push(in_constructor);
    }

    pub fn pop(self, sea: &mut Nodes) {
        debug_assert_eq!(sea[self].lex_size.len(), sea[self].in_cons.len());
        let n = sea[self].lex_size.pop().unwrap();
        sea[self].in_cons.pop().unwrap();
        self.pop_until(n, sea);
        sea[self].vars.truncate(n);
    }

    pub fn in_con(self, sea: &Nodes) -> bool {
        *sea[self].in_cons.last().unwrap()
    }

    /// Find name in reverse, return an index into _vars or -1.  Linear scan
    /// instead of hashtable, but probably doesn't matter until the scan
    /// typically hits many dozens of variables.
    fn find(self, name: &str, sea: &Nodes) -> Option<usize> {
        sea[self].vars.iter().rposition(|v| v.name == name)
    }

    /// Create a new variable name in the current scope
    pub fn define<'t>(
        self,
        name: &'t str,
        declared_ty: Ty<'t>,
        final_field: bool,
        init: Node,
        sea: &mut Nodes<'t>,
    ) -> Result<(), ()> {
        debug_assert!(
            name.chars().next() != Some('$') || sea[self].lex_size.len() == 1,
            "Later scopes do not define memory"
        );
        if sea[self].lex_size.len() > 1 {
            for v in &sea[self].vars[*sea[self].lex_size.last().unwrap()..] {
                if v.name == name {
                    return Err(()); // Double define
                }
            }
        }
        let index = self.inputs(sea).len();
        sea[self].vars.push(Var {
            index,
            name,
            ty: declared_ty,
            final_field,
        });
        self.add_def(Some(init), sea);
        Ok(())
    }

    /// Read from memory
    fn mem_read(self, alias: usize, sea: &mut Nodes) -> Option<Node> {
        self.mem(sea)._mem(alias, None, sea)
    }

    /// Write to memory
    fn mem_write(self, alias: usize, st: Node, sea: &mut Nodes) {
        self.mem(sea)._mem(alias, Some(st), sea);
    }

    /// Lookup a name in all scopes starting from most deeply nested.
    pub fn lookup(self, name: &str, sea: &mut Nodes) -> Result<usize, ()> {
        let index = self.find(name, sea).ok_or(())?;
        self.update_var_index(index, None, sea);
        Ok(index)
    }

    /// If the name is present in any scope, then redefine else null
    pub fn update(self, name: &str, value: Node, sea: &mut Nodes) {
        let index = self.find(name, sea).unwrap();
        self.update_var_index(index, Some(value), sea);
    }

    fn update_var_index(self, var_index: usize, st: Option<Node>, sea: &mut Nodes) {
        let mut old = self.inputs(sea)[var_index];
        if let Some(loop_) = old.and_then(|o| o.to_scope(sea)) {
            // Lazy Phi!
            let def = loop_.inputs(sea)[var_index];
            old = if def
                .and_then(|d| d.to_phi(sea))
                .is_some_and(|p| loop_.ctrl(sea) == Some(*p.region(sea)))
            {
                // Loop already has a real Phi, use it
                def
            } else {
                // Set real Phi in the loop head
                // The phi takes its one input (no backedge yet) from a recursive
                // lookup, which might have insert a Phi in every loop nest.
                let name = &sea[self].vars[var_index].name;
                let lglb = sea[self].vars[var_index].lazy_glb();
                loop_.update_var_index(var_index, None, sea);
                let inputs = vec![loop_.ctrl(sea), loop_.inputs(sea)[var_index], None];
                let phi = Phi::new(name, lglb, inputs, sea).peephole(sea);
                loop_.set_def(var_index, phi, sea);
                Some(phi)
            };
            self.set_def(var_index, old, sea);
        }
        if st.is_some() {
            self.set_def(var_index, st, sea); // Set new value
        }
    }

    /// Duplicate a ScopeNode; including all levels, up to Nodes.  So this is
    /// neither shallow (would dup the Scope but not the internal HashMap
    /// tables), nor deep (would dup the Scope, the HashMap tables, but then
    /// also the program Nodes).
    ///
    /// If the {@code loop} flag is set, the edges are filled in as the original
    /// Scope, as an indication of Lazy Phis at loop heads.  The goal here is to
    /// not make Phis at loop heads for variables which are never touched in the
    /// loop body.
    ///
    /// The new Scope is a full-fledged Node with proper use<->def edges.
    pub fn dup(self, lazy_loop_phis: bool, sea: &mut Nodes) -> Scope {
        // Our goals are:
        // 1) duplicate the name bindings of the ScopeNode across all stack levels
        // 2) Make the new ScopeNode a user of all the nodes bound
        // 3) Ensure that the order of defs is the same to allow easy merging
        let clone = sea[self].clone();

        // The dup'd guards all need dup'd keepers, to keep proper accounting
        // when later removing all guards
        for &n in &clone.guards {
            if !n.is_cfg(sea) {
                n.keep(sea);
            }
        }

        let dup = sea
            .create((Op::Scope(clone), vec![]))
            .to_scope(sea)
            .unwrap();

        dup.add_def(self.ctrl(sea), sea); // Control input is just copied

        // Memory input is a shallow copy
        let memdup = ScopeMin::new(sea);
        let mem = self.mem(sea);
        memdup.add_def(None, sea);
        memdup.add_def(
            if lazy_loop_phis {
                Some(self.to_node())
            } else {
                mem.inputs(sea)[1]
            },
            sea,
        );
        for i in 2..mem.inputs(sea).len() {
            // For lazy phis on loops we use a sentinel
            // that will trigger phi creation on update
            memdup.add_def(
                if lazy_loop_phis {
                    Some(self.to_node())
                } else {
                    mem.inputs(sea)[i]
                },
                sea,
            );
        }
        dup.add_def(Some(memdup.to_node()), sea);

        // Copy of other inputs
        for i in 2..self.inputs(sea).len() {
            // For lazy phis on loops we use a sentinel
            // that will trigger phi creation on update
            dup.add_def(
                if lazy_loop_phis {
                    Some(self.to_node())
                } else {
                    self.inputs(sea)[i]
                },
                sea,
            );
        }
        dup
    }

    pub fn merge_scopes(self, that: Scope, sea: &mut Nodes) -> Region {
        let r = Region::new(vec![None, self.ctrl(sea), that.ctrl(sea)], sea);
        self.set_ctrl(Some(r.keep(sea)), sea);
        self.mem(sea)._merge_min(that.mem(sea), r, sea);
        self._merge(that, r, sea);
        that.kill(sea); // Kill merged scope
        sea.iter_peeps.add(r.to_node());
        r.unkeep(sea);
        r
    }

    fn _merge(self, that: Scope, r: Region, sea: &mut Nodes) {
        for i in 2..self.inputs(sea).len() {
            // No need for redundant Phis
            if self.inputs(sea)[i] != that.inputs(sea)[i] {
                // If we are in lazy phi mode we need to a lookup
                // by name as it will trigger a phi creation

                self.update_var_index(i, None, sea);
                that.update_var_index(i, None, sea); // differs from java: lazy glb is potentially updated in `that`, not just in our `v`

                let lhs = self.inputs(sea)[i];
                let rhs = that.inputs(sea)[i];

                let name = sea[self].vars[i].name;
                let ty = sea[self].vars[i].ty();
                self.set_def(
                    i,
                    Phi::new(name, ty, vec![Some(r.to_node()), lhs, rhs], sea).peephole(sea),
                    sea,
                );
            }
        }
    }

    // peephole the backedge scope into this loop head scope
    // We set the second input to the phi from the back edge (i.e. loop body)
    pub fn end_loop(self, back: Scope, exit: Scope, sea: &mut Nodes) {
        let ctrl = self.ctrl(sea).unwrap();
        assert!(matches!(&sea[ctrl], Op::Loop));
        assert!(Nodes::in_progress(&sea.ops, &sea.inputs, ctrl));
        ctrl.set_def(2, back.ctrl(sea), sea);

        self.mem(sea)
            ._end_loop_mem(self, back.mem(sea), exit.mem(sea), sea);
        self._end_loop(self, back, exit, sea);
        back.kill(sea); // Loop backedge is dead

        // Now one-time do a useless-phi removal
        self.mem(sea)._useless(sea);
        self._useless(sea);

        // The exit mem's lazy default value had been the loop top,
        // now it goes back to predating the loop.
        exit.mem(sea).set_def(1, self.mem(sea).inputs(sea)[1], sea);
    }

    /// Fill in the backedge of any inserted Phis
    fn _end_loop(self, back: Scope, exit: Scope, sea: &mut Nodes) {
        for i in 2..self.inputs(sea).len() {
            if back.inputs(sea)[i] != Some(**self) {
                let phi = self.inputs(sea)[i].unwrap().to_phi(sea).unwrap();
                assert_eq!(Some(*phi.region(sea)), self.ctrl(sea));
                assert_eq!(phi.inputs(sea)[2], None);
                phi.set_def(2, back.inputs(sea)[i], sea); // Fill backedge
            }
            if exit.inputs(sea)[i] == Some(**self) {
                // Replace a lazy-phi on the exit path also
                exit.set_def(i, self.inputs(sea)[i], sea)
            }
        }
    }

    /// Up-casting: using the results of an If to improve a value.
    /// E.g. "if( ptr ) ptr.field;" is legal because ptr is known not-null.
    fn add_guards(self, ctrl: Node, pred: Option<Node>, invert: bool, sea: &mut Nodes) {
        debug_assert!(ctrl.is_cfg(sea));
        sea[self].guards.push(ctrl); // Marker to distinguish 0,1,2 guards

        // add pred & its cast to the normal input list, with special Vars
        let Some(mut pred) = pred else { return };
        if ctrl.ty(sea) == Some(sea.types.xctrl) || pred.is_dead(sea) {
            return; // Dead, do not add any guards
        }

        // Invert the If conditional
        if invert {
            pred = if let Some(not) = pred.to_not(sea) {
                not.inputs(sea)[1].unwrap()
            } else {
                let n = Not::new(pred, sea).peephole(sea);
                sea.iter_peeps.add(n);
                n
            }
        }

        // This is a zero/null test.
        // Compute the positive test type.
        let tnz = pred.ty(sea).unwrap().non_zero();
        let tcast = sea.types.join(tnz, pred.ty(sea).unwrap());
        if tcast != pred.ty(sea).unwrap() {
            let cast = Cast::new(tnz, ctrl, pred.keep(sea), sea)
                .peephole(sea)
                .keep(sea);
            sea[self].guards.push(pred);
            sea[self].guards.push(cast);
            self.replace(pred, Some(cast), sea);
        }

        // Compute the negative test type.
        if let Some(not) = pred.to_not(sea) {
            let npred = not.inputs(sea)[1].unwrap();
            let tzero = npred.ty(sea).unwrap().make_zero();
            let tzcast = sea.types.join(tzero, npred.ty(sea).unwrap());
            if tzcast != npred.ty(sea).unwrap() {
                let cast = Cast::new(tzero, ctrl, npred.keep(sea), sea)
                    .peephole(sea)
                    .keep(sea);
                sea[self].guards.push(npred);
                sea[self].guards.push(cast);
                self.replace(npred, Some(cast), sea);
            }
        }
    }

    /// Remove matching pred/cast pairs from this guarded region.
    fn remove_guards(self, ctrl: Node, sea: &mut Nodes) {
        debug_assert!(ctrl.is_cfg(sea));
        // 0,1 or 2 guards
        loop {
            let g = sea[self].guards.pop().unwrap();
            if g == ctrl {
                break;
            }
            if g.is_cfg(sea) {
                continue;
            }
            g.unkill(sea); // Pop/kill cast
            sea[self].guards.pop().unwrap().unkill(sea); // Pop/kill pred
        }
    }

    /// If we find a guarded instance of pred, replace with the upcasted version
    fn upcast_guard(self, pred: Node, sea: &mut Nodes) -> Node {
        // If finding an instanceof pred, replace with cast.
        // Otherwise, just pred itself.
        let mut i = sea[self].guards.len();
        while i > 0 {
            i -= 1;
            let cast = sea[self].guards[i];
            if cast.is_cfg(sea) {
                continue; // Marker between guard sets
            }
            i -= 1;
            let xpred = sea[self].guards[i];
            if xpred == pred {
                return cast;
            }
        }
        pred
    }

    fn replace(self, old: Node, cast: Option<Node>, sea: &mut Nodes) {
        debug_assert_ne!(Some(old), cast);
        for i in 0..self.inputs(sea).len() {
            if self.inputs(sea)[i] == Some(old) {
                self.set_def(i, cast, sea);
            }
        }
    }
}
