use std::collections::HashMap;

use crate::sea_of_nodes::nodes::index::{Constant, Scope};
use crate::sea_of_nodes::nodes::{Node, Nodes, Op};
use crate::sea_of_nodes::types::{Ty, Type};

#[derive(Clone, Debug)]
pub struct ScopeOp<'t> {
    pub scopes: Vec<HashMap<&'t str, (usize, Ty<'t>)>>,
}

impl<'t> ScopeOp<'t> {
    pub fn lookup(&self, name: &str) -> Option<&(usize, Ty<'t>)> {
        self.scopes.iter().rev().flat_map(|x| x.get(name)).next()
    }
}

impl<'t> Scope {
    pub const CTRL: &'static str = "$ctrl";
    pub const ARG0: &'static str = "arg";

    pub fn push(self, sea: &mut Nodes<'t>) {
        sea[self].scopes.push(HashMap::new());
    }

    pub fn pop(self, sea: &mut Nodes<'t>) {
        let last = sea[self].scopes.pop().unwrap();
        sea.pop_n(*self, last.len());
    }

    pub fn define(
        self,
        name: &'t str,
        declared_ty: Ty<'t>,
        value: Node,
        sea: &mut Nodes<'t>,
    ) -> Result<(), ()> {
        let len = self.inputs(sea).len();
        let syms = sea[self].scopes.last_mut().unwrap();
        if let Some(_old) = syms.insert(name, (len, declared_ty)) {
            return Err(());
        }
        sea.add_def(*self, Some(value));
        Ok(())
    }

    pub fn lookup(self, name: &str, sea: &mut Nodes<'t>) -> Result<Node, ()> {
        let nesting_level = sea[self].scopes.len() - 1;
        self.lookup_update(name, None, nesting_level, sea).ok_or(())
    }

    pub fn update(self, name: &str, value: Node, sea: &mut Nodes<'t>) -> Result<Node, ()> {
        let nesting_level = sea[self].scopes.len() - 1;
        self.lookup_update(name, Some(value), nesting_level, sea)
            .ok_or(())
    }

    fn lookup_update(
        self,
        name: &str,
        value: Option<Node>,
        nesting_level: usize,
        sea: &mut Nodes<'t>,
    ) -> Option<Node> {
        let syms = &sea[self].scopes[nesting_level];
        if let Some((index, declared_ty)) = syms.get(name).copied() {
            let mut old = self.inputs(sea)[index];

            if let Some(loop_) = old {
                if let Some(loop_) = loop_.to_scope(sea) {
                    // Lazy Phi!
                    let loop_phi = loop_.inputs(sea)[index];
                    old = if loop_phi.is_some_and(|p| {
                        p.to_phi(sea).is_some() && loop_.inputs(sea)[0] == p.inputs(sea)[0]
                    }) {
                        // Loop already has a real Phi, use it
                        loop_phi
                    } else {
                        // Set real Phi in the loop head
                        // The phi takes its one input (no backedge yet) from a recursive
                        // lookup, which might have insert a Phi in every loop nest.

                        let name = sea.types.get_str(name);
                        let recursive = loop_.lookup_update(name, None, nesting_level, sea);
                        let new_phi = sea.create_peepholed(Op::make_phi(
                            name,
                            declared_ty,
                            vec![loop_.inputs(sea)[0], recursive, None],
                        ));

                        sea.set_def(*loop_, index, Some(new_phi));
                        Some(new_phi)
                    };
                    sea.set_def(*self, index, old);
                }
            }

            // Not lazy, so this is the answer
            if value.is_some() {
                sea.set_def(*self, index, value);
                value
            } else {
                old
            }
        } else if nesting_level > 0 {
            self.lookup_update(name, value, nesting_level - 1, sea)
        } else {
            None
        }
    }

    pub fn dup(self, lazy_loop_phis: bool, sea: &mut Nodes<'t>) -> Scope {
        let clone = sea[self].clone();
        let dup = sea
            .create((Op::Scope(clone), vec![]))
            .to_scope(sea)
            .unwrap();

        sea.add_def(*dup, self.inputs(sea)[0]); // ctrl
        for i in 1..self.inputs(sea).len() {
            // For lazy phis on loops we use a sentinel
            // that will trigger phi creation on update
            sea.add_def(
                *dup,
                if lazy_loop_phis {
                    Some(*self)
                } else {
                    self.inputs(sea)[i]
                },
            );
        }
        dup
    }

    pub fn reverse_names(self, sea: &Nodes<'t>) -> Vec<Option<&'t str>> {
        let mut names = vec![None; self.inputs(sea).len()];
        for syms in &sea[self].scopes {
            for (&name, &(index, _)) in syms {
                debug_assert!(names[index].is_none());
                names[index] = Some(name);
            }
        }
        names
    }

    pub fn merge(self, that: Scope, sea: &mut Nodes<'t>) -> Node {
        let c1 = self.inputs(sea)[0];
        let c2 = that.inputs(sea)[0];
        let region = sea.create(Op::make_region(vec![None, c1, c2]));
        sea.keep(region);
        sea.set_def(*self, 0, Some(region)); // set ctrl

        let names = self.reverse_names(sea);

        // Note that we skip i==0, which is bound to '$ctrl'
        for (i, name) in names.into_iter().enumerate().skip(1) {
            let this_in = self.inputs(sea)[i];
            let that_in = that.inputs(sea)[i];
            if this_in != that_in {
                // If we are in lazy phi mode we need to a lookup
                // by name as it will trigger a phi creation
                let name = name.unwrap();
                let this_l = self.lookup(name, sea).unwrap();
                let that_l = that.lookup(name, sea).unwrap();

                let declared_ty = sea[self].lookup(name).unwrap().1;
                let phi = sea.create_peepholed(Op::make_phi(
                    name,
                    declared_ty,
                    vec![Some(region), Some(this_l), Some(that_l)],
                ));
                sea.set_def(*self, i, Some(phi));
            }
        }

        sea.kill(*that);
        sea.iter_peeps.add(region);

        sea.unkeep(region);
        region.peephole(sea)
    }

    /// Merge the backedge scope into this loop head scope
    /// We set the second input to the phi from the back edge (i.e. loop body)
    pub fn end_loop(self, back: Scope, exit: Scope, sea: &mut Nodes<'t>) {
        let ctrl = self.inputs(sea)[0].unwrap();
        assert!(matches!(&sea[ctrl], Op::Loop));
        assert!(Nodes::in_progress(&sea.ops, &sea.inputs, ctrl));

        sea.set_def(ctrl, 2, back.inputs(sea)[0]);

        for i in 1..self.inputs(sea).len() {
            if back.inputs(sea)[i] != Some(*self) {
                let phi = self.inputs(sea)[i].unwrap();
                assert_eq!(phi.inputs(sea)[0], Some(ctrl));
                assert_eq!(phi.inputs(sea)[2], None);
                sea.set_def(phi, 2, back.inputs(sea)[i]);
            }
            if exit.inputs(sea)[i] == Some(*self) {
                // Replace a lazy-phi on the exit path also
                sea.set_def(*exit, i, self.inputs(sea)[i])
            }
        }

        sea.kill(*back); // Loop backedge is dead

        // Now one-time do a useless-phi removal
        for i in 1..self.inputs(sea).len() {
            if let Some(phi) = self.inputs(sea)[i] {
                if let Op::Phi(_) = &sea[phi] {
                    // Do an eager useless-phi removal
                    let in_ = phi.peephole(sea);
                    for &o in &sea.outputs[phi] {
                        sea.iter_peeps.add(o);
                    }
                    sea.move_deps_to_worklist(phi);
                    if in_ != phi {
                        // Keeping phi around for parser elsewhere
                        if !sea.is_keep(phi) {
                            sea.subsume(phi, in_);
                        }
                        sea.set_def(*self, i, Some(in_)); // Set the update back into Scope
                    }
                }
            }
        }
    }

    /// Up-casting: using the results of an If to improve a value.
    /// E.g. "if( ptr ) ptr.field;" is legal because ptr is known not-null.
    ///
    /// This Scope looks for direct variable uses, or certain simple
    /// combinations, and replaces the variable with the upcast variant.
    pub fn upcast(
        self,
        ctrl: Node,
        mut pred: Node,
        invert: bool,
        sea: &mut Nodes<'t>,
    ) -> Option<Node> {
        if ctrl.ty(sea)? == sea.types.ty_xctrl {
            return None;
        }
        // Invert the If conditional
        if invert {
            if pred.to_not(sea).is_some() {
                pred = pred.inputs(sea)[1].unwrap()
            } else {
                pred = sea.create_peepholed(Op::make_not(pred));
                sea.iter_peeps.add(pred);
            }
        }

        // Direct use of a value as predicate.  This is a zero/null test.
        if self.inputs(sea).iter().any(|x| *x == Some(pred)) {
            let tmp = pred.ty(sea).unwrap();
            let Type::Pointer(_) = *tmp else {
                // Must be an `int`, since int and ptr are the only two value types
                // being tested. No representation for a generic not-null int, so no upcast.
                return None;
            };
            if sea.types.isa(tmp, sea.types.ty_pointer_void) {
                return None; // Already not-null, no reason to upcast
            }
            // Upcast the ptr to not-null ptr, and replace in scope
            let c = sea.create_peepholed(Op::make_cast(sea.types.ty_pointer_void, ctrl, pred));
            let t = c.compute(sea);
            c.set_type(t, sea);
            self.replace(pred, Some(c), sea);
        }

        if pred.to_not(sea).is_some() {
            // Direct use of a !value as predicate.  This is a zero/null test.
            let not_in_1 = pred.inputs(sea)[1].unwrap();
            if self.inputs(sea).iter().any(|x| *x == Some(not_in_1)) {
                let t = not_in_1.ty(sea).unwrap();
                let tinit = sea.types.make_init(t).unwrap();
                return if sea.types.isa(t, tinit) {
                    None // Already zero/null, no reason to upcast
                } else {
                    let c = Constant::new(tinit, sea).peephole(sea);
                    self.replace(not_in_1, Some(c), sea);
                    Some(c)
                };
            }
        }

        // Apr/9/2024: Attempted to replace X with Y if guarded by a test of
        // X==Y.  This didn't seem to help very much, or at least in the test
        // cases seen so far was a very minor help.

        // No upcast
        None
    }

    fn replace(self, old: Node, cast: Option<Node>, sea: &mut Nodes<'t>) {
        debug_assert_ne!(Some(old), cast);
        for i in 0..self.inputs(sea).len() {
            if self.inputs(sea)[i] == Some(old) {
                sea.set_def(*self, i, cast);
            }
        }
    }
}
