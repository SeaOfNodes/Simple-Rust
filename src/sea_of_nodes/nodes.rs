use std::collections::HashMap;
use std::num::NonZeroU32;

use crate::datastructures::id_set::IdSet;
use crate::datastructures::id_vec::IdVec;
use crate::sea_of_nodes::nodes::cfg::{CfgData, LoopTree, LoopTreeData};
use crate::sea_of_nodes::nodes::gvn::GvnEntry;
use crate::sea_of_nodes::nodes::node::{CProj, ToFloat, TypedNode};
use crate::sea_of_nodes::types::{Ty, Types};
use iter_peeps::IterPeeps;
pub use iter_peeps::WorkList;
pub use node::{
    BoolOp, Cfg, Constant, LoadOp, Node, Op, Phi, Proj, ProjOp, Scope, Start, StartOp, StoreOp,
    XCtrl,
};
pub use scope::ScopeOp;

mod cfg;
mod gvn;
mod idealize;
mod iter_peeps;
pub mod node;
mod peephole;
mod print;
mod scope;

pub struct OpVec<'t>(IdVec<Node, Op<'t>>);

/// Using `IdVec` has two advantages over `Vec`+helper methods:
/// 1) `self.inputs[x]` and `self.outputs[x]` can be borrowed simultaneously
///    while `self.inputs(x)` and `self.outputs_mut(x)` can't
/// 2) methods like `self.inputs(x)` and `self.inputs_mut(x)` require two versions for mutability
///    while `self.inputs[x]` automatically decides
pub struct Nodes<'t> {
    /// indexed by self[id]
    pub ops: OpVec<'t>,

    pub ty: IdVec<Node, Option<Ty<'t>>>,

    /// Inputs to the node. These are use-def references to Nodes.
    ///
    /// Generally fixed length, ordered, nulls allowed, no unused
    /// trailing space. Ordering is required because e.g. "a/ b"
    /// is different from "b/ a". The first input (offset 0) is
    /// often a isCFG node.
    pub inputs: IdVec<Node, Vec<Option<Node>>>,

    /// Outputs reference Nodes that are not null and have this Node
    /// as an input. These nodes are users of this node, thus these
    /// are def-use references to Nodes.
    ///
    /// Outputs directly match inputs, making a directed graph that
    /// can be walked in either direction. These outputs are typically
    /// used for efficient optimizations but otherwise have no semantics
    /// meaning
    pub outputs: IdVec<Node, Vec<Node>>,

    /// Some of the peephole rules get complex, and search further afield than
    /// just the nearest neighbor.  These peepholes can fail the pattern match
    /// on a node some distance away, and if that node ever changes we should
    /// retry the peephole.  Track a set of Nodes dependent on `this`, and
    /// revisit them if `this` changes.
    pub deps: IdVec<Node, Vec<Node>>,

    /// Control Flow Graph Nodes
    /// CFG nodes have a immediate dominator depth (idepth) and a loop nesting depth(loop_depth).
    pub cfg: IdVec<Cfg, CfgData>,

    /// If this is true peephole only computes the type.
    pub disable_peephole: bool,

    /// the start node to be used for creating constants.
    pub start: Start,

    /// zero constant node
    pub zero: Constant,

    /// xctrl constant node
    pub xctrl: XCtrl,

    /// Creating nodes such as constants and computing peepholes requires
    /// interning new types and operations such as meet and join.
    pub types: &'t Types<'t>,
    pub tys: &'t Types<'t>,

    /// Worklist for iterative peepholes
    iter_peeps: IterPeeps,

    pub iter_cnt: usize,
    pub iter_nop_cnt: usize,

    walk_visited: IdSet<Node>,

    /// Global Value Numbering. Hash over opcode and inputs; hits in this table
    /// are structurally equal.
    gvn: HashMap<GvnEntry, ()>,

    /// Cached hash.  If zero, then not computed AND this Node is NOT in the GVN
    /// table - and can have its edges hacked (which will change his hash
    /// anyway).  If Non-Zero then this Node is IN the GVN table, or is being
    /// probed to see if it can be inserted.  His edges are "locked", because
    /// hacking his edges will change his hash.
    hash: IdVec<Node, Option<NonZeroU32>>,

    /// True if debug printer can use schedule info
    pub scheduled: bool,

    /// Mapping from a type name to a Type.  The string name matches
    /// `type.str()` call.  No TypeMemPtrs are in here, because Simple does not
    /// have C-style '*ptr' references.
    ///
    /// differs from java: this doesn't live in the parser, because function
    /// in scope.rs had to access it to resolve types
    pub name_to_type: HashMap<&'t str, Ty<'t>>,

    pub loop_tree: IdVec<LoopTree, LoopTreeData>,
}

pub type NodeCreation<'t> = (Op<'t>, Vec<Option<Node>>);

impl<'t> Nodes<'t> {
    pub fn new(types: &'t Types<'t>) -> Self {
        let dummy = Op::Stop;
        Nodes {
            ops: OpVec(IdVec::new(vec![dummy])),
            inputs: IdVec::new(vec![vec![]]),
            outputs: IdVec::new(vec![vec![]]),
            ty: IdVec::new(vec![None]),
            deps: IdVec::new(vec![vec![]]),
            cfg: IdVec::new(vec![CfgData::new()]),
            disable_peephole: false,
            // TODO get rid of DUMMYs
            start: Start::DUMMY,
            zero: Constant::DUMMY,
            xctrl: XCtrl::DUMMY,
            types,
            tys: types,
            iter_peeps: IterPeeps::new(),
            iter_cnt: 0,
            iter_nop_cnt: 0,
            walk_visited: IdSet::zeros(0),
            gvn: HashMap::new(),
            hash: IdVec::new(vec![None]),
            scheduled: false,
            name_to_type: HashMap::new(),
            loop_tree: IdVec::new(vec![LoopTreeData::DUMMY]),
        }
    }
    pub fn len(&self) -> usize {
        self.ops.0.len()
    }

    pub fn create(&mut self, (op, inputs): NodeCreation<'t>) -> Node {
        let id = u32::try_from(self.len())
            .and_then(NonZeroU32::try_from)
            .map(Node)
            .unwrap();
        self.ops.0.push(op);
        self.inputs.push(inputs);
        self.outputs.push(vec![]);
        self.ty.push(None);
        self.deps.push(vec![]);
        self.cfg.push(CfgData::new());
        self.hash.push(None);
        for i in 0..self.inputs[id].len() {
            if let Some(input) = self.inputs[id][i] {
                input.add_use(id, self);
            }
        }

        debug_assert_eq!(self.len(), self.inputs.len());
        debug_assert_eq!(self.len(), self.outputs.len());
        debug_assert_eq!(self.len(), self.ty.len());
        id
    }
}

impl Node {
    pub fn is_dead(self, sea: &Nodes) -> bool {
        self.is_unused(sea) && self.inputs(sea).is_empty() && self.ty(sea).is_none()
    }

    /// Shortcut for "popping" until n nodes.  A "pop" is basically a
    //  setDef(last,null) followed by lowering the nIns() count.
    pub fn pop_until(self, n: usize, sea: &mut Nodes) {
        self.unlock(sea);
        while self.inputs(sea).len() > n {
            let old_def = sea.inputs[self].pop().unwrap();
            if let Some(old_def) = old_def {
                old_def.del_use(self, sea);
                if old_def.is_unused(sea) {
                    old_def.kill(sea);
                }
            }
        }
    }

    pub fn kill(self, sea: &mut Nodes) {
        if let Some(s) = self.to_scope(sea) {
            for n in 0..sea[s].guards.len() {
                let n = sea[s].guards[n];
                if !n.is_cfg(sea) {
                    n.unkill(sea);
                }
            }
            sea[s].guards.clear();
        }
        self.unlock(sea);
        self.move_deps_to_worklist(sea);
        debug_assert!(self.is_unused(sea), "Has no uses, so it is dead");
        // Set all inputs to null, recursively killing unused Nodes
        while let Some(old_def) = sea.inputs[self].pop() {
            if let Some(old_def) = old_def {
                sea.iter_peeps.add(old_def); // Revisit neighbor because removed use
                old_def.del_use(self, sea);
                if old_def.is_unused(sea) {
                    old_def.kill(sea); // If we removed the last use, the old def is now dead
                }
            }
        }
        sea.inputs[self] = vec![]; // deallocate
        sea.ty[self] = None; // flag as dead
        debug_assert!(self.is_dead(sea));
    }

    /// Replace 'this' with nnn in the graph, making 'this' go dead
    pub fn subsume(self, nnn: Node, sea: &mut Nodes) {
        assert_ne!(self, nnn);
        while let Some(n) = sea.outputs[self].pop() {
            n.unlock(sea);
            let idx = n.inputs(sea).iter().position(|&x| x == Some(self)).unwrap();
            sea.inputs[n][idx] = Some(nnn);
            nnn.add_use(n, sea);
            for &o in &sea.outputs[n] {
                sea.iter_peeps.add(o);
            }
        }
        self.kill(sea);
    }

    pub fn set_def<N: Into<Option<Node>>>(self, index: usize, new_def: N, sea: &mut Nodes) {
        let new_def = new_def.into();
        self.unlock(sea);

        let old_def = self.inputs(sea)[index];
        if old_def == new_def {
            return;
        }

        if let Some(new_def) = new_def {
            new_def.add_use(self, sea);
        }

        sea.inputs[self][index] = new_def;

        if let Some(old_def) = old_def {
            old_def.del_use(self, sea);
            if old_def.is_unused(sea) {
                old_def.kill(sea);
            }
        }

        self.move_deps_to_worklist(sea);
    }

    pub fn add_def(self, new_def: impl Into<Option<Node>>, sea: &mut Nodes) {
        let new_def = new_def.into();
        self.unlock(sea);
        sea.inputs[self].push(new_def);
        if let Some(new_def) = new_def {
            new_def.add_use(self, sea);
        }
    }

    /// Remove the numbered input, compressing the inputs in-place.  This
    /// shuffles the order deterministically - which is suitable for Region and
    /// Phi, but not for every Node.
    fn del_def(self, index: usize, sea: &mut Nodes) {
        self.unlock(sea);
        let old_def = self.inputs(sea)[index];
        sea.inputs[self].swap_remove(index);
        if let Some(old_def) = old_def {
            old_def.del_use(self, sea);
            if old_def.is_unused(sea) {
                old_def.kill(sea);
            }
            old_def.move_deps_to_worklist(sea);
        }
    }

    pub fn add_use(self, use_: Node, sea: &mut Nodes) {
        sea.outputs[self].push(use_)
    }

    pub fn del_use(self, use_: Node, sea: &mut Nodes) {
        if let Some(pos) = sea.outputs[self].iter().position(|n| *n == use_) {
            sea.outputs[self].swap_remove(pos);
        }
    }

    pub fn is_unused(self, sea: &Nodes) -> bool {
        sea.outputs[self].is_empty()
    }

    pub fn swap_12(self, sea: &mut Nodes) -> Node {
        self.unlock(sea);
        sea.inputs[self].swap(1, 2);
        self
    }
    pub fn keep(self, sea: &mut Nodes) -> Node {
        self.add_use(Node::DUMMY, sea);
        self
    }

    pub fn unkeep(self, sea: &mut Nodes) -> Node {
        self.del_use(Node::DUMMY, sea);
        self
    }

    pub fn is_keep(self, sea: &Nodes) -> bool {
        sea.outputs[self].contains(&Node::DUMMY)
    }

    pub fn unkill(self, sea: &mut Nodes) {
        self.unkeep(sea);
        if self.is_unused(sea) {
            self.kill(sea);
        }
    }

    pub fn err(self, sea: &Nodes) -> Option<String> {
        let binary_ints = |op: &str| {
            for i in [1, 2] {
                if let Some(in_ty) = self.inputs(sea)[i].and_then(|i| i.ty(sea)) {
                    if !in_ty.is_int() {
                        return Some(format!("Cannot '{op}' {in_ty}"));
                    }
                } else {
                    return Some(format!("Cannot '{op}' null"));
                }
            }
            None
        };
        let memop = |ptr: Option<Ty>, name: &str| {
            // Already an error, but better error messages come from elsewhere
            if ptr == Some(sea.tys.bot) {
                return None;
            }
            // Better be a not-nil TMP
            if ptr
                .and_then(|p| p.to_mem_ptr())
                .is_some_and(|tmp| !tmp.data().nil)
            {
                return None;
            }
            Some(format!("Might be null accessing '{name}'"))
        };
        match self.downcast(&sea.ops) {
            TypedNode::And(_) => binary_ints("&"),
            TypedNode::Or(_) => binary_ints("|"),
            TypedNode::Xor(_) => binary_ints("^"),
            TypedNode::Sar(_) => binary_ints(">>"),
            TypedNode::Shl(_) => binary_ints("<<"),
            TypedNode::Load(n) => memop(n.ptr(sea).unwrap().ty(sea), sea[n].name),
            TypedNode::Store(n) => {
                let ptr = n.ptr(sea).unwrap().ty(sea);
                let name = sea[n].name;
                memop(ptr, name).or_else(|| {
                    let tmp = ptr.unwrap().as_mem_ptr();
                    if tmp.data().to.field(name).unwrap().final_field {
                        Some(format!("Cannot modify final field '{name}'"))
                    } else {
                        let t = n.val(sea).unwrap().ty(sea).unwrap();
                        if sea[n].init || t.isa(sea[n].declared_ty, sea.tys) {
                            None
                        } else {
                            Some(format!(
                                "Cannot store {t} into field {} {name}",
                                sea[n].declared_ty
                            ))
                        }
                    }
                })
            }
            _ => None,
        }
    }

    pub fn unique_input(self, sea: &Nodes) -> Option<Node> {
        if self.inputs(sea).len() == 1 {
            self.inputs(sea)[0]
        } else {
            None // ambiguous
        }
    }

    /// Does this node contain all constants?
    /// Ignores in(0), as is usually control.
    /// In an input is not a constant, we add dep as
    /// a dependency to it, because dep can make progress
    /// if the input becomes a constant later.
    /// It is sufficient for one of the non-const
    /// inputs to have the dependency so we don't bother
    /// checking the rest.
    pub fn all_cons(self, dep: Node, sea: &mut Nodes) -> bool {
        if let Some(phi) = self.to_phi(sea) {
            let region = phi.inputs(sea)[0];
            if !sea.instanceof_region(region) {
                return false;
            }
            // When the region completes (is no longer in progress) the Phi can
            // become a "all constants" Phi, and the "dep" might make progress.
            self.add_dep(dep, sea);
            if Nodes::in_progress(&sea.ops, &sea.inputs, region.unwrap()) {
                return false;
            }
        }
        if let Some(non_const) = self
            .inputs(sea)
            .iter()
            .skip(1)
            .find(|n| !n.unwrap().ty(sea).unwrap().is_constant(sea.tys))
        {
            non_const.unwrap().add_dep(dep, sea); // If in(i) becomes a constant later, will trigger some peephole
            false
        } else {
            true
        }
    }

    fn same_op(self, sea: &Nodes) -> bool {
        for i in 2..self.inputs(sea).len() {
            if sea[self.inputs(sea)[1].unwrap()].operation()
                != sea[self.inputs(sea)[i].unwrap()].operation()
            {
                return false;
            }
        }
        true
    }

    /// Find a projection by index
    pub fn proj(self, index: usize, sea: &Nodes) -> Option<Proj> {
        sea.outputs[self]
            .iter()
            .filter(|&&o| o != Node::DUMMY)
            .filter_map(|o| o.to_proj(sea))
            .find(|&p| sea[p].index == index)
    }

    pub fn cproj(self, index: usize, sea: &mut Nodes) -> Option<CProj> {
        sea.outputs[self]
            .iter()
            .filter(|&&o| o != Node::DUMMY)
            .filter_map(|o| o.to_cproj(sea))
            .find(|&p| sea[p].index == index)
    }
    /// Semantic change to the graph (so NOT a peephole), used by the Parser.
    /// If any input is a float, flip to a float-flavored opcode and widen any
    /// non-float input.
    pub fn widen(self, sea: &mut Nodes) -> Node {
        if !self.has_float_input(sea) {
            return self;
        }
        if let Some(flt) = self.copy_f(sea) {
            for i in 1..self.inputs(sea).len() {
                let in_i = self.inputs(sea)[i].unwrap();
                flt.set_def(
                    i,
                    if in_i.ty(sea).is_some_and(|t| t.is_float()) {
                        in_i
                    } else {
                        ToFloat::new(in_i, sea).peephole(sea)
                    },
                    sea,
                );
            }
            self.kill(sea);
            flt
        } else {
            self
        }
    }

    fn has_float_input(self, sea: &Nodes) -> bool {
        self.inputs(sea)
            .iter()
            .skip(1)
            .flatten()
            .any(|i| i.ty(sea).is_some_and(|t| t.is_float()))
    }

    fn copy_f(self, sea: &mut Nodes) -> Option<Node> {
        let op = match &sea[self] {
            Op::Add => Op::AddF,
            Op::Bool(BoolOp::EQ) => Op::Bool(BoolOp::EQF),
            Op::Bool(BoolOp::LT) => Op::Bool(BoolOp::LTF),
            Op::Bool(BoolOp::LE) => Op::Bool(BoolOp::LEF),
            Op::Div => Op::DivF,
            Op::Minus => return Some(sea.create((Op::MinusF, vec![None, None]))),
            Op::Mul => Op::MulF,
            Op::Sub => Op::SubF,
            _ => return None,
        };
        Some(sea.create((op, vec![None, None, None])))
    }
}

impl Phi {
    fn single_unique_input(self, sea: &mut Nodes) -> Option<Node> {
        let region = self.inputs(sea)[0].unwrap();
        if region.is_loop(sea) && region.inputs(sea)[1].unwrap().ty(sea) == Some(sea.types.xctrl) {
            return None; // Dead entry loops just ignore and let the loop collapse
        }

        let mut live = None;
        for i in 1..self.inputs(sea).len() {
            // If the region's control input is live, add this as a dependency
            // to the control because we can be peeped should it become dead.
            let region_in_i = region.inputs(sea)[i].unwrap().add_dep(self, sea);
            if region_in_i.ty(sea) != Some(sea.types.xctrl) && self.inputs(sea)[i] != Some(*self) {
                if live.is_none() || live == self.inputs(sea)[i] {
                    live = self.inputs(sea)[i];
                } else {
                    return None;
                }
            }
        }
        live
    }
}

impl<'t> Nodes<'t> {
    fn in_progress(ops: &OpVec<'t>, inputs: &IdVec<Node, Vec<Option<Node>>>, region: Node) -> bool {
        debug_assert!(matches!(
            ops[region],
            Op::Region { .. } | Op::Loop | Op::Phi(_)
        ));
        (matches!(&ops[region], Op::Phi(_)) || !inputs[region].is_empty())
            && inputs[region].last().unwrap().is_none()
    }

    /// Utility to walk the entire graph applying a function; return the first
    /// not-null result.
    pub(crate) fn walk_non_reentrant<T, F: FnMut(&mut Self, Node) -> Option<T>>(
        &mut self,
        node: Node,
        mut f: F,
    ) -> Option<T> {
        assert!(self.walk_visited.is_empty());
        let result = self.walk_non_reentrant_inner(node, &mut f);
        self.walk_visited.clear();
        result
    }

    fn walk_non_reentrant_inner<T, F: FnMut(&mut Self, Node) -> Option<T>>(
        &mut self,
        node: Node,
        f: &mut F,
    ) -> Option<T> {
        if self.walk_visited.get(node) {
            return None; // Been there, done that
        }
        self.walk_visited.add(node);
        if let result @ Some(_) = f(self, node) {
            return result;
        };
        for i in 0..self.inputs[node].len() {
            if let Some(node) = self.inputs[node][i] {
                if let result @ Some(_) = self.walk_non_reentrant_inner(node, f) {
                    return result;
                };
            }
        }
        for i in 0..self.outputs[node].len() {
            let node = self.outputs[node][i];
            if node != Node::DUMMY {
                if let result @ Some(_) = self.walk_non_reentrant_inner(node, f) {
                    return result;
                };
            }
        }
        None
    }

    pub fn instanceof_region(&self, node: Option<Node>) -> bool {
        node.is_some_and(|n| n.is_region(self))
    }
}
