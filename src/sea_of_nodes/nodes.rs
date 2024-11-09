use std::collections::HashMap;
use std::num::NonZeroU32;

pub use id::Node;
pub use index::Op;
pub use node::{BoolOp, LoadOp, ProjOp, StartOp, StoreOp};
pub use scope::ScopeOp;

use crate::datastructures::id_set::IdSet;
use crate::datastructures::id_vec::IdVec;
use crate::sea_of_nodes::nodes::gvn::GvnEntry;
use crate::sea_of_nodes::nodes::index::{Constant, Phi, Proj, Scope, Start, XCtrl};
use crate::sea_of_nodes::parser::Parser;
use crate::sea_of_nodes::types::{MemPtr, Struct, Ty, Type, Types};
use iter_peeps::IterPeeps;

mod cfg;
mod gvn;
mod id;
mod idealize;
pub mod index;
mod iter_peeps;
mod node;
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

    /// Immediate dominator tree depth, used to approximate a real IDOM during
    ///  parsing where we do not have the whole program, and also peepholes
    ///  change the CFG incrementally.
    idepth: IdVec<Node, u32>,

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
            idepth: IdVec::new(vec![0]),
            disable_peephole: false,
            // TODO get rid of DUMMYs
            start: Start::DUMMY,
            zero: Constant::DUMMY,
            xctrl: XCtrl::DUMMY,
            types,
            iter_peeps: IterPeeps::new(),
            iter_cnt: 0,
            iter_nop_cnt: 0,
            walk_visited: IdSet::zeros(0),
            gvn: HashMap::new(),
            hash: IdVec::new(vec![None]),
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
        self.idepth.push(0);
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

    pub fn pop_n(self, n: usize, sea: &mut Nodes) {
        self.unlock(sea);
        for _ in 0..n {
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
        self.unlock(sea);
        debug_assert!(self.is_unused(sea));
        for _ in 0..self.inputs(sea).len() {
            // Set all inputs to null, recursively killing unused Nodes
            let old_def = sea.inputs[self].pop().unwrap();
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
    pub fn subsume(self, that: Node, sea: &mut Nodes) {
        assert_ne!(self, that);
        while let Some(n) = sea.outputs[self].pop() {
            n.unlock(sea);
            let idx = n.inputs(sea).iter().position(|&x| x == Some(self)).unwrap();
            sea.inputs[n][idx] = Some(that);
            that.add_use(n, sea);
        }
        self.kill(sea);
    }

    pub fn set_def(self, index: usize, new_def: Option<Node>, sea: &mut Nodes) {
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

    pub fn add_def(self, new_def: Option<Node>, sea: &mut Nodes) {
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
        if let Some(old_def) = old_def {
            old_def.del_use(self, sea);
            if old_def.is_unused(sea) {
                old_def.kill(sea);
            }
            old_def.move_deps_to_worklist(sea);
        }
        sea.inputs[self].swap_remove(index);
    }

    pub fn add_use(self, use_: Node, sea: &mut Nodes) {
        sea.outputs[self].push(use_)
    }

    pub fn del_use(self, use_: Node, sea: &mut Nodes) {
        if let Some(pos) = sea.outputs[self].iter().rposition(|n| *n == use_) {
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

    pub fn unkeep(self, sea: &mut Nodes) {
        self.del_use(Node::DUMMY, sea);
    }

    pub fn is_keep(self, sea: &Nodes) -> bool {
        sea.outputs[self].contains(&Node::DUMMY)
    }

    pub fn err(self, sea: &Nodes) -> Option<String> {
        if let Some(memop) = self.to_mem_name(sea) {
            let ptr = self.inputs(sea)[2]?.ty(sea)?;
            if ptr == sea.types.ty_bot || matches!(*ptr, Type::Pointer(MemPtr { nil: true, .. })) {
                return Some(format!("Might be null accessing '{memop}'"));
            }
        }
        None
    }

    pub fn is_cfg(self, sea: &Nodes) -> bool {
        self.to_cfg(&sea.ops).is_some()
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
            .find(|n| !n.unwrap().ty(sea).unwrap().is_constant())
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
}

impl Phi {
    fn single_unique_input(self, sea: &mut Nodes) -> Option<Node> {
        let region = self.inputs(sea)[0].unwrap();
        if region.to_loop(sea).is_some()
            && region.inputs(sea)[1].unwrap().ty(sea) == Some(sea.types.ty_xctrl)
        {
            return None; // Dead entry loops just ignore and let the loop collapse
        }

        let mut live = None;
        for i in 1..self.inputs(sea).len() {
            // If the region's control input is live, add this as a dependency
            // to the control because we can be peeped should it become dead.
            let region_in_i = region.inputs(sea)[i].unwrap();
            region_in_i.add_dep(*self, sea);
            if region_in_i.ty(sea) != Some(sea.types.ty_xctrl) && self.inputs(sea)[i] != Some(*self)
            {
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
    /// Return the immediate dominator of this Node and compute dom tree depth.
    fn idom(&mut self, node: Node) -> Option<Node> {
        match &self[node] {
            Op::Start { .. } | Op::Stop => None,
            Op::Loop => self.inputs[node][1],
            Op::Region => {
                if let &[_, i1] = self.inputs[node].as_slice() {
                    i1 // 1-input is that one input
                } else if let &[_, lhs, rhs] = self.inputs[node].as_slice() {
                    // Walk the LHS & RHS idom trees in parallel until they match, or either fails
                    // Because this does not cache, it can be linear in the size of the program.
                    let mut lhs = lhs?;
                    let mut rhs = rhs?;
                    while lhs != rhs {
                        let comp = self.idepth(lhs) as i32 - self.idepth(rhs) as i32;
                        if comp >= 0 {
                            lhs = self.idom(lhs)?
                        }
                        if comp <= 0 {
                            rhs = self.idom(rhs)?
                        }
                    }
                    Some(lhs)
                } else {
                    // Fails for anything other than 2-inputs
                    None
                }
            }
            _ => Some(self.inputs[node][0].expect("don't ask")),
        }
    }

    fn idepth(&mut self, node: Node) -> u32 {
        if self.idepth[node] != 0 {
            return self.idepth[node];
        }
        let index = match self[node] {
            Op::Start(_) => return 0, // uncached
            Op::Region => {
                let mut d = 0;
                for n in 0..self.inputs[node].len() {
                    if let Some(n) = self.inputs[node][n] {
                        d = d.max(self.idepth(n));
                    }
                }
                self.idepth[node] = d;
                return d;
            }
            Op::Loop => 1, // Bypass Region idom, same as the default idom() using use entry in(1) instead of in(0)
            _ => 0,
        };
        self.idepth[node] = self.idepth(self.inputs[node][index].unwrap()) + 1;
        self.idepth[node]
    }

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
        node.is_some_and(|n| matches!(&self[n], Op::Region { .. } | Op::Loop))
    }
}

impl Start {
    /// Creates a projection for each of the struct's fields, using the field alias
    /// as the key.
    pub fn add_mem_proj<'t>(self, ts: Ty<'t>, scope: Scope, sea: &mut Nodes<'t>) {
        let Type::Struct(Struct::Struct { name, fields }) = *ts else {
            unreachable!()
        };
        let Type::Tuple { types } = *sea[self].args else {
            unreachable!()
        };

        let len = types.len();
        sea[self].alias_starts.insert(name, len as u32);

        // resize the tuple's type array to include all fields of the struct
        let args = types
            .iter()
            .copied()
            .chain(fields.iter().enumerate().map(|(i, _)| {
                // The new members of the tuple get a mem type with an alias
                sea.types.get_mem((i + types.len()) as u32)
            }))
            .collect::<Vec<Ty>>();

        let args_ty = sea.types.get_tuple_from_slice(&args);
        sea.ops[self].args = args_ty;
        sea.ty[self] = Some(args_ty);

        // For each of the fields we now add a mem projection.  Note that the
        // alias matches the slot of the field in the tuple
        for (alias, &alias_ty) in args.iter().enumerate().skip(len) {
            let name = sea.types.get_str(&Parser::mem_name(alias as u32));
            let n = Proj::new(self, alias, name, sea).peephole(sea);
            scope.define(name, alias_ty, n, sea).unwrap()
        }
    }
}
