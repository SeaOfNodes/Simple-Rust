use std::collections::HashMap;
use std::num::NonZeroU32;

pub use id::NodeId;
pub use node::{BoolOp, Op, ProjOp, StartOp};
pub use scope::ScopeOp;

use crate::datastructures::id_set::IdSet;
use crate::datastructures::id_vec::IdVec;
use crate::sea_of_nodes::nodes::gvn::GvnEntry;
use crate::sea_of_nodes::nodes::index::{ScopeId, StartId};
use crate::sea_of_nodes::parser::Parser;
use crate::sea_of_nodes::types::{MemPtr, Struct, Ty, Type, Types};
use iter_peeps::IterPeeps;

mod gvn;
mod id;
mod idealize;
pub mod index;
mod iter_peeps;
mod node;
mod peephole;
mod print;
mod scope;

pub struct NodeVec<'t>(IdVec<NodeId, Op<'t>>);

/// Using `IdVec` has two advantages over `Vec`+helper methods:
/// 1) `self.inputs[x]` and `self.outputs[x]` can be borrowed simultaneously
///    while `self.inputs(x)` and `self.outputs_mut(x)` can't
/// 2) methods like `self.inputs(x)` and `self.inputs_mut(x)` require two versions for mutability
///    while `self.inputs[x]` automatically decides
pub struct Nodes<'t> {
    /// indexed by self[id]
    nodes: NodeVec<'t>,

    pub ty: IdVec<NodeId, Option<Ty<'t>>>,

    /// Inputs to the node. These are use-def references to Nodes.
    ///
    /// Generally fixed length, ordered, nulls allowed, no unused
    /// trailing space. Ordering is required because e.g. "a/ b"
    /// is different from "b/ a". The first input (offset 0) is
    /// often a isCFG node.
    pub inputs: IdVec<NodeId, Vec<Option<NodeId>>>,

    /// Outputs reference Nodes that are not null and have this Node
    /// as an input. These nodes are users of this node, thus these
    /// are def-use references to Nodes.
    ///
    /// Outputs directly match inputs, making a directed graph that
    /// can be walked in either direction. These outputs are typically
    /// used for efficient optimizations but otherwise have no semantics
    /// meaning
    pub outputs: IdVec<NodeId, Vec<NodeId>>,

    /// Some of the peephole rules get complex, and search further afield than
    /// just the nearest neighbor.  These peepholes can fail the pattern match
    /// on a node some distance away, and if that node ever changes we should
    /// retry the peephole.  Track a set of Nodes dependent on `this`, and
    /// revisit them if `this` changes.
    pub deps: IdVec<NodeId, Vec<NodeId>>,

    /// Immediate dominator tree depth, used to approximate a real IDOM during
    ///  parsing where we do not have the whole program, and also peepholes
    ///  change the CFG incrementally.
    idepth: IdVec<NodeId, u32>,

    /// If this is true peephole only computes the type.
    pub disable_peephole: bool,

    /// the start node to be used for creating constants.
    pub start: StartId,

    /// Creating nodes such as constants and computing peepholes requires
    /// interning new types and operations such as meet and join.
    pub types: &'t Types<'t>,

    /// Worklist for iterative peepholes
    iter_peeps: IterPeeps,

    pub iter_cnt: usize,
    pub iter_nop_cnt: usize,

    walk_visited: IdSet<NodeId>,

    /// Global Value Numbering. Hash over opcode and inputs; hits in this table
    /// are structurally equal.
    gvn: HashMap<GvnEntry, ()>,

    /// Cached hash.  If zero, then not computed AND this Node is NOT in the GVN
    /// table - and can have its edges hacked (which will change his hash
    /// anyway).  If Non-Zero then this Node is IN the GVN table, or is being
    /// probed to see if it can be inserted.  His edges are "locked", because
    /// hacking his edges will change his hash.
    hash: IdVec<NodeId, Option<NonZeroU32>>,
}

pub type NodeCreation<'t> = (Op<'t>, Vec<Option<NodeId>>);

impl<'t> Nodes<'t> {
    pub fn new(types: &'t Types<'t>) -> Self {
        let dummy = Op::Stop;
        Nodes {
            nodes: NodeVec(IdVec::new(vec![dummy])),
            inputs: IdVec::new(vec![vec![]]),
            outputs: IdVec::new(vec![vec![]]),
            ty: IdVec::new(vec![None]),
            deps: IdVec::new(vec![vec![]]),
            idepth: IdVec::new(vec![0]),
            disable_peephole: false,
            start: StartId::DUMMY,
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
        self.nodes.0.len()
    }

    pub fn create(&mut self, (node, inputs): NodeCreation<'t>) -> NodeId {
        let id = u32::try_from(self.len())
            .and_then(NonZeroU32::try_from)
            .map(NodeId)
            .unwrap();
        self.nodes.0.push(node);
        self.inputs.push(inputs);
        self.outputs.push(vec![]);
        self.ty.push(None);
        self.deps.push(vec![]);
        self.idepth.push(0);
        self.hash.push(None);
        for i in 0..self.inputs[id].len() {
            if let Some(input) = self.inputs[id][i] {
                self.add_use(input, id);
            }
        }

        debug_assert_eq!(self.len(), self.inputs.len());
        debug_assert_eq!(self.len(), self.outputs.len());
        debug_assert_eq!(self.len(), self.ty.len());
        id
    }

    pub fn create_peepholed(&mut self, c: NodeCreation<'t>) -> NodeId {
        self.create(c).peephole(self)
    }

    pub fn is_dead(&self, node: NodeId) -> bool {
        self.is_unused(node) && self.inputs[node].is_empty() && self.ty[node].is_none()
    }

    pub fn pop_n(&mut self, node: NodeId, n: usize) {
        self.unlock(node);
        for _ in 0..n {
            let old_def = self.inputs[node].pop().unwrap();
            if let Some(old_def) = old_def {
                self.del_use(old_def, node);
                if self.is_unused(old_def) {
                    self.kill(old_def);
                }
            }
        }
    }

    pub fn kill(&mut self, node: NodeId) {
        self.unlock(node);
        debug_assert!(self.is_unused(node));
        for _ in 0..self.inputs[node].len() {
            // Set all inputs to null, recursively killing unused Nodes
            let old_def = self.inputs[node].pop().unwrap();
            if let Some(old_def) = old_def {
                self.iter_peeps.add(old_def); // Revisit neighbor because removed use
                self.del_use(old_def, node);
                if self.is_unused(old_def) {
                    self.kill(old_def); // If we removed the last use, the old def is now dead
                }
            }
        }
        self.inputs[node] = vec![]; // deallocate
        self.ty[node] = None; // flag as dead
        debug_assert!(self.is_dead(node));
    }

    /// Replace 'this' with nnn in the graph, making 'this' go dead
    pub fn subsume(&mut self, this: NodeId, that: NodeId) {
        assert_ne!(this, that);
        while let Some(n) = self.outputs[this].pop() {
            self.unlock(n);
            let idx = self.inputs[n]
                .iter()
                .position(|&x| x == Some(this))
                .unwrap();
            self.inputs[n][idx] = Some(that);
            self.add_use(that, n);
        }
        self.kill(this);
    }

    pub fn set_def(&mut self, this: NodeId, index: usize, new_def: Option<NodeId>) {
        self.unlock(this);

        let old_def = self.inputs[this][index];
        if old_def == new_def {
            return;
        }

        if let Some(new_def) = new_def {
            self.add_use(new_def, this);
        }

        if let Some(old_def) = old_def {
            self.del_use(old_def, this);
            if self.is_unused(old_def) {
                self.kill(old_def);
            }
        }

        self.inputs[this][index] = new_def;
        self.move_deps_to_worklist(this);
    }

    pub fn add_def(&mut self, node: NodeId, new_def: Option<NodeId>) {
        self.unlock(node);
        self.inputs[node].push(new_def);
        if let Some(new_def) = new_def {
            self.add_use(new_def, node);
        }
    }

    /// Remove the numbered input, compressing the inputs in-place.  This
    /// shuffles the order deterministically - which is suitable for Region and
    /// Phi, but not for every Node.
    fn del_def(&mut self, node: NodeId, index: usize) {
        self.unlock(node);
        let old_def = self.inputs[node][index];
        if let Some(old_def) = old_def {
            self.del_use(old_def, node);
            if self.is_unused(old_def) {
                self.kill(old_def);
            }
            self.move_deps_to_worklist(old_def);
        }
        self.inputs[node].swap_remove(index);
    }

    pub fn add_use(&mut self, node: NodeId, use_: NodeId) {
        self.outputs[node].push(use_)
    }

    pub fn del_use(&mut self, node: NodeId, use_: NodeId) {
        if let Some(pos) = self.outputs[node].iter().rposition(|n| *n == use_) {
            self.outputs[node].swap_remove(pos);
        }
    }

    pub fn is_unused(&self, node: NodeId) -> bool {
        self.outputs[node].is_empty()
    }

    pub fn swap_12(&mut self, node: NodeId) -> NodeId {
        self.unlock(node);
        self.inputs[node].swap(1, 2);
        node
    }
    pub fn keep(&mut self, node: NodeId) {
        self.add_use(node, NodeId::DUMMY);
    }

    pub fn unkeep(&mut self, node: NodeId) {
        self.del_use(node, NodeId::DUMMY);
    }

    pub fn is_keep(&self, node: NodeId) -> bool {
        self.outputs[node].contains(&NodeId::DUMMY)
    }

    pub fn err(&self, node: NodeId) -> Option<String> {
        if let Some(memop) = self.to_mem_op(node) {
            let ptr = node.inputs(self)[2]?.ty(self)?;
            if ptr == self.types.ty_bot || matches!(*ptr, Type::Pointer(MemPtr { nil: true, .. })) {
                return Some(format!("Might be null accessing '{}'", self[memop].name));
            }
        }
        None
    }

    pub fn is_cfg(&self, node: NodeId) -> bool {
        match &self[node] {
            Op::Start { .. } | Op::Return | Op::Stop => true,
            Op::If | Op::Region { .. } | Op::Loop => true,
            Op::Proj(p) => {
                p.index == 0 || self.inputs[node][0].is_some_and(|n| matches!(&self[n], Op::If))
            }
            Op::Constant(_)
            | Op::Add
            | Op::Sub
            | Op::Mul
            | Op::Div
            | Op::Minus
            | Op::Scope(_)
            | Op::Bool(_)
            | Op::Phi(_)
            | Op::Cast(_)
            | Op::MemOp(_)
            | Op::New(_)
            | Op::Not => false,
        }
    }

    pub fn unique_input(&self, stop: NodeId) -> Option<NodeId> {
        if self.inputs[stop].len() == 1 {
            self.inputs[stop][0]
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
    pub fn all_cons(&mut self, node: NodeId, dep: NodeId) -> bool {
        if matches!(&self[node], Op::Phi(_)) {
            let region = self.inputs[node][0];
            if !self.instanceof_region(region) {
                return false;
            }
            // When the region completes (is no longer in progress) the Phi can
            // become a "all constants" Phi, and the "dep" might make progress.
            self.add_dep(node, dep);
            if Self::in_progress(&self.nodes, &self.inputs, region.unwrap()) {
                return false;
            }
        }
        if let Some(non_const) = self.inputs[node]
            .iter()
            .skip(1)
            .find(|n| !self.ty[n.unwrap()].unwrap().is_constant())
        {
            self.add_dep(non_const.unwrap(), dep); // If in(i) becomes a constant later, will trigger some peephole
            false
        } else {
            true
        }
    }

    fn same_op(&self, node: NodeId) -> bool {
        for i in 2..self.inputs[node].len() {
            if self[self.inputs[node][1].unwrap()].operation()
                != self[self.inputs[node][i].unwrap()].operation()
            {
                return false;
            }
        }
        true
    }
    fn single_unique_input(&mut self, phi: NodeId) -> Option<NodeId> {
        let region = self.inputs[phi][0].unwrap();
        if matches!(&self[region], Op::Loop)
            && self.ty[self.inputs[region][1].unwrap()] == Some(self.types.ty_xctrl)
        {
            return None; // Dead entry loops just ignore and let the loop collapse
        }

        let mut live = None;
        for i in 1..self.inputs[phi].len() {
            // If the region's control input is live, add this as a dependency
            // to the control because we can be peeped should it become dead.
            let region_in_i = self.inputs[region][i].unwrap();
            self.add_dep(region_in_i, phi);
            if self.ty[region_in_i] != Some(self.types.ty_xctrl) && self.inputs[phi][i] != Some(phi)
            {
                if live.is_none() || live == self.inputs[phi][i] {
                    live = self.inputs[phi][i];
                } else {
                    return None;
                }
            }
        }
        live
    }

    /// Return the immediate dominator of this Node and compute dom tree depth.
    fn idom(&mut self, node: NodeId) -> Option<NodeId> {
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

    fn idepth(&mut self, node: NodeId) -> u32 {
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
            Op::Loop => 1,
            _ => 0,
        };
        self.idepth[node] = self.idepth(self.inputs[node][index].unwrap()) + 1;
        self.idepth[node]
    }

    fn in_progress(
        nodes: &NodeVec<'t>,
        inputs: &IdVec<NodeId, Vec<Option<NodeId>>>,
        region: NodeId,
    ) -> bool {
        debug_assert!(matches!(
            nodes[region],
            Op::Region { .. } | Op::Loop | Op::Phi(_)
        ));
        (matches!(&nodes[region], Op::Phi(_)) || !inputs[region].is_empty())
            && inputs[region].last().unwrap().is_none()
    }

    /// Utility to walk the entire graph applying a function; return the first
    /// not-null result.
    fn walk_non_reentrant<T, F: FnMut(&mut Self, NodeId) -> Option<T>>(
        &mut self,
        node: NodeId,
        mut f: F,
    ) -> Option<T> {
        assert!(self.walk_visited.is_empty());
        let result = self.walk_non_reentrant_inner(node, &mut f);
        self.walk_visited.clear();
        result
    }

    fn walk_non_reentrant_inner<T, F: FnMut(&mut Self, NodeId) -> Option<T>>(
        &mut self,
        node: NodeId,
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
            if let result @ Some(_) = self.walk_non_reentrant_inner(node, f) {
                return result;
            };
        }
        None
    }

    pub fn instanceof_region(&self, node: Option<NodeId>) -> bool {
        node.is_some_and(|n| matches!(&self[n], Op::Region { .. } | Op::Loop))
    }

    /// Creates a projection for each of the struct's fields, using the field alias
    /// as the key.
    pub fn add_mem_proj(&mut self, start: StartId, ts: Ty<'t>, scope: ScopeId) {
        let Type::Struct(Struct::Struct { name, fields }) = *ts else {
            unreachable!()
        };
        let Type::Tuple { types } = *self[start].args else {
            unreachable!()
        };

        let len = types.len();
        self[start].alias_starts.insert(name, len as u32);

        // resize the tuple's type array to include all fields of the struct
        let args = types
            .iter()
            .copied()
            .chain(fields.iter().enumerate().map(|(i, _)| {
                // The new members of the tuple get a mem type with an alias
                self.types.get_mem((i + types.len()) as u32)
            }))
            .collect::<Vec<Ty>>();

        let args_ty = self.types.get_tuple_from_slice(&args);
        self.nodes[start].args = args_ty;
        self.ty[start] = Some(args_ty);

        // For each of the fields we now add a mem projection.  Note that the
        // alias matches the slot of the field in the tuple
        for (alias, &alias_ty) in args[len..].iter().enumerate() {
            let name = self.types.get_str(&Parser::mem_name(alias as u32));
            let n = self.create_peepholed(Op::make_proj(start, alias, name));
            scope.define(name, alias_ty, n, self).unwrap()
        }
    }
}
