use std::num::NonZeroU32;
use std::ops::{Index, IndexMut};

pub use id::NodeId;
pub use node::{BoolOp, Node};
pub use scope::ScopeNode;

use crate::datastructures::id::Id;
use crate::datastructures::id_vec::IdVec;
use crate::sea_of_nodes::types::{Ty, Types};

mod id;
mod idealize;
mod node;
mod peephole;
mod print;
mod scope;

/// Using `IdVec` has two advantages over `Vec`+helper methods:
/// 1) `self.inputs[x]` and `self.outputs[x]` can be borrowed simultaneously
///    while `self.inputs(x)` and `self.outputs_mut(x)` can't
/// 2) methods like `self.inputs(x)` and `self.inputs_mut(x)` require two versions for mutability
///    while `self.inputs[x]` automatically decides
pub struct Nodes<'t> {
    /// indexed by self[id]
    nodes: Vec<Node<'t>>,

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

    /// If this is true peephole only computes the type.
    pub disable_peephole: bool,

    /// the start node to be used for creating constants.
    pub start: NodeId,
}

pub type NodeCreation<'t> = (Node<'t>, Vec<Option<NodeId>>);

impl<'t> Nodes<'t> {
    pub fn new() -> Self {
        let dummy = Node::Stop;
        Nodes {
            nodes: vec![dummy],
            inputs: IdVec::new(vec![vec![]]),
            outputs: IdVec::new(vec![vec![]]),
            ty: IdVec::new(vec![None]),
            disable_peephole: false,
            start: NodeId::DUMMY,
        }
    }
    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    pub fn create(&mut self, (node, inputs): NodeCreation<'t>) -> NodeId {
        let id = u32::try_from(self.nodes.len())
            .and_then(NonZeroU32::try_from)
            .map(NodeId)
            .unwrap();
        self.nodes.push(node);
        self.inputs.push(inputs);
        self.outputs.push(vec![]);
        self.ty.push(None);
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

    pub fn create_peepholed(&mut self, types: &mut Types<'t>, c: NodeCreation<'t>) -> NodeId {
        let id = self.create(c);
        let better = self.peephole(id, types);
        debug_assert_eq!(better, self.peephole(better, types));
        if better != id {
            // TODO: we could re-use the dead slots: self.nodes.trim_end()
        }
        better
    }

    pub fn is_dead(&self, node: NodeId) -> bool {
        self.is_unused(node) && self.inputs[node].is_empty() && self.ty[node].is_none()
    }

    pub fn pop_n(&mut self, node: NodeId, n: usize) {
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
        debug_assert!(self.is_unused(node));
        self.pop_n(node, self.inputs[node].len());
        self.inputs[node] = vec![]; // deallocate
        self.ty[node] = None; // flag as dead
        debug_assert!(self.is_dead(node));
    }

    pub fn set_def(&mut self, this: NodeId, index: usize, new_def: Option<NodeId>) {
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
    }

    pub fn add_def(&mut self, node: NodeId, new_def: Option<NodeId>) {
        self.inputs[node].push(new_def);
        if let Some(new_def) = new_def {
            self.add_use(new_def, node);
        }
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
        self.inputs[node].swap(1, 2);
        node
    }
    pub fn keep(&mut self, node: NodeId) {
        self.add_use(node, NodeId::DUMMY);
    }

    pub fn unkeep(&mut self, node: NodeId) {
        self.del_use(node, NodeId::DUMMY);
    }

    pub fn is_cfg(&self, node: NodeId) -> bool {
        match &self[node] {
            Node::Start { .. } | Node::Return | Node::Stop => true,
            Node::If | Node::Region => true,
            Node::Proj(p) => {
                p.index == 0 || self.inputs[node][0].is_some_and(|n| matches!(&self[n], Node::If))
            }
            Node::Constant(_)
            | Node::Add
            | Node::Sub
            | Node::Mul
            | Node::Div
            | Node::Minus
            | Node::Scope(_)
            | Node::Bool(_)
            | Node::Phi(_)
            | Node::Not => false,
        }
    }

    pub fn unique_input(&self, stop: NodeId) -> Option<NodeId> {
        if self.inputs[stop].len() == 1 {
            self.inputs[stop][0]
        } else {
            None // ambiguous
        }
    }

    /// ignores input 0
    pub fn all_cons(&self, node: NodeId) -> bool {
        self.inputs[node]
            .iter()
            .skip(1)
            .all(|n| self.ty[n.unwrap()].unwrap().is_constant())
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
    fn same_inputs(&self, node: NodeId) -> bool {
        for i in 2..self.inputs[node].len() {
            if self.inputs[node][1] != self.inputs[node][i] {
                return false;
            }
        }
        true
    }
}

impl<'t> Index<NodeId> for Nodes<'t> {
    type Output = Node<'t>;

    fn index(&self, index: NodeId) -> &Self::Output {
        &self.nodes[index.index()]
    }
}

impl<'t> IndexMut<NodeId> for Nodes<'t> {
    fn index_mut(&mut self, index: NodeId) -> &mut Self::Output {
        &mut self.nodes[index.index()]
    }
}
