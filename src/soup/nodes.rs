use std::num::NonZeroU32;
use std::ops::{Index, IndexMut};

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct NodeId(NonZeroU32);

impl NodeId {
    pub const DUMMY: NodeId = NodeId(NonZeroU32::MAX);

    fn index(self) -> usize { self.0.get() as usize }
}

pub struct Nodes {
    nodes: Vec<Node>,
}

impl Nodes {
    pub fn new() -> Self {
        let dummy = Node::ConstantNode(ConstantNode {
            base: NodeBase {
                id: NodeId::DUMMY,
                inputs: vec![],
                outputs: vec![],
            },
            value: 0,
        });
        Nodes {
            nodes: vec![dummy]
        }
    }
    pub fn create<F: FnOnce(NodeId) -> Node>(&mut self, f: F) -> NodeId {
        let id = u32::try_from(self.nodes.len())
            .and_then(NonZeroU32::try_from)
            .map(NodeId).unwrap();
        self.nodes.push(f(id));
        debug_assert_eq!(self[id].id(), id);
        id
    }
}

impl Index<NodeId> for Nodes {
    type Output = Node;

    fn index(&self, index: NodeId) -> &Self::Output {
        &self.nodes[index.index()]
    }
}

impl IndexMut<NodeId> for Nodes {
    fn index_mut(&mut self, index: NodeId) -> &mut Self::Output {
        &mut self.nodes[index.index()]
    }
}


pub struct NodeBase {
    id: NodeId,

    /// Inputs to the node. These are use-def references to Nodes.
    ///
    /// Generally fixed length, ordered, nulls allowed, no unused
    /// trailing space. Ordering is required because e. g. "a/ b"
    /// is different from "b/ a". The first input (offset 0) is
    /// often a isCFG node.
    pub inputs: Vec<Option<NodeId>>,

    /// Outputs reference Nodes that are not null and have this Node
    /// as an input. These nodes are users of this node, thus these
    /// are def-use references to Nodes.
    ///
    /// Outputs directly match inputs, making a directed graph that
    /// can be walked in either direction. These outputs are typically
    /// used for efficient optimizations but otherwise have no semantics
    /// meaning
    pub outputs: Vec<NodeId>,
}

pub enum Node {
    ConstantNode(ConstantNode),
    ReturnNode(ReturnNode),
    StartNode(StartNode),
}

pub struct ConstantNode {
    pub base: NodeBase,
    value: i64,
}

pub struct ReturnNode {
    pub base: NodeBase,
}

pub struct StartNode {
    pub base: NodeBase,
}

impl Node {
    pub fn id(&self) -> NodeId {
        self.base().id
    }
    pub fn base(&self) -> &NodeBase {
        match self {
            Node::ConstantNode(n) => &n.base,
            Node::ReturnNode(n) => &n.base,
            Node::StartNode(n) => &n.base,
        }
    }

    pub fn is_cfg(&self) -> bool {
        match self {
            Node::ConstantNode(_) => false,
            Node::ReturnNode(_) => true,
            Node::StartNode(_) => true
        }
    }
}

impl NodeBase {
    fn new(id: NodeId, inputs: Vec<Option<NodeId>>) -> Self {
        Self {
            id,
            inputs,
            outputs: vec![],
        }
    }
}

impl StartNode {
    pub fn new(id: NodeId) -> Self {
        Self {
            base: NodeBase::new(id, vec![])
        }
    }
}

impl ReturnNode {
    pub fn new(id: NodeId, ctrl: NodeId, data: NodeId) -> Self {
        Self {
            base: NodeBase::new(id, vec![Some(ctrl), Some(data)])
        }
    }

    pub fn ctrl(&self) -> Option<NodeId> {
        return self.base.inputs[0];
    }

    pub fn expr(&self) -> Option<NodeId> {
        return self.base.inputs[1];
    }
}

impl ConstantNode {
    pub fn new(id: NodeId, start: NodeId, value: i64) -> Self {
        Self {
            base: NodeBase::new(id, vec![Some(start)]),
            value,
        }
    }

    pub fn start(&self) -> Option<NodeId> {
        self.base.inputs[0]
    }

    pub fn value(&self) -> i64 {
        self.value
    }
}
