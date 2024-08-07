use std::borrow::Cow;
use std::fmt;
use std::fmt::{Debug, Display};
use std::num::NonZeroU32;
use std::ops::{Index, IndexMut};

use crate::bit_set;
use crate::bit_set::BitSet;
use crate::soup::types::{Ty, Types};

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct NodeId(NonZeroU32);

impl NodeId {
    pub const DUMMY: NodeId = NodeId(NonZeroU32::MAX);

    fn index(self) -> usize { self.0.get() as usize }
}

impl bit_set::Index for NodeId {
    fn index(&self) -> usize {
        NodeId::index(*self)
    }
}

impl Display for NodeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0.get(), f)
    }
}
impl Debug for NodeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.0.get(), f)
    }
}

pub struct Nodes<'t> {
    nodes: Vec<Node<'t>>,
}

impl<'t> Nodes<'t> {
    pub fn new() -> Self {
        let dummy = Node::StartNode(StartNode {
            base: NodeBase {
                id: NodeId::DUMMY,
                inputs: vec![],
                outputs: vec![],
            },
        });
        Nodes {
            nodes: vec![dummy]
        }
    }
    pub fn create<F: FnOnce(NodeId) -> Node<'t>>(&mut self, f: F) -> NodeId {
        let id = u32::try_from(self.nodes.len())
            .and_then(NonZeroU32::try_from)
            .map(NodeId).unwrap();
        self.nodes.push(f(id));
        debug_assert_eq!(self[id].id(), id);
        id
    }

    pub fn get_many<const N: usize>(&self, nodes: [Option<NodeId>; N]) -> [Option<&Node>; N] {
        nodes.map(|n| n.map(|n| &self[n]))
    }

    pub fn compute(&self, node: NodeId, types: &mut Types<'t>) -> Ty<'t> {
        match &self[node] {
            Node::ConstantNode(ConstantNode { ty, .. }) => *ty,
            Node::ReturnNode(_) => types.ty_bot,
            Node::StartNode(_) => types.ty_bot,
            Node::AddNode(AddNode { base }) => {
                match self.get_many([base.inputs[1], base.inputs[2]]) {
                    [Some(Node::ConstantNode(c1)), Some(Node::ConstantNode(c2))]
                    if c1.is_constant() && c2.is_constant()
                    => {
                        types.get_int(c1.value().wrapping_add(c2.value()))
                    }
                    _ => types.ty_bot
                }
            }
            Node::SubNode(SubNode { base }) => {
                match self.get_many([base.inputs[1], base.inputs[2]]) {
                    [Some(Node::ConstantNode(c1)), Some(Node::ConstantNode(c2))]
                    if c1.is_constant() && c2.is_constant()
                    => {
                        types.get_int(c1.value().wrapping_sub(c2.value()))
                    }
                    _ => types.ty_bot
                }
            }
            Node::MulNode(MulNode { base }) => {
                match self.get_many([base.inputs[1], base.inputs[2]]) {
                    [Some(Node::ConstantNode(c1)), Some(Node::ConstantNode(c2))]
                    if c1.is_constant() && c2.is_constant()
                    => {
                        types.get_int(c1.value().wrapping_mul(c2.value()))
                    }
                    _ => types.ty_bot
                }
            }
            Node::DivNode(DivNode { base }) => {
                match self.get_many([base.inputs[1], base.inputs[2]]) {
                    [Some(Node::ConstantNode(c1)), Some(Node::ConstantNode(c2))]
                    if c1.is_constant() && c2.is_constant()
                    => {
                        // TODO handle (or ignore?) div by 0
                        types.get_int(c1.value().wrapping_div(c2.value()))
                    }
                    _ => types.ty_bot
                }
            }
            Node::MinusNode(MinusNode { base }) => {
                match self.get_many([base.inputs[1]]) {
                    [Some(Node::ConstantNode(c1))] if c1.is_constant() => {
                        types.get_int(c1.value().wrapping_neg())
                    }
                    _ => types.ty_bot
                }
            }
        }
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

pub enum Node<'t> {
    ConstantNode(ConstantNode<'t>),
    ReturnNode(ReturnNode),
    StartNode(StartNode),
    AddNode(AddNode),
    SubNode(SubNode),
    MulNode(MulNode),
    DivNode(DivNode),
    MinusNode(MinusNode),
}

pub struct ConstantNode<'t> {
    pub base: NodeBase,
    ty: Ty<'t>,
}

pub struct ReturnNode {
    pub base: NodeBase,
}

pub struct StartNode {
    pub base: NodeBase,
}

impl<'t> Node<'t> {
    pub fn id(&self) -> NodeId {
        self.base().id
    }
    pub fn base(&self) -> &NodeBase {
        match self {
            Node::ConstantNode(n) => &n.base,
            Node::ReturnNode(n) => &n.base,
            Node::StartNode(n) => &n.base,
            Node::AddNode(n) => &n.base,
            Node::SubNode(n) => &n.base,
            Node::MulNode(n) => &n.base,
            Node::DivNode(n) => &n.base,
            Node::MinusNode(n) => &n.base,
        }
    }

    pub fn is_cfg(&self) -> bool {
        match self {
            Node::ConstantNode(_) => false,
            Node::ReturnNode(_) => true,
            Node::StartNode(_) => true,
            Node::AddNode(_) => false,
            Node::SubNode(_) => false,
            Node::MulNode(_) => false,
            Node::DivNode(_) => false,
            Node::MinusNode(_) => false,
        }
    }

    /// Easy reading label for debugger
    pub fn label(&self) -> Cow<str> {
        match self {
            Node::ConstantNode(c) => Cow::Owned(format!("#{}", c.value())),
            Node::ReturnNode(_) => Cow::Borrowed("Return"),
            Node::StartNode(_) => Cow::Borrowed("Start"),
            Node::AddNode(_) => Cow::Borrowed("Add"),
            Node::SubNode(_) => Cow::Borrowed("Sub"),
            Node::MulNode(_) => Cow::Borrowed("Mul"),
            Node::DivNode(_) => Cow::Borrowed("Div"),
            Node::MinusNode(_) => Cow::Borrowed("Minus"),
        }
    }

    // Unique label for graph visualization, e.g. "Add12" or "Region30" or "EQ99"
    pub fn unique_name(&self) -> String {
        match self {
            Node::ConstantNode(c) => format!("Con_{}", self.base().id),
            _ => format!("{}{}", self.label(), self.base().id)
        }
    }

    // Graphical label, e.g. "+" or "Region" or "=="
    pub fn glabel(&self) -> Cow<str> {
        match self {
            Node::ConstantNode(_) => self.label(),
            Node::ReturnNode(_) => self.label(),
            Node::StartNode(_) => self.label(),
            Node::AddNode(_) => Cow::Borrowed("+"),
            Node::SubNode(_) => Cow::Borrowed("-"),
            Node::MulNode(_) => Cow::Borrowed("*"),
            Node::DivNode(_) => Cow::Borrowed("//"),
            Node::MinusNode(_) => Cow::Borrowed("-"),
        }
    }

    fn is_dead(&self) -> bool {
        false
    }
}

pub struct PrintNodes<'a, 't> {
    node: Option<NodeId>,
    nodes: &'a Nodes<'t>,
}

impl Display for PrintNodes<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut visited = BitSet::zeros(self.nodes.nodes.len());
        self.nodes.fmt(self.node, f, &mut visited)
    }
}

impl<'t> Nodes<'t> {
    pub(crate) fn print(&self, node: Option<NodeId>) -> PrintNodes {
        PrintNodes {
            node,
            nodes: self,
        }
    }

    fn fmt(&self, node: Option<NodeId>, f: &mut fmt::Formatter, visited: &mut BitSet<NodeId>) -> fmt::Result {
        let Some(node) = node else { return write!(f, "<?>") };
        visited.add(node);
        match &self[node] {
            node if node.is_dead() => write!(f, "{}:DEAD", node.unique_name()),
            Node::ReturnNode(r) => {
                write!(f, "return ")?;
                self.fmt(r.expr(), f, visited)?;
                write!(f, ";")
            }
            Node::AddNode(add) => {
                write!(f, "(")?;
                self.fmt(add.base.inputs[1], f, visited)?;
                write!(f, "+")?;
                self.fmt(add.base.inputs[2], f, visited)?;
                write!(f, ")")
            }
            Node::ConstantNode(c) => write!(f, "{}", c.value()),
            n @ Node::StartNode(_) => write!(f, "{}", n.label()),
            Node::SubNode(sub) => {
                write!(f, "(")?;
                self.fmt(sub.base.inputs[1], f, visited)?;
                write!(f, "-")?;
                self.fmt(sub.base.inputs[2], f, visited)?;
                write!(f, ")")
            }
            Node::MulNode(mul) => {
                write!(f, "(")?;
                self.fmt(mul.base.inputs[1], f, visited)?;
                write!(f, "*")?;
                self.fmt(mul.base.inputs[2], f, visited)?;
                write!(f, ")")
            }
            Node::DivNode(div) => {
                write!(f, "(")?;
                self.fmt(div.base.inputs[1], f, visited)?;
                write!(f, "*")?;
                self.fmt(div.base.inputs[2], f, visited)?;
                write!(f, ")")
            }
            Node::MinusNode(minus) => {
                write!(f, "(-")?;
                self.fmt(minus.base.inputs[1], f, visited)?;
                write!(f, ")")
            }
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

impl<'t> ConstantNode<'t> {
    pub fn new(id: NodeId, start: NodeId, ty: Ty<'t>) -> Self {
        Self {
            base: NodeBase::new(id, vec![Some(start)]),
            ty,
        }
    }

    pub fn start(&self) -> Option<NodeId> {
        self.base.inputs[0]
    }

    pub fn value(&self) -> i64 {
        self.ty.unwrap_int()
    }

    pub fn is_constant(&self) -> bool {
        true
    }
}

pub struct AddNode {
    pub base: NodeBase,
}

impl AddNode {
    pub fn new(id: NodeId, [left, right]: [NodeId; 2]) -> Self {
        Self {
            base: NodeBase::new(id, vec![None, Some(left), Some(right)])
        }
    }
}

pub struct SubNode {
    pub base: NodeBase,
}

impl SubNode {
    pub fn new(id: NodeId, [left, right]: [NodeId; 2]) -> Self {
        Self {
            base: NodeBase::new(id, vec![None, Some(left), Some(right)])
        }
    }
}

pub struct MulNode {
    pub base: NodeBase,
}

impl MulNode {
    pub fn new(id: NodeId, [left, right]: [NodeId; 2]) -> Self {
        Self {
            base: NodeBase::new(id, vec![None, Some(left), Some(right)])
        }
    }
}

pub struct DivNode {
    pub base: NodeBase,
}

impl DivNode {
    pub fn new(id: NodeId, [left, right]: [NodeId; 2]) -> Self {
        Self {
            base: NodeBase::new(id, vec![None, Some(left), Some(right)])
        }
    }
}

pub struct MinusNode {
    pub base: NodeBase,
}

impl MinusNode {
    pub fn new(id: NodeId, expr: NodeId) -> Self {
        Self {
            base: NodeBase::new(id, vec![None, Some(expr)])
        }
    }
}
