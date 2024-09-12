use crate::datastructures::id_vec::IdVec;
use crate::sea_of_nodes::nodes::node::{MemOp, PhiNode, StartNode};
use crate::sea_of_nodes::nodes::{BoolOp, Node, NodeId, NodeVec, Nodes, ProjNode, ScopeNode};
use crate::sea_of_nodes::types::Ty;
use std::ops::{Deref, Index, IndexMut};

// generic index
impl<'t> Index<NodeId> for NodeVec<'t> {
    type Output = Node<'t>;
    fn index(&self, index: NodeId) -> &Self::Output {
        &self.0[index]
    }
}
impl<'t> IndexMut<NodeId> for NodeVec<'t> {
    fn index_mut(&mut self, index: NodeId) -> &mut Self::Output {
        &mut self.0[index]
    }
}

// forward generic index
impl<'t> Index<NodeId> for Nodes<'t> {
    type Output = Node<'t>;
    fn index(&self, index: NodeId) -> &Self::Output {
        &self.nodes[index]
    }
}
impl<'t> IndexMut<NodeId> for Nodes<'t> {
    fn index_mut(&mut self, index: NodeId) -> &mut Self::Output {
        &mut self.nodes[index]
    }
}

macro_rules! define_id {
    ($TyId:ident, $Id:ident, $downcast:ident, $t:lifetime, $Out:ty, $($pattern:pat => $result:expr),*) => {

        #[derive(Copy, Clone, Eq, PartialEq, Hash)]
        pub struct $Id(NodeId);

        // conversions
        impl From<$Id> for NodeId {
            fn from(value: $Id) -> Self { value.0 }
        }
        impl Deref for $Id {
            type Target = NodeId;
            fn deref(&self) -> &Self::Target { &self.0 }
        }
        impl From<$Id> for TypedNodeId {
            fn from(value: $Id) -> Self { Self::$TyId(value) }
        }

        // downcast
        impl NodeVec<'_> {
            pub fn $downcast<N: Into<Option<NodeId>>>(&self, node: N) -> Option<$Id> {
                let node = node.into()?;
                #[allow(unused_variables)]
                match &self[node] {
                    $($pattern => Some($Id(node)),)*
                    _ => None,
                }
            }
        }

        // forward downcast
        impl Nodes<'_> {
            pub fn $downcast<N: Into<Option<NodeId>>>(&self, node: N) -> Option<$Id> { self.nodes.$downcast(node) }
        }

        // generic index
        impl<T> Index<$Id> for IdVec<NodeId, T> {
            type Output = T;
            fn index(&self, index: $Id) -> &Self::Output { &self[index.0] }
        }
        impl<T> IndexMut<$Id> for IdVec<NodeId, T> {
            fn index_mut(&mut self, index: $Id) -> &mut Self::Output { &mut self[index.0] }
        }

        // downcasting index
        impl<$t> Index<$Id> for NodeVec<$t> {
            type Output = $Out;

            fn index(&self, index: $Id) -> &Self::Output {
                match &self[index.0] {
                    $($pattern => $result,)*
                    _ => unreachable!()
                }
            }
        }
        impl<$t> IndexMut<$Id> for NodeVec<$t> {
            fn index_mut(&mut self, index: $Id) -> &mut Self::Output {
                match &mut self[index.0] {
                    $($pattern => $result,)*
                    _ => unreachable!()
                }
            }
        }

        // forward downcasting
        impl<$t> Index<$Id> for Nodes<$t> {
            type Output = $Out;
            fn index(&self, index: $Id) -> &Self::Output { &self.nodes[index] }
        }
        impl<$t> IndexMut<$Id> for Nodes<$t> {
            fn index_mut(&mut self, index: $Id) -> &mut Self::Output { &mut self.nodes[index] }
        }
    };
}

define_id!(Constant, ConstantId, to_constant, 't, Ty<'t>, Node::Constant(n) => n);
define_id!(Return, ReturnId, to_return, 't, Node<'t>, n @ Node::Return => n);
define_id!(Start, StartId, to_start, 't, StartNode<'t>, Node::Start(n) => n);
define_id!(Add, AddId, to_add, 't, Node<'t>, n @ Node::Add => n);
define_id!(Sub, SubId, to_sub, 't, Node<'t>, n @ Node::Sub => n);
define_id!(Mul, MulId, to_mul, 't, Node<'t>, n @ Node::Mul => n);
define_id!(Div, DivId, to_div, 't, Node<'t>, n @ Node::Div => n);
define_id!(Minus, MinusId, to_minus, 't, Node<'t>, n @ Node::Minus => n);
define_id!(Scope, ScopeId, to_scope, 't, ScopeNode<'t>, Node::Scope(n) => n);
define_id!(Bool, BoolId, to_bool, 't, BoolOp, Node::Bool(n) => n);
define_id!(Not, NotId, to_not, 't, Node<'t>, n @ Node::Not => n);
define_id!(Proj, ProjId, to_proj, 't, ProjNode, Node::Proj(n) => n);
define_id!(If, IfId, to_if, 't, Node<'t>, n @ Node::If => n);
define_id!(Phi, PhiId, to_phi, 't, PhiNode<'t>, Node::Phi(n) => n);
define_id!(Region, RegionId, to_region, 't, Option<NodeId>, Node::Region { cached_idom: n } => n);
define_id!(Loop, LoopId, to_loop, 't, Node<'t>, n @ Node::Loop => n);
define_id!(Stop, StopId, to_stop, 't, Node<'t>, n @ Node::Stop => n);
define_id!(Cast, CastId, to_cast, 't, Ty<'t>, Node::Cast(n) => n);
define_id!(MemOp, MemOpId, to_mem_op, 't, MemOp<'t>, Node::MemOp(n) => n);
define_id!(New, NewId, to_new, 't, Ty<'t>, Node::New(n) => n);

pub enum TypedNodeId {
    Constant(ConstantId),
    Return(ReturnId),
    Start(StartId),
    Add(AddId),
    Sub(SubId),
    Mul(MulId),
    Div(DivId),
    Minus(MinusId),
    Scope(ScopeId),
    Bool(BoolId),
    Not(NotId),
    Proj(ProjId),
    If(IfId),
    Phi(PhiId),
    Region(RegionId),
    Loop(LoopId),
    Stop(StopId),
    Cast(CastId),
    MemOp(MemOpId),
    New(NewId),
}

impl<'t> NodeVec<'t> {
    pub fn downcast(&self, node: NodeId) -> TypedNodeId {
        match &self[node] {
            Node::Constant(_) => ConstantId(node).into(),
            Node::Return => ReturnId(node).into(),
            Node::Start { .. } => StartId(node).into(),
            Node::Add => AddId(node).into(),
            Node::Sub => SubId(node).into(),
            Node::Mul => MulId(node).into(),
            Node::Div => DivId(node).into(),
            Node::Minus => MinusId(node).into(),
            Node::Scope(_) => ScopeId(node).into(),
            Node::Bool(_) => BoolId(node).into(),
            Node::Not => NotId(node).into(),
            Node::Proj(_) => ProjId(node).into(),
            Node::If => IfId(node).into(),
            Node::Phi(_) => PhiId(node).into(),
            Node::Region { .. } => RegionId(node).into(),
            Node::Loop => LoopId(node).into(),
            Node::Stop => StopId(node).into(),
            Node::Cast(_) => CastId(node).into(),
            Node::MemOp(_) => MemOpId(node).into(),
            Node::New(_) => NewId(node).into(),
        }
    }
}

impl<'t> Nodes<'t> {
    pub fn downcast(&self, node: NodeId) -> TypedNodeId {
        self.nodes.downcast(node)
    }
}

impl StartId {
    pub const DUMMY: StartId = StartId(NodeId::DUMMY);
}
