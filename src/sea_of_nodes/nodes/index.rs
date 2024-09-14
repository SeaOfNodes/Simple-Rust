use crate::datastructures::id_vec::IdVec;
use crate::sea_of_nodes::nodes::node::{MemOp, PhiOp, StartOp};
use crate::sea_of_nodes::nodes::{BoolOp, NodeId, NodeVec, Nodes, Op, ProjOp, ScopeOp};
use crate::sea_of_nodes::types::Ty;
use std::fmt;
use std::ops::{Deref, Index, IndexMut};

// generic index
impl<'t> Index<NodeId> for NodeVec<'t> {
    type Output = Op<'t>;
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
    type Output = Op<'t>;
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

        #[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
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
        impl fmt::Display for $Id {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { self.0.fmt(f) }
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

define_id!(Constant, ConstantId, to_constant, 't, Ty<'t>, Op::Constant(n) => n);
define_id!(Return, ReturnId, to_return, 't, Op<'t>, n @ Op::Return => n);
define_id!(Start, StartId, to_start, 't, StartOp<'t>, Op::Start(n) => n);
define_id!(Add, AddId, to_add, 't, Op<'t>, n @ Op::Add => n);
define_id!(Sub, SubId, to_sub, 't, Op<'t>, n @ Op::Sub => n);
define_id!(Mul, MulId, to_mul, 't, Op<'t>, n @ Op::Mul => n);
define_id!(Div, DivId, to_div, 't, Op<'t>, n @ Op::Div => n);
define_id!(Minus, MinusId, to_minus, 't, Op<'t>, n @ Op::Minus => n);
define_id!(Scope, ScopeId, to_scope, 't, ScopeOp<'t>, Op::Scope(n) => n);
define_id!(Bool, BoolId, to_bool, 't, BoolOp, Op::Bool(n) => n);
define_id!(Not, NotId, to_not, 't, Op<'t>, n @ Op::Not => n);
define_id!(Proj, ProjId, to_proj, 't, ProjOp<'t>, Op::Proj(n) => n);
define_id!(If, IfId, to_if, 't, Op<'t>, n @ Op::If => n);
define_id!(Phi, PhiId, to_phi, 't, PhiOp<'t>, Op::Phi(n) => n);
define_id!(Region, RegionId, to_region, 't, Op<'t>, n @ Op::Region => n);
define_id!(Loop, LoopId, to_loop, 't, Op<'t>, n @ Op::Loop => n);
define_id!(Stop, StopId, to_stop, 't, Op<'t>, n @ Op::Stop => n);
define_id!(Cast, CastId, to_cast, 't, Ty<'t>, Op::Cast(n) => n);
define_id!(MemOp, MemOpId, to_mem_op, 't, MemOp<'t>, Op::MemOp(n) => n);
define_id!(New, NewId, to_new, 't, Ty<'t>, Op::New(n) => n);

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
            Op::Constant(_) => ConstantId(node).into(),
            Op::Return => ReturnId(node).into(),
            Op::Start { .. } => StartId(node).into(),
            Op::Add => AddId(node).into(),
            Op::Sub => SubId(node).into(),
            Op::Mul => MulId(node).into(),
            Op::Div => DivId(node).into(),
            Op::Minus => MinusId(node).into(),
            Op::Scope(_) => ScopeId(node).into(),
            Op::Bool(_) => BoolId(node).into(),
            Op::Not => NotId(node).into(),
            Op::Proj(_) => ProjId(node).into(),
            Op::If => IfId(node).into(),
            Op::Phi(_) => PhiId(node).into(),
            Op::Region { .. } => RegionId(node).into(),
            Op::Loop => LoopId(node).into(),
            Op::Stop => StopId(node).into(),
            Op::Cast(_) => CastId(node).into(),
            Op::MemOp(_) => MemOpId(node).into(),
            Op::New(_) => NewId(node).into(),
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
