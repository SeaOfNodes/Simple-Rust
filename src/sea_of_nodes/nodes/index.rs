use crate::datastructures::id_vec::IdVec;
use crate::sea_of_nodes::nodes::node::{MemOp, PhiOp, StartOp};
use crate::sea_of_nodes::nodes::{BoolOp, Node, Nodes, Op, OpVec, ProjOp, ScopeOp};
use crate::sea_of_nodes::types::Ty;
use std::fmt;
use std::ops::{Deref, Index, IndexMut};

// generic index
impl<'t> Index<Node> for OpVec<'t> {
    type Output = Op<'t>;
    fn index(&self, index: Node) -> &Self::Output {
        &self.0[index]
    }
}
impl<'t> IndexMut<Node> for OpVec<'t> {
    fn index_mut(&mut self, index: Node) -> &mut Self::Output {
        &mut self.0[index]
    }
}

// forward generic index
impl<'t> Index<Node> for Nodes<'t> {
    type Output = Op<'t>;
    fn index(&self, index: Node) -> &Self::Output {
        &self.ops[index]
    }
}
impl<'t> IndexMut<Node> for Nodes<'t> {
    fn index_mut(&mut self, index: Node) -> &mut Self::Output {
        &mut self.ops[index]
    }
}

macro_rules! define_id {
    ($Id:ident, $downcast:ident, $t:lifetime, $Out:ty, $($pattern:pat => $result:expr),*) => {

        #[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
        pub struct $Id(Node);

        // conversions
        impl From<$Id> for Node {
            fn from(value: $Id) -> Self { value.0 }
        }
        impl Deref for $Id {
            type Target = Node;
            fn deref(&self) -> &Self::Target { &self.0 }
        }
        impl From<$Id> for TypedNode {
            fn from(value: $Id) -> Self { Self::$Id(value) }
        }
        impl fmt::Display for $Id {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { self.0.fmt(f) }
        }

        // downcast
        impl OpVec<'_> {
            pub fn $downcast<N: Into<Option<Node>>>(&self, node: N) -> Option<$Id> {
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
            pub fn $downcast<N: Into<Option<Node>>>(&self, node: N) -> Option<$Id> { self.ops.$downcast(node) }
        }

        // generic index
        impl<T> Index<$Id> for IdVec<Node, T> {
            type Output = T;
            fn index(&self, index: $Id) -> &Self::Output { &self[index.0] }
        }
        impl<T> IndexMut<$Id> for IdVec<Node, T> {
            fn index_mut(&mut self, index: $Id) -> &mut Self::Output { &mut self[index.0] }
        }

        // downcasting index
        impl<$t> Index<$Id> for OpVec<$t> {
            type Output = $Out;

            fn index(&self, index: $Id) -> &Self::Output {
                match &self[index.0] {
                    $($pattern => $result,)*
                    _ => unreachable!()
                }
            }
        }
        impl<$t> IndexMut<$Id> for OpVec<$t> {
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
            fn index(&self, index: $Id) -> &Self::Output { &self.ops[index] }
        }
        impl<$t> IndexMut<$Id> for Nodes<$t> {
            fn index_mut(&mut self, index: $Id) -> &mut Self::Output { &mut self.ops[index] }
        }
    };
}

define_id!(Constant, to_constant, 't, Ty<'t>, Op::Constant(n) => n);
define_id!(Return, to_return, 't, Op<'t>, n @ Op::Return => n);
define_id!(Start, to_start, 't, StartOp<'t>, Op::Start(n) => n);
define_id!(Add, to_add, 't, Op<'t>, n @ Op::Add => n);
define_id!(Sub, to_sub, 't, Op<'t>, n @ Op::Sub => n);
define_id!(Mul, to_mul, 't, Op<'t>, n @ Op::Mul => n);
define_id!(Div, to_div, 't, Op<'t>, n @ Op::Div => n);
define_id!(Minus, to_minus, 't, Op<'t>, n @ Op::Minus => n);
define_id!(Scope, to_scope, 't, ScopeOp<'t>, Op::Scope(n) => n);
define_id!(Bool, to_bool, 't, BoolOp, Op::Bool(n) => n);
define_id!(Not, to_not, 't, Op<'t>, n @ Op::Not => n);
define_id!(Proj, to_proj, 't, ProjOp<'t>, Op::Proj(n) => n);
define_id!(If, to_if, 't, Op<'t>, n @ Op::If => n);
define_id!(Phi, to_phi, 't, PhiOp<'t>, Op::Phi(n) => n);
define_id!(Region, to_region, 't, Op<'t>, n @ Op::Region => n);
define_id!(Loop, to_loop, 't, Op<'t>, n @ Op::Loop => n);
define_id!(Stop, to_stop, 't, Op<'t>, n @ Op::Stop => n);
define_id!(Cast, to_cast, 't, Ty<'t>, Op::Cast(n) => n);
define_id!(Mem, to_mem_op, 't, MemOp<'t>, Op::Mem(n) => n);
define_id!(New, to_new, 't, Ty<'t>, Op::New(n) => n);

pub enum TypedNode {
    Constant(Constant),
    Return(Return),
    Start(Start),
    Add(Add),
    Sub(Sub),
    Mul(Mul),
    Div(Div),
    Minus(Minus),
    Scope(Scope),
    Bool(Bool),
    Not(Not),
    Proj(Proj),
    If(If),
    Phi(Phi),
    Region(Region),
    Loop(Loop),
    Stop(Stop),
    Cast(Cast),
    Mem(Mem),
    New(New),
}

impl<'t> OpVec<'t> {
    pub fn downcast(&self, node: Node) -> TypedNode {
        match &self[node] {
            Op::Constant(_) => Constant(node).into(),
            Op::Return => Return(node).into(),
            Op::Start { .. } => Start(node).into(),
            Op::Add => Add(node).into(),
            Op::Sub => Sub(node).into(),
            Op::Mul => Mul(node).into(),
            Op::Div => Div(node).into(),
            Op::Minus => Minus(node).into(),
            Op::Scope(_) => Scope(node).into(),
            Op::Bool(_) => Bool(node).into(),
            Op::Not => Not(node).into(),
            Op::Proj(_) => Proj(node).into(),
            Op::If => If(node).into(),
            Op::Phi(_) => Phi(node).into(),
            Op::Region { .. } => Region(node).into(),
            Op::Loop => Loop(node).into(),
            Op::Stop => Stop(node).into(),
            Op::Cast(_) => Cast(node).into(),
            Op::Mem(_) => Mem(node).into(),
            Op::New(_) => New(node).into(),
        }
    }
}

impl<'t> Nodes<'t> {
    pub fn downcast(&self, node: Node) -> TypedNode {
        self.ops.downcast(node)
    }
}

impl Start {
    pub const DUMMY: Start = Start(Node::DUMMY);
}
