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
    ($Id:ident, $downcast:ident, $t:lifetime, $Out:ty, $pattern:pat => $result:expr) => {
        #[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
        pub struct $Id(Node);

        // conversions
        impl From<$Id> for Node {
            fn from(value: $Id) -> Self {
                value.0
            }
        }
        impl Deref for $Id {
            type Target = Node;
            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }
        impl From<$Id> for TypedNode {
            fn from(value: $Id) -> Self {
                Self::$Id(value)
            }
        }
        impl fmt::Display for $Id {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.0.fmt(f)
            }
        }

        // downcast
        impl OpVec<'_> {
            pub fn $downcast<N: Into<Option<Node>>>(&self, node: N) -> Option<$Id> {
                let node = node.into()?;
                #[allow(unused_variables)]
                match &self[node] {
                    $pattern => Some($Id(node)),
                    _ => None,
                }
            }
        }

        // forward downcast
        impl Nodes<'_> {
            pub fn $downcast<N: Into<Option<Node>>>(&self, node: N) -> Option<$Id> {
                self.ops.$downcast(node)
            }
        }

        // downcast
        impl Node {
            pub fn $downcast(self, sea: &Nodes) -> Option<$Id> {
                #[allow(unused_variables)]
                match &sea[self] {
                    $pattern => Some($Id(self)),
                    _ => None,
                }
            }
        }

        // generic index
        impl<T> Index<$Id> for IdVec<Node, T> {
            type Output = T;
            fn index(&self, index: $Id) -> &Self::Output {
                &self[index.0]
            }
        }
        impl<T> IndexMut<$Id> for IdVec<Node, T> {
            fn index_mut(&mut self, index: $Id) -> &mut Self::Output {
                &mut self[index.0]
            }
        }

        // downcasting index
        impl<$t> Index<$Id> for OpVec<$t> {
            type Output = $Out;

            fn index(&self, index: $Id) -> &Self::Output {
                match &self[index.0] {
                    $pattern => $result,
                    _ => unreachable!(),
                }
            }
        }
        impl<$t> IndexMut<$Id> for OpVec<$t> {
            fn index_mut(&mut self, index: $Id) -> &mut Self::Output {
                match &mut self[index.0] {
                    $pattern => $result,
                    _ => unreachable!(),
                }
            }
        }

        // forward downcasting
        impl<$t> Index<$Id> for Nodes<$t> {
            type Output = $Out;
            fn index(&self, index: $Id) -> &Self::Output {
                &self.ops[index]
            }
        }
        impl<$t> IndexMut<$Id> for Nodes<$t> {
            fn index_mut(&mut self, index: $Id) -> &mut Self::Output {
                &mut self.ops[index]
            }
        }
    };
}

macro_rules! define_ids {
    (<$t:lifetime> $($Id:ident: $downcast:ident, $Out:ty, $pattern:pat => $result:expr;)*) => {
        $(define_id!($Id, $downcast, $t, $Out, $pattern => $result);)*

        pub enum TypedNode {
            $($Id($Id)),*
        }

        impl Node {
            pub fn downcast(self, ops: &OpVec) -> TypedNode {
                #[allow(unused_variables)]
                match &ops[self] {
                    $($pattern => $Id(self).into()),*
                }
            }
        }

    };
}

define_ids!(<'t>
    Constant: to_constant, Ty<'t>, Op::Constant(n) => n;
    Return: to_return, Op<'t>, n @ Op::Return => n;
    Start: to_start, StartOp<'t>, Op::Start(n) => n;
    Add: to_add, Op<'t>, n @ Op::Add => n;
    Sub: to_sub, Op<'t>, n @ Op::Sub => n;
    Mul: to_mul, Op<'t>, n @ Op::Mul => n;
    Div: to_div, Op<'t>, n @ Op::Div => n;
    Minus: to_minus, Op<'t>, n @ Op::Minus => n;
    Scope: to_scope, ScopeOp<'t>, Op::Scope(n) => n;
    Bool: to_bool, BoolOp, Op::Bool(n) => n;
    Not: to_not, Op<'t>, n @ Op::Not => n;
    Proj: to_proj, ProjOp<'t>, Op::Proj(n) => n;
    If: to_if, Op<'t>, n @ Op::If => n;
    Phi: to_phi, PhiOp<'t>, Op::Phi(n) => n;
    Region: to_region, Op<'t>, n @ Op::Region => n;
    Loop: to_loop, Op<'t>, n @ Op::Loop => n;
    Stop: to_stop, Op<'t>, n @ Op::Stop => n;
    Cast: to_cast, Ty<'t>, Op::Cast(n) => n;
    Mem: to_mem_op, MemOp<'t>, Op::Mem(n) => n;
    New: to_new, Ty<'t>, Op::New(n) => n;
);

impl Start {
    pub const DUMMY: Start = Start(Node::DUMMY);
}
