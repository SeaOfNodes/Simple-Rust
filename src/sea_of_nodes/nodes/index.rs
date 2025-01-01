use crate::datastructures::id_vec::IdVec;
use crate::sea_of_nodes::nodes::node::{IfOp, PhiOp, StartOp};
use crate::sea_of_nodes::nodes::{
    BoolOp, Cfg, LoadOp, Node, Nodes, OpVec, ProjOp, ScopeOp, StoreOp,
};
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

macro_rules! ite {
    ((         ) ($($t:tt)*) ($($e:tt)*) ) => { $($e)* };
    (($($x:tt)+) ($($t:tt)*) ($($e:tt)*) ) => { $($t)* };
}

macro_rules! define_id {
    ($Id:ident, $(($op:ty))?, $downcast:ident, $t:lifetime) => {
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
                match &self[node] {
                    ite!(($($op)?) (Op::$Id(_)) (Op::$Id)) => Some($Id(node)),
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
                match &sea[self] {
                    ite!(($($op)?) (Op::$Id(_)) (Op::$Id)) => Some($Id(self)),
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
            type Output = ite!(($($op)?) ($($op)?) (Op<$t>));

            fn index(&self, index: $Id) -> &Self::Output {
                match &self[index.0] {
                    ite!(($($op)?) (Op::$Id(n)) (n @ Op::$Id)) => n,
                    _ => unreachable!(),
                }
            }
        }
        impl<$t> IndexMut<$Id> for OpVec<$t> {
            fn index_mut(&mut self, index: $Id) -> &mut Self::Output {
                match &mut self[index.0] {
                    ite!(($($op)?) (Op::$Id(n)) (n @ Op::$Id)) => n,
                    _ => unreachable!(),
                }
            }
        }

        // forward downcasting
        impl<$t> Index<$Id> for Nodes<$t> {
            type Output = ite!(($($op)?) ($($op)?) (Op<$t>));
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
    (<$t:lifetime> $($Id:ident $(($op:ty))? $downcast:ident;)*) => {
        $(define_id!($Id, $(($op))?, $downcast, $t);)*

        /// Node specific operation
        #[derive(Clone, Debug)]
        pub enum Op<$t> {
            $($Id$(($op))?),*
        }

        #[derive(Copy, Clone, Debug)]
        pub enum TypedNode {
            $($Id($Id)),*
        }

        impl Node {
            pub fn downcast(self, ops: &OpVec) -> TypedNode {
                match &ops[self] {
                    $(ite!(($($op)?) (Op::$Id(_)) (Op::$Id)) => $Id(self).into()),*
                }
            }
        }

    };
}

define_ids!(<'t>
    Constant(Ty<'t>)   to_constant;
    XCtrl              to_xctrl;
    Return             to_return;
    Start(StartOp<'t>) to_start;
    Add                to_add;
    Sub                to_sub;
    Mul                to_mul;
    Div                to_div;
    Minus              to_minus;
    Scope(ScopeOp<'t>) to_scope;
    Bool(BoolOp)       to_bool;
    Not                to_not;
    Proj(ProjOp<'t>)   to_proj;
    CProj(ProjOp<'t>)  to_cproj;
    If(IfOp)           to_if;
    Phi(PhiOp<'t>)     to_phi;
    Region             to_region;
    Loop               to_loop;
    Stop               to_stop;
    Cast(Ty<'t>)       to_cast;
    Load(LoadOp<'t>)   to_load;
    Store(StoreOp<'t>) to_store;
    New(Ty<'t>)        to_new;
);

impl Start {
    pub const DUMMY: Start = Start(Node::DUMMY);
}
impl Constant {
    pub const DUMMY: Constant = Constant(Node::DUMMY);
}
impl XCtrl {
    pub const DUMMY: XCtrl = XCtrl(Node::DUMMY);
}

// TODO impl with macro
impl Node {
    pub fn is_load(self, sea: &Nodes) -> bool {
        self.to_load(sea).is_some()
    }

    pub fn is_store(self, sea: &Nodes) -> bool {
        self.to_store(sea).is_some()
    }

    // TODO Mem Node type?
    pub fn to_mem_name<'t>(self, sea: &Nodes<'t>) -> Option<&'t str> {
        match self.downcast(&sea.ops) {
            TypedNode::Load(n) => Some(sea[n].name),
            TypedNode::Store(n) => Some(sea[n].name),
            _ => None,
        }
    }
}

impl Load {
    pub fn mem(self, sea: &Nodes) -> Option<Node> {
        self.inputs(sea)[1]
    }
    pub fn ptr(self, sea: &Nodes) -> Option<Node> {
        self.inputs(sea)[2]
    }
}
impl Store {
    pub fn mem(self, sea: &Nodes) -> Option<Node> {
        self.inputs(sea)[1]
    }
    pub fn ptr(self, sea: &Nodes) -> Option<Node> {
        self.inputs(sea)[2]
    }
}

impl Phi {
    pub fn region(self, sea: &Nodes) -> Cfg {
        self.inputs(sea)[0].unwrap().to_cfg(&sea.ops).unwrap()
    }
}

impl Loop {
    pub fn entry(self, sea: &Nodes) -> Cfg {
        self.to_cfg(&sea.ops).unwrap().cfg(1, sea).unwrap()
    }
    pub fn back(self, sea: &Nodes) -> Cfg {
        self.to_cfg(&sea.ops).unwrap().cfg(2, sea).unwrap()
    }
}
