use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt;
use std::fmt::{Debug, Display};
use std::num::NonZeroU32;
use std::ops::{Deref, Index, IndexMut};
use Cow::*;

use crate::datastructures::id::Id;
use crate::datastructures::id_vec::IdVec;
use crate::sea_of_nodes::nodes::{Nodes, OpVec, ScopeOp};
use crate::sea_of_nodes::types::Ty;

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct Node(pub(super) NonZeroU32);

impl Node {
    pub const DUMMY: Node = Node(NonZeroU32::MAX);

    /// attemt to make code read more like the op version:
    ///
    /// node.in(1).in(2)._type // java
    /// self[self.inputs[self.inputs[node][1]?][2]?]
    /// node.get(&self.inputs)[1]?.get(&self.inputs)[2]?.get(&self.ty)
    /// node.inputs(self)[1]?.inputs(self)[2]?.ty(self) // doesn't allow separate borrowing
    pub fn get<T>(self, vec: &IdVec<Self, T>) -> &T {
        &vec[self]
    }

    pub fn inputs<'a>(self, sea: &'a Nodes) -> &'a Vec<Option<Node>> {
        self.get(&sea.inputs)
    }

    pub fn node<'a, 't>(self, sea: &'a Nodes<'t>) -> &'a Op<'t> {
        &sea[self]
    }

    pub fn ty<'t>(self, sea: &Nodes<'t>) -> Option<Ty<'t>> {
        *self.get(&sea.ty)
    }

    fn from_node_unchecked(node: Node) -> Node {
        node
    }
    /// upcast from subclass via deref
    pub fn to_node(self) -> Node {
        self
    }
}

impl Id for Node {
    fn index(&self) -> usize {
        self.0.get() as usize
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0.get(), f)
    }
}
impl Debug for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.0.get(), f)
    }
}

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
    ($Id:ident, $(($op:ty))?, $Super:ident, $cast:ident, $checkcast:ident, $t:lifetime) => {
        #[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
        pub struct $Id($Super);

        // conversions
        impl From<$Id> for Node {
            fn from(value: $Id) -> Node {
                value.to_node()
            }
        }
        impl Deref for $Id {
            type Target = $Super;
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
                Display::fmt(&self.0, f)
            }
        }
        impl Id for $Id {
            fn index(&self) -> usize {
                self.0.index()
            }
        }
        impl $Id {
            fn from_node_unchecked(node: Node) -> $Id {
                $Id($Super::from_node_unchecked(node))
            }
            /// upcasts from subclass via deref
            pub fn $cast(self) -> $Id {
                self
            }
        }

        // downcast
        impl OpVec<'_> {
            pub fn $cast<N: Into<Option<Node>>>(&self, node: N) -> Option<$Id> {
                let node = node.into()?;
                let class = Class::of(&self[node]);
                class.is_subclass(Class::$Id).then_some($Id::from_node_unchecked(node))
            }
        }

        // forward downcast
        impl Nodes<'_> {
            pub fn $cast<N: Into<Option<Node>>>(&self, node: N) -> Option<$Id> {
                self.ops.$cast(node)
            }
        }

        // downcast
        impl Node {
            pub fn $cast(self, sea: &Nodes) -> Option<$Id> {
                sea.ops.$cast(self)
            }
            pub fn $checkcast(self, sea: &Nodes) -> bool {
                self.$cast(sea).is_some()
            }
        }

        // generic index
        impl<T> Index<$Id> for IdVec<Node, T> {
            type Output = T;
            fn index(&self, index: $Id) -> &Self::Output {
                &self[index.to_node()]
            }
        }
        impl<T> IndexMut<$Id> for IdVec<Node, T> {
            fn index_mut(&mut self, index: $Id) -> &mut Self::Output {
                &mut self[index.to_node()]
            }
        }

        // downcasting index
        impl<$t> Index<$Id> for OpVec<$t> {
            type Output = ite!(($($op)?) ($($op)?) (Op<$t>));

            fn index(&self, index: $Id) -> &Self::Output {
                match &self[index.to_node()] {
                    ite!(($($op)?) (Op::$Id(n)) (n @ Op::$Id)) => n,
                    _ => unreachable!(),
                }
            }
        }
        impl<$t> IndexMut<$Id> for OpVec<$t> {
            fn index_mut(&mut self, index: $Id) -> &mut Self::Output {
                match &mut self[index.to_node()] {
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
    (<$t:lifetime> $($Id:ident $(($op:ty))? : $Super:ident { $cast:ident $checkcast:ident };)*) => {
        $(define_id!($Id, $(($op))?, $Super, $cast, $checkcast, $t);)*

        /// Node specific operation
        #[derive(Clone, Debug)]
        pub enum Op<$t> {
            $($Id$(($op))?),*
        }

        #[derive(Copy, Clone, Debug, Eq, PartialEq)]
        pub enum Class {
            Node,
            $($Id),*
        }

        impl Class {
            const fn superclass(self) -> Option<Class> {
                match self {
                    Class::Node => None,
                    $(Class::$Id => Some(Class::$Super)),*
                }
            }
            const fn is_subclass(self, other: Class) -> bool {
                self as usize == other as usize || if let Some(s) = self.superclass() {
                    s.is_subclass(other)
                } else {
                    false
                }
            }
            fn of(op: &Op) -> Class {
                match op {
                    $(ite!(($($op)?) (Op::$Id(_)) (Op::$Id)) => Class::$Id),*
                }
            }
        }


        #[derive(Copy, Clone, Debug)]
        pub enum TypedNode {
            $($Id($Id)),*
        }

        impl Node {
            pub fn downcast(self, ops: &OpVec) -> TypedNode {
                match &ops[self] {
                    $(ite!(($($op)?) (Op::$Id(_)) (Op::$Id)) => $Id::from_node_unchecked(self).into()),*
                }
            }
        }

    };
}

define_ids!(<'t>
    Add                : Node     { to_add       is_add       };
    AddF               : Node     { to_addf      is_addf      };
    And                : Node     { to_and       is_and       };
    Bool(BoolOp)       : Node     { to_bool      is_bool      };
    CProj(ProjOp<'t>)  : Cfg      { to_cproj     is_cproj     };
    Cast(Ty<'t>)       : Node     { to_cast      is_cast      };
    Cfg                : Node     { to_cfg       is_cfg       };
    Constant(Ty<'t>)   : Node     { to_constant  is_constant  };
    Div                : Node     { to_div       is_div       };
    DivF               : Node     { to_divf      is_divf      };
    If(IfOp)           : Cfg      { to_if        is_if        };
    Load(LoadOp<'t>)   : Node     { to_load      is_load      };
    Loop               : Region   { to_loop      is_loop      };
    Minus              : Node     { to_minus     is_minus     };
    MinusF             : Node     { to_minusf    is_minusf    };
    Mul                : Node     { to_mul       is_mul       };
    MulF               : Node     { to_mulf      is_mulf      };
    New(Ty<'t>)        : Node     { to_new       is_new       };
    Not                : Node     { to_not       is_not       };
    Or                 : Node     { to_or        is_or        };
    Phi(PhiOp<'t>)     : Node     { to_phi       is_phi       };
    Proj(ProjOp<'t>)   : Node     { to_proj      is_proj      };
    ReadOnly           : Node     { to_ronly     is_ronly     };
    Region             : Cfg      { to_region    is_region    };
    Return             : Cfg      { to_return    is_return    };
    RoundF32           : Node     { to_roundf32  is_roundf32  };
    Sar                : Node     { to_sar       is_sar       };
    Scope(ScopeOp<'t>) : ScopeMin { to_scope     is_scope     };
    ScopeMin           : Node     { to_scope_min is_scope_min };
    Shl                : Node     { to_shl       is_shl       };
    Shr                : Node     { to_shr       is_shr       };
    Start(StartOp<'t>) : Loop     { to_start     is_start     };
    Stop               : Node     { to_stop      is_stop      };
    Store(StoreOp<'t>) : Node     { to_store     is_store     };
    Struct             : Node     { to_struct    is_struct    };
    Sub                : Node     { to_sub       is_sub       };
    SubF               : Node     { to_subf      is_subf      };
    ToFloat            : Node     { to_tofloat   is_tofloat   };
    XCtrl              : Cfg      { to_xctrl     is_xctrl     };
    Xor                : Node     { to_xor       is_xor       };
);

impl<'t> Op<'t> {
    /// Easy reading label for debugger
    pub fn label(&self) -> Cow<str> {
        Borrowed(match self {
            Op::Add => "Add",
            Op::AddF => "AddF",
            Op::And => "And",
            Op::Bool(op) => match op {
                BoolOp::EQ => "EQ",
                BoolOp::LE => "LE",
                BoolOp::LT => "LT",
            },
            Op::CProj(p) => p.label,
            Op::Cast(t) => return Owned(format!("({})", t.str())),
            Op::Cfg => unreachable!(),
            Op::Constant(ty) => return Owned(format!("#{ty}")),
            Op::Div => "Div",
            Op::DivF => "DivF",
            Op::If(IfOp::Cond) => "If",
            Op::If(IfOp::Never) => "Never",
            Op::Load(_) => "Load",
            Op::Loop => "Loop",
            Op::Minus => "Minus",
            Op::MinusF => "MinusF",
            Op::Mul => "Mul",
            Op::MulF => "MulF",
            Op::New(_) => "new",
            Op::Not => "Not",
            Op::Or => "Or",
            Op::Phi(p) => return Owned(format!("Phi_{}", p.label)),
            Op::Proj(p) => p.label,
            Op::ReadOnly => "ReadOnly",
            Op::Region { .. } => "Region",
            Op::Return => "Return",
            Op::RoundF32 => "RoundF32",
            Op::Sar => "Sar",
            Op::Scope(_) => "Scope",
            Op::ScopeMin => "MEM",
            Op::Shl => "Shl",
            Op::Shr => "Shr",
            Op::Start { .. } => "Start",
            Op::Stop => "Stop",
            Op::Store(_) => "Store",
            Op::Struct => todo!(),
            Op::Sub => "Sub",
            Op::SubF => "SubF",
            Op::ToFloat => "ToFloat",
            Op::XCtrl => "Xctrl",
            Op::Xor => "Xor",
        })
    }

    // Graphical label, e.g. "+" or "Region" or "=="
    pub fn glabel(&self) -> Cow<str> {
        match self {
            Op::Add | Op::AddF => Borrowed("+"),
            Op::Sub | Op::SubF => Borrowed("-"),
            Op::Mul | Op::MulF => Borrowed("*"),
            Op::Div => Borrowed("//"),
            Op::DivF => Borrowed("/"),
            Op::Or => Borrowed("|"),
            Op::And => Borrowed("&"),
            Op::Xor => Borrowed("^"),
            Op::Minus | Op::MinusF => Borrowed("-"),
            Op::Bool(op) => Borrowed(op.str()),
            Op::Not => Borrowed("!"),
            Op::Phi(p) => Owned(format!("&phi;_{}", p.label)),
            Op::Cast(t) => Owned(format!("({})", t.str())),
            Op::New(_) => Borrowed("new"),
            Op::Load(_) => Borrowed("Load"),
            Op::Store(_) => Borrowed("Store"),
            Op::RoundF32 => Borrowed("(f32)"),
            Op::ToFloat => Borrowed("(flt)"),
            Op::Sar => Borrowed(">>"),
            Op::Shl => Borrowed("<<"),
            Op::Shr => Borrowed(">>>"),
            Op::CProj(_)
            | Op::Constant(_)
            | Op::If(_)
            | Op::Loop
            | Op::Proj(_)
            | Op::ReadOnly
            | Op::Region { .. }
            | Op::Return
            | Op::Scope(_)
            | Op::ScopeMin
            | Op::Start { .. }
            | Op::Stop
            | Op::Struct
            | Op::XCtrl => self.label(),
            Op::Cfg => unreachable!(),
        }
    }

    // implements the MultiNode interface
    pub fn is_multi_node(&self) -> bool {
        matches!(self, Op::Start(_) | Op::If(_))
    }

    pub fn operation(&self) -> usize {
        match self {
            Op::Constant(_) => 0,
            Op::XCtrl => 1,
            Op::Return => 2,
            Op::Start { .. } => 3,
            Op::Add => 4,
            Op::Sub => 5,
            Op::Mul => 6,
            Op::Div => 7,
            Op::Minus => 8,
            Op::Scope(_) => 9,
            Op::Bool(BoolOp::EQ) => 10,
            Op::Bool(BoolOp::LT) => 11,
            Op::Bool(BoolOp::LE) => 12,
            Op::Not => 13,
            Op::Proj(_) => 14,
            Op::CProj(_) => 15,
            Op::If(IfOp::Cond) => 16,
            Op::If(IfOp::Never) => 17,
            Op::Phi(_) => 18,
            Op::Region { .. } => 19,
            Op::Loop => 20,
            Op::Stop => 21,
            Op::Cast(_) => 22,
            Op::New(_) => 23,
            Op::Load(_) => 24,
            Op::Store(_) => 25,
            Op::AddF => 26,
            Op::SubF => 27,
            Op::MulF => 28,
            Op::DivF => 29,
            Op::Or => 30,
            Op::And => 31,
            Op::Xor => 32,
            Op::MinusF => 33,
            Op::ReadOnly => 34,
            Op::ScopeMin => 35,
            Op::RoundF32 => 36,
            Op::Sar => 37,
            Op::Shl => 38,
            Op::Shr => 39,
            Op::Struct => 40,
            Op::ToFloat => 41,
            Op::Cfg => unreachable!(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct StartOp<'t> {
    pub args: Ty<'t>,
    pub alias_starts: HashMap<&'t str, u32>,
}

impl Start {
    pub const DUMMY: Start = Start(Loop(Region(Cfg(Node::DUMMY))));

    pub fn new<'t>(args: &[Ty<'t>], sea: &mut Nodes<'t>) -> Self {
        let args = sea.types.get_tuple_from_slice(args);
        let this = Start::from_node_unchecked(sea.create((
            Op::Start(StartOp {
                args,
                alias_starts: HashMap::new(),
            }),
            vec![],
        )));
        sea.ty[this] = Some(args);
        this
    }
}

impl Return {
    pub fn new(ctrl: Node, data: Node, scope: Option<Scope>, sea: &mut Nodes) -> Self {
        let this =
            Return::from_node_unchecked(sea.create((Op::Return, vec![Some(ctrl), Some(data)])));

        if let Some(scope) = scope {
            // We lookup memory slices by the naming convention that they start with $
            // We could also use implicit knowledge that all memory projects are at offset >= 2
            let names = scope.reverse_names(sea);
            for name in names.into_iter().map(Option::unwrap) {
                if name.starts_with("$") && name != Scope::CTRL {
                    let v = scope.lookup(name, sea).unwrap();
                    this.add_def(Some(v), sea);
                }
            }
        }

        this
    }
}
impl Constant {
    pub const DUMMY: Constant = Constant(Node::DUMMY);

    pub fn new<'t>(ty: Ty<'t>, sea: &mut Nodes<'t>) -> Self {
        let start = sea.start;
        Constant(sea.create((Op::Constant(ty), vec![Some(start.to_node())])))
    }
}
impl XCtrl {
    pub const DUMMY: XCtrl = XCtrl(Cfg(Node::DUMMY));

    pub fn new(sea: &mut Nodes) -> Self {
        let start = sea.start;
        XCtrl::from_node_unchecked(sea.create((Op::XCtrl, vec![Some(start.to_node())])))
    }
}

impl Add {
    pub fn new(left: Node, right: Node, sea: &mut Nodes) -> Self {
        Add::from_node_unchecked(sea.create((Op::Add, vec![None, Some(left), Some(right)])))
    }
}
impl Sub {
    pub fn new(left: Node, right: Node, sea: &mut Nodes) -> Self {
        Sub::from_node_unchecked(sea.create((Op::Sub, vec![None, Some(left), Some(right)])))
    }
}
impl Mul {
    pub fn new(left: Node, right: Node, sea: &mut Nodes) -> Self {
        Mul::from_node_unchecked(sea.create((Op::Mul, vec![None, Some(left), Some(right)])))
    }
}
impl Div {
    pub fn new(left: Node, right: Node, sea: &mut Nodes) -> Self {
        Div::from_node_unchecked(sea.create((Op::Div, vec![None, Some(left), Some(right)])))
    }
}

impl Minus {
    pub fn new(expr: Node, sea: &mut Nodes) -> Self {
        Minus::from_node_unchecked(sea.create((Op::Minus, vec![None, Some(expr)])))
    }
}

impl Scope {
    pub fn new(sea: &mut Nodes) -> Self {
        Scope::from_node_unchecked(sea.create((Op::Scope(ScopeOp { scopes: vec![] }), vec![])))
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum BoolOp {
    EQ,
    LT,
    LE,
}
impl BoolOp {
    pub(crate) fn str(&self) -> &'static str {
        match self {
            BoolOp::EQ => "==",
            BoolOp::LT => "<",
            BoolOp::LE => "<=",
        }
    }

    pub(crate) fn compute(&self, x: i64, y: i64) -> bool {
        match self {
            BoolOp::EQ => x == y,
            BoolOp::LT => x < y,
            BoolOp::LE => x <= y,
        }
    }
}

impl Bool {
    pub fn new(left: Node, right: Node, op: BoolOp, sea: &mut Nodes) -> Self {
        Bool::from_node_unchecked(sea.create((Op::Bool(op), vec![None, Some(left), Some(right)])))
    }
}

impl Not {
    pub fn new(expr: Node, sea: &mut Nodes) -> Self {
        Not::from_node_unchecked(sea.create((Op::Not, vec![None, Some(expr)])))
    }
}

#[derive(Clone, Debug)]
pub struct ProjOp<'t> {
    pub index: usize,
    pub label: &'t str,
}

impl Proj {
    pub fn new<'t, N: Into<Node>>(
        ctrl: N,
        index: usize,
        label: &'t str,
        sea: &mut Nodes<'t>,
    ) -> Self {
        Proj::from_node_unchecked(
            sea.create((Op::Proj(ProjOp { index, label }), vec![Some(ctrl.into())])),
        )
    }
}

impl CProj {
    pub fn new<'t, N: Into<Node>>(
        ctrl: N,
        index: usize,
        label: &'t str,
        sea: &mut Nodes<'t>,
    ) -> Self {
        CProj::from_node_unchecked(
            sea.create((Op::CProj(ProjOp { index, label }), vec![Some(ctrl.into())])),
        )
    }
}

#[derive(Clone, Debug)]
pub enum IfOp {
    Cond,
    Never,
}

impl If {
    pub fn new(ctrl: Node, pred: Option<Node>, sea: &mut Nodes) -> Self {
        let (op, pred) = match pred {
            Some(n) => (IfOp::Cond, n),
            None => (IfOp::Never, *sea.zero),
        };
        let this = If::from_node_unchecked(sea.create((Op::If(op), vec![Some(ctrl), Some(pred)])));
        sea.iter_peeps.add(this.to_node()); // Because idoms are complex, just add it
        this
    }
    pub fn pred(self, sea: &Nodes) -> Option<Node> {
        self.inputs(sea)[1]
    }
}

#[derive(Clone, Debug)]
pub struct PhiOp<'t> {
    pub label: &'t str,
    pub ty: Ty<'t>,
}

impl Phi {
    pub fn new<'t>(
        label: &'t str,
        ty: Ty<'t>,
        inputs: Vec<Option<Node>>,
        sea: &mut Nodes<'t>,
    ) -> Self {
        Phi::from_node_unchecked(sea.create((Op::Phi(PhiOp { label, ty }), inputs)))
    }
    pub fn region(self, sea: &Nodes) -> Cfg {
        self.inputs(sea)[0].unwrap().to_cfg(sea).unwrap()
    }
}
impl Region {
    pub fn new(inputs: Vec<Option<Node>>, sea: &mut Nodes) -> Self {
        Region::from_node_unchecked(sea.create((Op::Region, inputs)))
    }
}
impl Stop {
    pub fn new(sea: &mut Nodes) -> Self {
        Stop::from_node_unchecked(sea.create((Op::Stop, vec![])))
    }
}
impl Loop {
    pub fn new(entry: Node, sea: &mut Nodes) -> Self {
        Loop::from_node_unchecked(sea.create((Op::Loop, vec![None, Some(entry), None])))
    }

    pub fn entry(self, sea: &Nodes) -> Cfg {
        self.to_cfg().cfg(1, sea).unwrap()
    }
    pub fn back(self, sea: &Nodes) -> Cfg {
        self.to_cfg().cfg(2, sea).unwrap()
    }
}
impl Cast {
    pub fn new<'t>(ty: Ty<'t>, ctrl: Node, i: Node, sea: &mut Nodes<'t>) -> Self {
        Cast::from_node_unchecked(sea.create((Op::Cast(ty), vec![Some(ctrl), Some(i)])))
    }
}

#[derive(Clone, Debug)]
pub struct LoadOp<'t> {
    pub name: &'t str,
    pub alias: u32,
    pub declared_type: Ty<'t>,
}

impl Load {
    pub fn new<'t>(
        name: &'t str,
        alias: u32,
        declared_type: Ty<'t>,
        [mem_slice, mem_ptr]: [Node; 2],
        sea: &mut Nodes<'t>,
    ) -> Self {
        Load::from_node_unchecked(sea.create((
            Op::Load(LoadOp {
                name,
                alias,
                declared_type,
            }),
            vec![None, Some(mem_slice), Some(mem_ptr)],
        )))
    }
    pub fn mem(self, sea: &Nodes) -> Option<Node> {
        self.inputs(sea)[1]
    }
    pub fn ptr(self, sea: &Nodes) -> Option<Node> {
        self.inputs(sea)[2]
    }
}

#[derive(Clone, Debug)]
pub struct StoreOp<'t> {
    pub name: &'t str,
    pub alias: u32,
}

impl Store {
    pub fn new<'t>(
        name: &'t str,
        alias: u32,
        [ctrl, mem_slice, mem_ptr, value]: [Node; 4],
        sea: &mut Nodes<'t>,
    ) -> Self {
        Store::from_node_unchecked(sea.create((
            Op::Store(StoreOp { name, alias }),
            vec![Some(ctrl), Some(mem_slice), Some(mem_ptr), Some(value)],
        )))
    }
    pub fn mem(self, sea: &Nodes) -> Option<Node> {
        self.inputs(sea)[1]
    }
    pub fn ptr(self, sea: &Nodes) -> Option<Node> {
        self.inputs(sea)[2]
    }
}

impl Node {
    // TODO Mem Node type?
    pub fn to_mem_name<'t>(self, sea: &Nodes<'t>) -> Option<&'t str> {
        match self.downcast(&sea.ops) {
            TypedNode::Load(n) => Some(sea[n].name),
            TypedNode::Store(n) => Some(sea[n].name),
            _ => None,
        }
    }
}

impl New {
    pub fn new<'t>(ptr: Ty<'t>, ctrl: Node, sea: &mut Nodes<'t>) -> Self {
        New::from_node_unchecked(sea.create((Op::New(ptr), vec![Some(ctrl)])))
    }
}
