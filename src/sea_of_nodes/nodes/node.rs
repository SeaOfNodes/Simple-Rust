use crate::sea_of_nodes::nodes::index::{
    Add, Bool, CProj, Cast, Constant, Div, If, Load, Loop, Minus, Mul, New, Not, Phi, Proj, Region,
    Return, Scope, Start, Stop, Store, Sub, XCtrl,
};
use crate::sea_of_nodes::nodes::{Node, Nodes, Op, ScopeOp};
use crate::sea_of_nodes::types::Ty;
use std::borrow::Cow;
use std::collections::HashMap;
use Cow::*;

#[derive(Clone, Debug)]
pub struct StartOp<'t> {
    pub args: Ty<'t>,
    pub alias_starts: HashMap<&'t str, u32>,
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

#[derive(Clone, Debug)]
pub struct ProjOp<'t> {
    pub index: usize,
    pub label: &'t str,
}

#[derive(Clone, Debug)]
pub struct PhiOp<'t> {
    pub label: &'t str,
    pub ty: Ty<'t>,
}

#[derive(Clone, Debug)]
pub struct LoadOp<'t> {
    pub name: &'t str,
    pub alias: u32,
    pub declared_type: Ty<'t>,
}

#[derive(Clone, Debug)]
pub struct StoreOp<'t> {
    pub name: &'t str,
    pub alias: u32,
}

#[derive(Clone, Debug)]
pub enum IfOp {
    Cond,
    Never,
}

impl<'t> Op<'t> {
    /// Easy reading label for debugger
    pub fn label(&self) -> Cow<str> {
        Borrowed(match self {
            Op::Constant(ty) => return Owned(format!("#{ty}")),
            Op::XCtrl => "Xctrl",
            Op::Return => "Return",
            Op::Start { .. } => "Start",
            Op::Add => "Add",
            Op::Sub => "Sub",
            Op::Mul => "Mul",
            Op::Div => "Div",
            Op::Minus => "Minus",
            Op::Scope(_) => "Scope",
            Op::Bool(op) => match op {
                BoolOp::EQ => "EQ",
                BoolOp::LT => "LT",
                BoolOp::LE => "LE",
            },
            Op::Not => "Not",
            Op::Proj(p) => p.label,
            Op::CProj(p) => p.label,
            Op::If(IfOp::Cond) => "If",
            Op::If(IfOp::Never) => "Never",
            Op::Phi(p) => return Owned(format!("Phi_{}", p.label)),
            Op::Region { .. } => "Region",
            Op::Loop => "Loop",
            Op::Stop => "Stop",
            Op::Cast(t) => return Owned(format!("({})", t.str())),
            Op::New(_) => "new",
            Op::Load(_) => "Load",
            Op::Store(_) => "Store",
        })
    }

    // Graphical label, e.g. "+" or "Region" or "=="
    pub fn glabel(&self) -> Cow<str> {
        match self {
            Op::Constant(_) => self.label(),
            Op::XCtrl => self.label(),
            Op::Return => self.label(),
            Op::Start { .. } => self.label(),
            Op::Add => Borrowed("+"),
            Op::Sub => Borrowed("-"),
            Op::Mul => Borrowed("*"),
            Op::Div => Borrowed("//"),
            Op::Minus => Borrowed("-"),
            Op::Scope(_) => self.label(),
            Op::Bool(op) => Borrowed(op.str()),
            Op::Not => Borrowed("!"),
            Op::Proj(_) => self.label(),
            Op::CProj(_) => self.label(),
            Op::If(_) => self.label(),
            Op::Phi(p) => Owned(format!("&phi;_{}", p.label)),
            Op::Region { .. } => self.label(),
            Op::Loop => self.label(),
            Op::Stop => self.label(),
            Op::Cast(t) => Owned(format!("({})", t.str())),
            Op::New(_) => Borrowed("new"),
            Op::Load(_) => Borrowed("Load"),
            Op::Store(_) => Borrowed("Store"),
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
        }
    }
}

impl Start {
    pub fn new<'t>(args: &[Ty<'t>], sea: &mut Nodes<'t>) -> Self {
        let args = sea.types.get_tuple_from_slice(args);
        let this = sea.create((
            Op::Start(StartOp {
                args,
                alias_starts: HashMap::new(),
            }),
            vec![],
        ));
        sea.ty[this] = Some(args);
        this.to_start(sea).unwrap()
    }
}

impl Return {
    pub fn new(ctrl: Node, data: Node, scope: Option<Scope>, sea: &mut Nodes) -> Self {
        let this = sea.create((Op::Return, vec![Some(ctrl), Some(data)]));

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

        this.to_return(sea).unwrap()
    }
}
impl Constant {
    pub fn new<'t>(ty: Ty<'t>, sea: &mut Nodes<'t>) -> Self {
        let start = sea.start;
        let this = sea.create((Op::Constant(ty), vec![Some(*start)]));
        this.to_constant(sea).unwrap()
    }
}
impl XCtrl {
    pub fn new(sea: &mut Nodes) -> Self {
        let start = sea.start;
        let this = sea.create((Op::XCtrl, vec![Some(*start)]));
        this.to_xctrl(sea).unwrap()
    }
}

impl Add {
    pub fn new(left: Node, right: Node, sea: &mut Nodes) -> Self {
        let this = sea.create((Op::Add, vec![None, Some(left), Some(right)]));
        this.to_add(sea).unwrap()
    }
}
impl Sub {
    pub fn new(left: Node, right: Node, sea: &mut Nodes) -> Self {
        let this = sea.create((Op::Sub, vec![None, Some(left), Some(right)]));
        this.to_sub(sea).unwrap()
    }
}
impl Mul {
    pub fn new(left: Node, right: Node, sea: &mut Nodes) -> Self {
        let this = sea.create((Op::Mul, vec![None, Some(left), Some(right)]));
        this.to_mul(sea).unwrap()
    }
}
impl Div {
    pub fn new(left: Node, right: Node, sea: &mut Nodes) -> Self {
        let this = sea.create((Op::Div, vec![None, Some(left), Some(right)]));
        this.to_div(sea).unwrap()
    }
}

impl Minus {
    pub fn new(expr: Node, sea: &mut Nodes) -> Self {
        let this = sea.create((Op::Minus, vec![None, Some(expr)]));
        this.to_minus(sea).unwrap()
    }
}

impl Scope {
    pub fn new(sea: &mut Nodes) -> Self {
        let this = sea.create((Op::Scope(ScopeOp { scopes: vec![] }), vec![]));
        this.to_scope(sea).unwrap()
    }
}

impl Bool {
    pub fn new(left: Node, right: Node, op: BoolOp, sea: &mut Nodes) -> Self {
        let this = sea.create((Op::Bool(op), vec![None, Some(left), Some(right)]));
        this.to_bool(sea).unwrap()
    }
}

impl Not {
    pub fn new(expr: Node, sea: &mut Nodes) -> Self {
        let this = sea.create((Op::Not, vec![None, Some(expr)]));
        this.to_not(sea).unwrap()
    }
}

impl Proj {
    pub fn new<'t, N: Into<Node>>(
        ctrl: N,
        index: usize,
        label: &'t str,
        sea: &mut Nodes<'t>,
    ) -> Self {
        let this = sea.create((Op::Proj(ProjOp { index, label }), vec![Some(ctrl.into())]));
        this.to_proj(sea).unwrap()
    }
}

impl CProj {
    pub fn new<'t, N: Into<Node>>(
        ctrl: N,
        index: usize,
        label: &'t str,
        sea: &mut Nodes<'t>,
    ) -> Self {
        let this = sea.create((Op::CProj(ProjOp { index, label }), vec![Some(ctrl.into())]));
        this.to_cproj(sea).unwrap()
    }
}

impl If {
    pub fn new(ctrl: Node, pred: Option<Node>, sea: &mut Nodes) -> Self {
        let (op, pred) = match pred {
            Some(n) => (IfOp::Cond, n),
            None => (IfOp::Never, *sea.zero),
        };
        let this = sea.create((Op::If(op), vec![Some(ctrl), Some(pred)]));
        sea.iter_peeps.add(this); // Because idoms are complex, just add it
        this.to_if(sea).unwrap()
    }
}

impl Phi {
    pub fn new<'t>(
        label: &'t str,
        ty: Ty<'t>,
        inputs: Vec<Option<Node>>,
        sea: &mut Nodes<'t>,
    ) -> Self {
        let this = sea.create((Op::Phi(PhiOp { label, ty }), inputs));
        this.to_phi(sea).unwrap()
    }
}
impl Region {
    pub fn new(inputs: Vec<Option<Node>>, sea: &mut Nodes) -> Self {
        let this = sea.create((Op::Region, inputs));
        this.to_region(sea).unwrap()
    }
}
impl Stop {
    pub fn new(sea: &mut Nodes) -> Self {
        let this = sea.create((Op::Stop, vec![]));
        this.to_stop(sea).unwrap()
    }
}
impl Loop {
    pub fn new(entry: Node, sea: &mut Nodes) -> Self {
        let this = sea.create((Op::Loop, vec![None, Some(entry), None]));
        this.to_loop(sea).unwrap()
    }
}
impl Cast {
    pub fn new<'t>(ty: Ty<'t>, ctrl: Node, i: Node, sea: &mut Nodes<'t>) -> Self {
        let this = sea.create((Op::Cast(ty), vec![Some(ctrl), Some(i)]));
        this.to_cast(sea).unwrap()
    }
}

impl Load {
    pub fn new<'t>(
        name: &'t str,
        alias: u32,
        declared_type: Ty<'t>,
        [mem_slice, mem_ptr]: [Node; 2],
        sea: &mut Nodes<'t>,
    ) -> Self {
        let this = sea.create((
            Op::Load(LoadOp {
                name,
                alias,
                declared_type,
            }),
            vec![None, Some(mem_slice), Some(mem_ptr)],
        ));
        this.to_load(sea).unwrap()
    }
}
impl Store {
    pub fn new<'t>(
        name: &'t str,
        alias: u32,
        [ctrl, mem_slice, mem_ptr, value]: [Node; 4],
        sea: &mut Nodes<'t>,
    ) -> Self {
        let this = sea.create((
            Op::Store(StoreOp { name, alias }),
            vec![Some(ctrl), Some(mem_slice), Some(mem_ptr), Some(value)],
        ));
        this.to_store(sea).unwrap()
    }
}
impl New {
    pub fn new<'t>(ptr: Ty<'t>, ctrl: Node, sea: &mut Nodes<'t>) -> Self {
        let this = sea.create((Op::New(ptr), vec![Some(ctrl)]));
        this.to_new(sea).unwrap()
    }
}
