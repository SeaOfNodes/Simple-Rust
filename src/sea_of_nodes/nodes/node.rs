use crate::sea_of_nodes::nodes::index::{
    Add, Bool, Constant, Div, If, Minus, Mul, Not, Proj, Return, Scope, Start, Sub,
};
use crate::sea_of_nodes::nodes::{Node, NodeCreation, Nodes, Op, ScopeOp};
use crate::sea_of_nodes::types::Ty;
use std::borrow::Cow;
use std::collections::HashMap;

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
pub struct MemOp<'t> {
    pub name: &'t str,
    pub alias: u32,
    pub kind: MemOpKind<'t>,
}

#[derive(Clone, Debug)]
pub enum MemOpKind<'t> {
    Load { declared_type: Ty<'t> },
    Store,
}

impl<'t> Op<'t> {
    /// Easy reading label for debugger
    pub fn label(&self) -> Cow<str> {
        Cow::Borrowed(match self {
            Op::Constant(ty) => return Cow::Owned(format!("#{ty}")),
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
            Op::Proj(p) => return Cow::Borrowed(p.label),
            Op::If => "If",
            Op::Phi(p) => return Cow::Owned(format!("Phi_{}", p.label)),
            Op::Region { .. } => "Region",
            Op::Loop => "Loop",
            Op::Stop => "Stop",
            Op::Cast(t) => return Cow::Owned(format!("({})", t.str())),
            Op::New(_) => "new",
            Op::Mem(MemOp {
                kind: MemOpKind::Load { .. },
                ..
            }) => "Load",
            Op::Mem(MemOp {
                kind: MemOpKind::Store,
                ..
            }) => "Store",
        })
    }

    // Graphical label, e.g. "+" or "Region" or "=="
    pub fn glabel(&self) -> Cow<str> {
        match self {
            Op::Constant(_) => self.label(),
            Op::Return => self.label(),
            Op::Start { .. } => self.label(),
            Op::Add => Cow::Borrowed("+"),
            Op::Sub => Cow::Borrowed("-"),
            Op::Mul => Cow::Borrowed("*"),
            Op::Div => Cow::Borrowed("//"),
            Op::Minus => Cow::Borrowed("-"),
            Op::Scope(_) => self.label(),
            Op::Bool(op) => Cow::Borrowed(op.str()),
            Op::Not => Cow::Borrowed("!"),
            Op::Proj(_) => self.label(),
            Op::If => self.label(),
            Op::Phi(p) => Cow::Owned(format!("&phi;_{}", p.label)),
            Op::Region { .. } => self.label(),
            Op::Loop => self.label(),
            Op::Stop => self.label(),
            Op::Cast(t) => Cow::Owned(format!("({})", t.str())),
            Op::New(_) => Cow::Borrowed("new"),
            Op::Mem(m) => match m.kind {
                MemOpKind::Load { .. } => Cow::Borrowed("Load"),
                MemOpKind::Store => Cow::Borrowed("Store"),
            },
        }
    }

    pub fn is_multi_node(&self) -> bool {
        match self {
            Op::Start { .. } | Op::If => true,
            Op::Constant(_)
            | Op::Return
            | Op::Add
            | Op::Sub
            | Op::Mul
            | Op::Div
            | Op::Minus
            | Op::Scope(_)
            | Op::Bool(_)
            | Op::Not
            | Op::Proj(_)
            | Op::Phi(_)
            | Op::Region { .. }
            | Op::Loop
            | Op::Cast(_)
            | Op::New(_)
            | Op::Mem(_)
            | Op::Stop => false,
        }
    }

    pub fn operation(&self) -> usize {
        match self {
            Op::Constant(_) => 0,
            Op::Return => 1,
            Op::Start { .. } => 2,
            Op::Add => 3,
            Op::Sub => 4,
            Op::Mul => 5,
            Op::Div => 6,
            Op::Minus => 7,
            Op::Scope(_) => 8,
            Op::Bool(BoolOp::EQ) => 9,
            Op::Bool(BoolOp::LT) => 10,
            Op::Bool(BoolOp::LE) => 11,
            Op::Not => 12,
            Op::Proj(_) => 13,
            Op::If => 14,
            Op::Phi(_) => 15,
            Op::Region { .. } => 16,
            Op::Loop => 17,
            Op::Stop => 18,
            Op::Cast(_) => 19,
            Op::New(_) => 20,
            Op::Mem(MemOp {
                kind: MemOpKind::Load { .. },
                ..
            }) => 21,
            Op::Mem(MemOp {
                kind: MemOpKind::Store,
                ..
            }) => 22,
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
    pub fn new(ctrl: Node, data: Node, scope: Scope, sea: &mut Nodes) -> Self {
        let this = sea.create((Op::Return, vec![Some(ctrl), Some(data)]));

        // We lookup memory slices by the naming convention that they start with $
        // We could also use implicit knowledge that all memory projects are at offset >= 2
        let names = scope.reverse_names(sea);
        for name in names.into_iter().map(Option::unwrap) {
            if name.starts_with("$") && name != Scope::CTRL {
                let v = scope.lookup(name, sea).unwrap();
                this.add_def(Some(v), sea);
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

impl If {
    pub fn new(ctrl: Node, pred: Node, sea: &mut Nodes) -> Self {
        let this = sea.create((Op::If, vec![Some(ctrl), Some(pred)]));
        sea.iter_peeps.add(this); // Because idoms are complex, just add it
        this.to_if(sea).unwrap()
    }
}

impl<'t> Op<'t> {
    pub fn make_phi(label: &'t str, ty: Ty<'t>, inputs: Vec<Option<Node>>) -> NodeCreation<'t> {
        (Op::Phi(PhiOp { label, ty }), inputs)
    }

    pub fn make_region(inputs: Vec<Option<Node>>) -> NodeCreation<'t> {
        (Op::Region, inputs)
    }

    pub fn make_stop() -> NodeCreation<'t> {
        (Op::Stop, vec![])
    }

    pub fn make_loop(entry: Node) -> NodeCreation<'t> {
        (Op::Loop, vec![None, Some(entry), None])
    }

    pub fn make_cast(ty: Ty<'t>, ctrl: Node, i: Node) -> NodeCreation<'t> {
        (Op::Cast(ty), vec![Some(ctrl), Some(i)])
    }

    pub fn make_load(
        name: &'t str,
        alias: u32,
        declared_type: Ty<'t>,
        [mem_slice, mem_ptr]: [Node; 2],
    ) -> NodeCreation<'t> {
        (
            Op::Mem(MemOp {
                name,
                alias,
                kind: MemOpKind::Load { declared_type },
            }),
            vec![None, Some(mem_slice), Some(mem_ptr), None],
        )
    }

    pub fn make_store(
        name: &'t str,
        alias: u32,
        [mem_slice, mem_ptr, value]: [Node; 3],
    ) -> NodeCreation<'t> {
        (
            Op::Mem(MemOp {
                name,
                alias,
                kind: MemOpKind::Store,
            }),
            vec![None, Some(mem_slice), Some(mem_ptr), Some(value)],
        )
    }

    pub fn make_new(ptr: Ty<'t>, ctrl: Node) -> NodeCreation<'t> {
        (Op::New(ptr), vec![Some(ctrl)])
    }
}
