use std::borrow::Cow;

use crate::sea_of_nodes::nodes::{NodeCreation, NodeId, ScopeNode};
use crate::sea_of_nodes::types::{Ty, Type};

#[derive(Clone, Debug)]
pub enum Node<'t> {
    Constant(Ty<'t>),
    Return,
    Start { args: Ty<'t> },
    Add,
    Sub,
    Mul,
    Div,
    Minus,
    Scope(ScopeNode),
    Bool(BoolOp),
    Not,
    Proj(ProjNode),
    If,
    Phi(PhiNode),
    Region { cached_idom: Option<NodeId> },
    Loop,
    Stop,
}

#[derive(Copy, Clone, Debug)]
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
pub struct ProjNode {
    pub index: usize,
    pub label: String,
}

#[derive(Clone, Debug)]
pub struct PhiNode {
    pub label: String,
}

impl<'t> Node<'t> {
    /// Easy reading label for debugger
    pub fn label(&self) -> Cow<str> {
        Cow::Borrowed(match self {
            Node::Constant(ty) => return Cow::Owned(format!("#{ty}")),
            Node::Return => "Return",
            Node::Start { .. } => "Start",
            Node::Add => "Add",
            Node::Sub => "Sub",
            Node::Mul => "Mul",
            Node::Div => "Div",
            Node::Minus => "Minus",
            Node::Scope(_) => "Scope",
            Node::Bool(op) => match op {
                BoolOp::EQ => "EQ",
                BoolOp::LT => "LT",
                BoolOp::LE => "LE",
            },
            Node::Not => "Not",
            Node::Proj(p) => return Cow::Owned(p.label.clone()), // clone to keep static lifetime for others
            Node::If => "If",
            Node::Phi(p) => return Cow::Owned(format!("Phi_{}", p.label)),
            Node::Region { .. } => "Region",
            Node::Loop => "Loop",
            Node::Stop => "Stop",
        })
    }

    // Graphical label, e.g. "+" or "Region" or "=="
    pub fn glabel(&self) -> Cow<str> {
        match self {
            Node::Constant(_) => self.label(),
            Node::Return => self.label(),
            Node::Start { .. } => self.label(),
            Node::Add => Cow::Borrowed("+"),
            Node::Sub => Cow::Borrowed("-"),
            Node::Mul => Cow::Borrowed("*"),
            Node::Div => Cow::Borrowed("//"),
            Node::Minus => Cow::Borrowed("-"),
            Node::Scope(_) => self.label(),
            Node::Bool(op) => Cow::Borrowed(op.str()),
            Node::Not => Cow::Borrowed("!"),
            Node::Proj(_) => self.label(),
            Node::If => self.label(),
            Node::Phi(p) => Cow::Owned(format!("&phi;_{}", p.label)),
            Node::Region { .. } => self.label(),
            Node::Loop => self.label(),
            Node::Stop => self.label(),
        }
    }

    pub fn is_multi_node(&self) -> bool {
        match self {
            Node::Start { .. } | Node::If => true,
            Node::Constant(_)
            | Node::Return
            | Node::Add
            | Node::Sub
            | Node::Mul
            | Node::Div
            | Node::Minus
            | Node::Scope(_)
            | Node::Bool(_)
            | Node::Not
            | Node::Proj(_)
            | Node::Phi(_)
            | Node::Region { .. }
            | Node::Loop
            | Node::Stop => false,
        }
    }

    pub fn phi_label(&self) -> Option<&str> {
        if let Node::Phi(p) = self {
            Some(&p.label)
        } else {
            None
        }
    }

    pub fn operation(&self) -> usize {
        match self {
            Node::Constant(_) => 0,
            Node::Return => 1,
            Node::Start { .. } => 2,
            Node::Add => 3,
            Node::Sub => 4,
            Node::Mul => 5,
            Node::Div => 6,
            Node::Minus => 7,
            Node::Scope(_) => 8,
            Node::Bool(BoolOp::EQ) => 9,
            Node::Bool(BoolOp::LT) => 10,
            Node::Bool(BoolOp::LE) => 11,
            Node::Not => 12,
            Node::Proj(_) => 13,
            Node::If => 14,
            Node::Phi(_) => 15,
            Node::Region { .. } => 16,
            Node::Loop => 17,
            Node::Stop => 18,
        }
    }

    pub fn make_start(args: Ty<'t>) -> NodeCreation<'t> {
        debug_assert!(matches!(&*args, Type::Tuple { .. }));
        (Node::Start { args }, vec![])
    }
    pub fn make_return(ctrl: NodeId, data: NodeId) -> NodeCreation<'t> {
        (Node::Return, vec![Some(ctrl), Some(data)])
    }

    pub fn make_constant(start: NodeId, ty: Ty<'t>) -> NodeCreation<'t> {
        (Node::Constant(ty), vec![Some(start)])
    }

    pub fn make_add([left, right]: [NodeId; 2]) -> NodeCreation<'t> {
        (Node::Add, vec![None, Some(left), Some(right)])
    }
    pub fn make_sub([left, right]: [NodeId; 2]) -> NodeCreation<'t> {
        (Node::Sub, vec![None, Some(left), Some(right)])
    }
    pub fn make_mul([left, right]: [NodeId; 2]) -> NodeCreation<'t> {
        (Node::Mul, vec![None, Some(left), Some(right)])
    }
    pub fn make_div([left, right]: [NodeId; 2]) -> NodeCreation<'t> {
        (Node::Div, vec![None, Some(left), Some(right)])
    }

    pub fn make_minus(expr: NodeId) -> NodeCreation<'t> {
        (Node::Minus, vec![None, Some(expr)])
    }

    pub fn make_scope() -> NodeCreation<'t> {
        (Node::Scope(ScopeNode { scopes: vec![] }), vec![])
    }

    pub fn make_bool([left, right]: [NodeId; 2], op: BoolOp) -> NodeCreation<'t> {
        (Node::Bool(op), vec![None, Some(left), Some(right)])
    }

    pub fn make_not(expr: NodeId) -> NodeCreation<'t> {
        (Node::Not, vec![None, Some(expr)])
    }

    pub fn make_proj(ctrl: NodeId, index: usize, label: String) -> NodeCreation<'t> {
        (Node::Proj(ProjNode { index, label }), vec![Some(ctrl)])
    }

    pub fn make_if(ctrl: NodeId, pred: NodeId) -> NodeCreation<'t> {
        (Node::If, vec![Some(ctrl), Some(pred)])
    }

    pub fn make_phi(label: String, inputs: Vec<Option<NodeId>>) -> NodeCreation<'t> {
        (Node::Phi(PhiNode { label }), inputs)
    }

    pub fn make_region(inputs: Vec<Option<NodeId>>) -> NodeCreation<'t> {
        (Node::Region { cached_idom: None }, inputs)
    }

    pub fn make_stop() -> NodeCreation<'t> {
        (Node::Stop, vec![])
    }

    pub fn make_loop(entry: NodeId) -> NodeCreation<'t> {
        (Node::Loop, vec![None, Some(entry), None])
    }
}
