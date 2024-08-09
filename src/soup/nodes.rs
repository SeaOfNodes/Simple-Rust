use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt;
use std::fmt::{Debug, Display};
use std::num::NonZeroU32;
use std::ops::{Index, IndexMut};

use crate::bit_set;
use crate::bit_set::BitSet;
use crate::id_vec::IdVec;
use crate::soup::types::{Ty, Type};

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct NodeId(NonZeroU32);

impl NodeId {
    pub const DUMMY: NodeId = NodeId(NonZeroU32::MAX);

    pub fn index(self) -> usize {
        self.0.get() as usize
    }
}

impl bit_set::Index for NodeId {
    fn index(&self) -> usize {
        NodeId::index(*self)
    }
}

impl Display for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0.get(), f)
    }
}
impl Debug for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.0.get(), f)
    }
}

/// Using `IdVec` has two advantages over `Vec`+helper methods:
/// 1) `self.inputs[x]` and `self.outputs[x]` can be borrowed simultaneously
///    while `self.inputs(x)` and `self.outputs_mut(x)` can't
/// 2) methods like `self.inputs(x)` and `self.inputs_mut(x)` require two versions for mutability
///    while `self.inputs[x]` automatically decides
pub struct Nodes<'t> {
    /// indexed by self[id]
    nodes: Vec<Node<'t>>,

    pub ty: IdVec<NodeId, Option<Ty<'t>>>,

    /// Inputs to the node. These are use-def references to Nodes.
    ///
    /// Generally fixed length, ordered, nulls allowed, no unused
    /// trailing space. Ordering is required because e.g. "a/ b"
    /// is different from "b/ a". The first input (offset 0) is
    /// often a isCFG node.
    pub inputs: IdVec<NodeId, Vec<Option<NodeId>>>,

    /// Outputs reference Nodes that are not null and have this Node
    /// as an input. These nodes are users of this node, thus these
    /// are def-use references to Nodes.
    ///
    /// Outputs directly match inputs, making a directed graph that
    /// can be walked in either direction. These outputs are typically
    /// used for efficient optimizations but otherwise have no semantics
    /// meaning
    pub outputs: IdVec<NodeId, Vec<NodeId>>,
}

pub type NodeCreation<'t> = (Node<'t>, Vec<Option<NodeId>>);

impl<'t> Nodes<'t> {
    pub fn new() -> Self {
        let dummy = Node::Stop;
        Nodes {
            nodes: vec![dummy],
            inputs: IdVec::new(vec![vec![]]),
            outputs: IdVec::new(vec![vec![]]),
            ty: IdVec::new(vec![None]),
        }
    }
    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    pub fn create(&mut self, (node, inputs): NodeCreation<'t>) -> NodeId {
        let id = u32::try_from(self.nodes.len())
            .and_then(NonZeroU32::try_from)
            .map(NodeId)
            .unwrap();
        self.nodes.push(node);
        self.inputs.push(inputs);
        self.outputs.push(vec![]);
        self.ty.push(None);
        for i in 0..self.inputs[id].len() {
            if let Some(input) = self.inputs[id][i] {
                self.add_use(input, id);
            }
        }

        debug_assert_eq!(self.len(), self.inputs.len());
        debug_assert_eq!(self.len(), self.outputs.len());
        debug_assert_eq!(self.len(), self.ty.len());
        id
    }

    pub fn is_dead(&self, node: NodeId) -> bool {
        self.is_unused(node) && self.inputs[node].is_empty() && self.ty[node].is_none()
    }

    pub fn pop_n(&mut self, node: NodeId, n: usize) {
        for _ in 0..n {
            let old_def = self.inputs[node].pop().unwrap();
            if let Some(old_def) = old_def {
                self.del_use(old_def, node);
                if self.is_unused(old_def) {
                    self.kill(old_def);
                }
            }
        }
    }

    pub fn kill(&mut self, node: NodeId) {
        debug_assert!(self.is_unused(node));
        self.pop_n(node, self.inputs[node].len());
        self.inputs[node] = vec![]; // deallocate
        self.ty[node] = None; // flag as dead
        debug_assert!(self.is_dead(node));
    }

    pub fn set_def(&mut self, this: NodeId, index: usize, new_def: Option<NodeId>) {
        let old_def = self.inputs[this][index];
        if old_def == new_def {
            return;
        }

        if let Some(new_def) = new_def {
            self.add_use(new_def, this);
        }

        if let Some(old_def) = old_def {
            self.del_use(old_def, this);
            if self.is_unused(old_def) {
                self.kill(old_def);
            }
        }

        self.inputs[this][index] = new_def;
    }

    pub fn add_def(&mut self, node: NodeId, new_def: Option<NodeId>) {
        self.inputs[node].push(new_def);
        if let Some(new_def) = new_def {
            self.add_use(new_def, node);
        }
    }

    pub fn add_use(&mut self, node: NodeId, use_: NodeId) {
        self.outputs[node].push(use_)
    }

    pub fn del_use(&mut self, node: NodeId, use_: NodeId) {
        if let Some(pos) = self.outputs[node].iter().rposition(|n| *n == use_) {
            self.outputs[node].swap_remove(pos);
        }
    }

    pub fn is_unused(&self, node: NodeId) -> bool {
        self.outputs[node].is_empty()
    }

    pub fn swap_12(&mut self, node: NodeId) -> NodeId {
        self.inputs[node].swap(1, 2);
        node
    }
    pub fn keep(&mut self, node: NodeId) {
        self.add_use(node, NodeId::DUMMY);
    }
    pub fn unkeep(&mut self, node: NodeId) {
        self.del_use(node, NodeId::DUMMY);
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
    Region,
    Stop,
}

impl<'t> Node<'t> {
    /// Easy reading label for debugger
    pub fn label(&self) -> Cow<str> {
        Cow::Borrowed(match self {
            Node::Constant(ty) => return Cow::Owned(format!("{ty}")),
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
            Node::Region => "Region",
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
            Node::Region => self.label(),
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
            | Node::Region
            | Node::Stop => false,
        }
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
        PrintNodes { node, nodes: self }
    }

    fn fmt(
        &self,
        node: Option<NodeId>,
        f: &mut fmt::Formatter,
        visited: &mut BitSet<NodeId>,
    ) -> fmt::Result {
        let Some(node) = node else {
            return write!(f, "<?>");
        };
        visited.add(node);
        let inputs = &self.inputs[node];
        match &self[node] {
            _ if self.is_dead(node) => write!(f, "{}:DEAD", self.unique_name(node)),
            Node::Return => {
                write!(f, "return ")?;
                self.fmt(inputs[1], f, visited)?;
                write!(f, ";")
            }
            Node::Add => {
                write!(f, "(")?;
                self.fmt(inputs[1], f, visited)?;
                write!(f, "+")?;
                self.fmt(inputs[2], f, visited)?;
                write!(f, ")")
            }
            Node::Constant(ty) => write!(f, "{}", ty),
            n @ Node::Start { .. } => write!(f, "{}", n.label()),
            Node::Sub => {
                write!(f, "(")?;
                self.fmt(inputs[1], f, visited)?;
                write!(f, "-")?;
                self.fmt(inputs[2], f, visited)?;
                write!(f, ")")
            }
            Node::Mul => {
                write!(f, "(")?;
                self.fmt(inputs[1], f, visited)?;
                write!(f, "*")?;
                self.fmt(inputs[2], f, visited)?;
                write!(f, ")")
            }
            Node::Div => {
                write!(f, "(")?;
                self.fmt(inputs[1], f, visited)?;
                write!(f, "*")?;
                self.fmt(inputs[2], f, visited)?;
                write!(f, ")")
            }
            Node::Minus => {
                write!(f, "(-")?;
                self.fmt(inputs[1], f, visited)?;
                write!(f, ")")
            }
            n @ Node::Scope(scope) => {
                write!(f, "{}", n.label())?;
                for s in &scope.scopes {
                    write!(f, "[")?;
                    for (i, (name, input)) in s.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{name}:")?;
                        self.fmt(inputs[*input], f, visited)?;
                    }
                    write!(f, "]")?;
                }
                Ok(())
            }
            Node::Bool(op) => {
                write!(f, "(")?;
                self.fmt(inputs[1], f, visited)?;
                write!(f, "{}", op.str())?;
                self.fmt(inputs[2], f, visited)?;
                write!(f, ")")
            }
            Node::Not => {
                write!(f, "(!")?;
                self.fmt(inputs[1], f, visited)?;
                write!(f, ")")
            }
            Node::Proj(proj) => {
                write!(f, "{}", proj.label)
            }
            Node::If => {
                write!(f, "if( ")?;
                self.fmt(inputs[1], f, visited)?;
                write!(f, " )")
            }
            Node::Phi(_) => {
                write!(f, "Phi(")?;
                for (i, input) in inputs.iter().enumerate() {
                    if i > 0 {
                        write!(f, ",")?;
                    }
                    self.fmt(*input, f, visited)?;
                }
                write!(f, ")")
            }
            n @ Node::Region => {
                write!(f, "{}{}", n.label(), node.index())
            }
            Node::Stop => {
                if let Some(ret) = self.unique_input(node) {
                    self.fmt(Some(ret), f, visited)
                } else {
                    write!(f, "Stop[ ")?;
                    for ret in inputs {
                        self.fmt(*ret, f, visited)?;
                        write!(f, " ")?;
                    }
                    write!(f, "]")
                }
            }
        }
    }

    // Unique label for graph visualization, e.g. "Add12" or "Region30" or "EQ99"
    pub fn unique_name(&self, node: NodeId) -> String {
        match &self[node] {
            Node::Constant(_) => format!("Con_{}", node),
            _ => format!("{}{}", self[node].label(), node),
        }
    }

    pub fn is_cfg(&self, node: NodeId) -> bool {
        match &self[node] {
            Node::Start { .. } | Node::Return | Node::Stop => true,
            Node::If | Node::Region => true,
            Node::Proj(p) => {
                p.index == 0 || self.inputs[node][0].is_some_and(|n| matches!(&self[n], Node::If))
            }
            Node::Constant(_)
            | Node::Add
            | Node::Sub
            | Node::Mul
            | Node::Div
            | Node::Minus
            | Node::Scope(_)
            | Node::Bool(_)
            | Node::Phi(_)
            | Node::Not => false,
        }
    }

    pub fn unique_input(&self, stop: NodeId) -> Option<NodeId> {
        if self.inputs[stop].len() == 1 {
            self.inputs[stop][0]
        } else {
            None // ambiguous
        }
    }

    fn scope_mut(&mut self, scope_node: NodeId) -> &mut ScopeNode {
        let Node::Scope(scope) = &mut self[scope_node] else {
            panic!("Must be called with a scope node id")
        };
        scope
    }
    pub fn scope_push(&mut self, scope_node: NodeId) {
        self.scope_mut(scope_node).scopes.push(HashMap::new());
    }

    pub fn scope_pop(&mut self, scope_node: NodeId) {
        let last = self.scope_mut(scope_node).scopes.pop().unwrap();
        self.pop_n(scope_node, last.len());
    }

    pub fn scope_define(
        &mut self,
        scope_node: NodeId,
        name: String,
        value: NodeId,
    ) -> Result<(), ()> {
        let len = self.inputs[scope_node].len();
        let scope = self.scope_mut(scope_node);
        let syms = scope.scopes.last_mut().unwrap();
        if let Some(_old) = syms.insert(name, len) {
            return Err(());
        }
        self.add_def(scope_node, Some(value));
        Ok(())
    }

    pub fn scope_lookup(&mut self, scope_node: NodeId, name: &str) -> Result<NodeId, ()> {
        let nesting_level = self.scope_mut(scope_node).scopes.len() - 1;
        self.scope_lookup_update(scope_node, name, None, nesting_level)
            .ok_or(())
    }

    pub fn scope_update(
        &mut self,
        scope_node: NodeId,
        name: &str,
        value: NodeId,
    ) -> Result<NodeId, ()> {
        let nesting_level = self.scope_mut(scope_node).scopes.len() - 1;
        self.scope_lookup_update(scope_node, name, Some(value), nesting_level)
            .ok_or(())
    }

    fn scope_lookup_update(
        &mut self,
        scope_node: NodeId,
        name: &str,
        value: Option<NodeId>,
        nesting_level: usize,
    ) -> Option<NodeId> {
        let scope = self.scope_mut(scope_node);
        let syms = &mut scope.scopes[nesting_level];
        if let Some(index) = syms.get(name).copied() {
            let old = self.inputs[scope_node][index];
            if value.is_some() {
                self.set_def(scope_node, index, value);
                value
            } else {
                old
            }
        } else if nesting_level > 0 {
            self.scope_lookup_update(scope_node, name, value, nesting_level - 1)
        } else {
            None
        }
    }

    pub fn shallow_copy(&mut self, node: NodeId, ops: [NodeId; 2]) -> NodeId {
        match &self[node] {
            Node::Constant(_)
            | Node::Return
            | Node::Start { .. }
            | Node::Minus
            | Node::Scope(_)
            | Node::Not
            | Node::If
            | Node::Phi(_)
            | Node::Region
            | Node::Stop
            | Node::Proj(_) => unreachable!(),
            Node::Add => self.create(Node::make_add(ops)),
            Node::Sub => self.create(Node::make_sub(ops)),
            Node::Mul => self.create(Node::make_mul(ops)),
            Node::Div => self.create(Node::make_div(ops)),
            Node::Bool(op) => self.create(Node::make_bool(ops, *op)),
        }
    }
}

impl<'t> Node<'t> {
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
        (Node::Region, inputs)
    }

    pub fn make_stop() -> NodeCreation<'t> {
        (Node::Stop, vec![])
    }
}

pub struct ScopeNode {
    pub scopes: Vec<HashMap<String, usize>>,
}

impl ScopeNode {
    pub const CTRL: &'static str = "$ctrl";
    pub const ARG0: &'static str = "arg";
}

#[derive(Copy, Clone)]
pub enum BoolOp {
    EQ,
    LT,
    LE,
}
impl BoolOp {
    fn str(&self) -> &'static str {
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

pub struct ProjNode {
    pub index: usize,
    pub label: String,
}

pub struct PhiNode {
    pub label: String,
}
