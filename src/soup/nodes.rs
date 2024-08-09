use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt;
use std::fmt::{Debug, Display};
use std::num::NonZeroU32;
use std::ops::{Index, IndexMut};

use crate::bit_set;
use crate::bit_set::BitSet;
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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0.get(), f)
    }
}
impl Debug for NodeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.0.get(), f)
    }
}

pub struct Nodes<'t> {
    nodes: Vec<Node<'t>>,
    node_ty: Vec<Option<Ty<'t>>>,
}

impl<'t> Nodes<'t> {
    pub fn new() -> Self {
        let dummy = Node::Not(NotNode {
            base: NodeBase {
                id: NodeId::DUMMY,
                inputs: vec![],
                outputs: vec![],
            },
        });
        Nodes {
            nodes: vec![dummy],
            node_ty: vec![None],
        }
    }
    pub fn len(&self) -> usize {
        self.nodes.len()
    }
    pub fn create<F: FnOnce(NodeId) -> Node<'t>>(&mut self, f: F) -> NodeId {
        let id = u32::try_from(self.nodes.len())
            .and_then(NonZeroU32::try_from)
            .map(NodeId)
            .unwrap();
        self.nodes.push(f(id));
        self.node_ty.push(None);
        for i in 0..self[id].base().inputs.len() {
            if let Some(input) = self[id].base().inputs[i] {
                self[input].base_mut().add_use(id);
            }
        }

        debug_assert_eq!(self[id].id(), id);
        debug_assert_eq!(self.nodes.len(), self.node_ty.len());
        id
    }

    pub fn ty(&self, node: NodeId) -> Option<Ty<'t>> {
        self.node_ty[node.index()]
    }
    pub fn get_ty_mut(&mut self, node: NodeId) -> &mut Option<Ty<'t>> {
        &mut self.node_ty[node.index()]
    }

    pub fn is_dead(&self, node: NodeId) -> bool {
        self[node].is_unused() && self[node].base().inputs.is_empty() && self.ty(node).is_none()
    }

    pub fn pop_n(&mut self, node: NodeId, n: usize) {
        for _ in 0..n {
            let old_def = self[node].base_mut().inputs.pop().unwrap();
            if let Some(old_def) = old_def {
                self[old_def].base_mut().del_use(node);
                if self[old_def].base_mut().is_unused() {
                    self.kill(old_def);
                }
            }
        }
    }

    pub fn kill(&mut self, node: NodeId) {
        debug_assert!(self[node].is_unused());
        self.pop_n(node, self[node].base().inputs.len());
        self[node].base_mut().inputs = vec![]; // deallocate
        *self.get_ty_mut(node) = None; // flag as dead
        debug_assert!(self.is_dead(node));
    }

    pub fn set_def(&mut self, this: NodeId, index: usize, new_def: Option<NodeId>) {
        let old_def = self[this].base().inputs[index];
        if old_def == new_def {
            return;
        }

        if let Some(new_def) = new_def {
            self[new_def].base_mut().add_use(this);
        }

        if let Some(old_def) = old_def {
            self[old_def].base_mut().del_use(this);
            if self[old_def].base_mut().is_unused() {
                self.kill(old_def);
            }
        }

        self[this].base_mut().inputs[index] = new_def;
    }

    fn add_def(&mut self, node: NodeId, new_def: Option<NodeId>) {
        self[node].base_mut().inputs.push(new_def);
        if let Some(new_def) = new_def {
            self[new_def].base_mut().add_use(node);
        }
    }

    pub fn swap_12(&mut self, node: NodeId) -> NodeId {
        self[node].base_mut().inputs.swap(1, 2);
        node
    }
    pub fn keep(&mut self, node: NodeId) {
        self[node].base_mut().add_use(NodeId::DUMMY);
    }
    pub fn unkeep(&mut self, node: NodeId) {
        self[node].base_mut().del_use(NodeId::DUMMY);
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

pub struct NodeBase {
    id: NodeId,

    /// Inputs to the node. These are use-def references to Nodes.
    ///
    /// Generally fixed length, ordered, nulls allowed, no unused
    /// trailing space. Ordering is required because e. g. "a/ b"
    /// is different from "b/ a". The first input (offset 0) is
    /// often a isCFG node.
    pub inputs: Vec<Option<NodeId>>,

    /// Outputs reference Nodes that are not null and have this Node
    /// as an input. These nodes are users of this node, thus these
    /// are def-use references to Nodes.
    ///
    /// Outputs directly match inputs, making a directed graph that
    /// can be walked in either direction. These outputs are typically
    /// used for efficient optimizations but otherwise have no semantics
    /// meaning
    pub outputs: Vec<NodeId>,
}

pub enum Node<'t> {
    Constant(ConstantNode<'t>),
    Return(ReturnNode),
    Start(StartNode<'t>),
    Add(AddNode),
    Sub(SubNode),
    Mul(MulNode),
    Div(DivNode),
    Minus(MinusNode),
    Scope(ScopeNode),
    Bool(BoolNode),
    Not(NotNode),
    Proj(ProjNode),
}

pub struct ConstantNode<'t> {
    pub base: NodeBase,
    ty: Ty<'t>,
}

pub struct ReturnNode {
    pub base: NodeBase,
}

pub struct StartNode<'t> {
    pub base: NodeBase,
    pub args: Ty<'t>,
}

impl<'t> Node<'t> {
    pub fn id(&self) -> NodeId {
        self.base().id
    }
    pub fn base(&self) -> &NodeBase {
        match self {
            Node::Constant(n) => &n.base,
            Node::Return(n) => &n.base,
            Node::Start(n) => &n.base,
            Node::Add(n) => &n.base,
            Node::Sub(n) => &n.base,
            Node::Mul(n) => &n.base,
            Node::Div(n) => &n.base,
            Node::Minus(n) => &n.base,
            Node::Scope(n) => &n.base,
            Node::Bool(n) => &n.base,
            Node::Not(n) => &n.base,
            Node::Proj(n) => &n.base,
        }
    }

    pub fn base_mut(&mut self) -> &mut NodeBase {
        match self {
            Node::Constant(n) => &mut n.base,
            Node::Return(n) => &mut n.base,
            Node::Start(n) => &mut n.base,
            Node::Add(n) => &mut n.base,
            Node::Sub(n) => &mut n.base,
            Node::Mul(n) => &mut n.base,
            Node::Div(n) => &mut n.base,
            Node::Minus(n) => &mut n.base,
            Node::Scope(n) => &mut n.base,
            Node::Bool(n) => &mut n.base,
            Node::Not(n) => &mut n.base,
            Node::Proj(n) => &mut n.base,
        }
    }

    pub fn is_cfg(&self) -> bool {
        match self {
            Node::Start(_) | Node::Return(_) => true,
            Node::Proj(p) => p.index == 0,
            Node::Constant(_)
            | Node::Add(_)
            | Node::Sub(_)
            | Node::Mul(_)
            | Node::Div(_)
            | Node::Minus(_)
            | Node::Scope(_)
            | Node::Bool(_)
            | Node::Not(_) => false,
        }
    }

    /// Easy reading label for debugger
    pub fn label(&self) -> Cow<str> {
        Cow::Borrowed(match self {
            Node::Constant(c) => return Cow::Owned(format!("{}", c.ty())),
            Node::Return(_) => "Return",
            Node::Start(_) => "Start",
            Node::Add(_) => "Add",
            Node::Sub(_) => "Sub",
            Node::Mul(_) => "Mul",
            Node::Div(_) => "Div",
            Node::Minus(_) => "Minus",
            Node::Scope(_) => "Scope",
            Node::Bool(b) => match b.op {
                BoolOp::EQ => "EQ",
                BoolOp::LT => "LT",
                BoolOp::LE => "LE",
            },
            Node::Not(_) => "Not",
            Node::Proj(p) => return Cow::Owned(p.label.clone()), // clone to keep static lifetime for others
        })
    }

    // Unique label for graph visualization, e.g. "Add12" or "Region30" or "EQ99"
    pub fn unique_name(&self) -> String {
        match self {
            Node::Constant(_) => format!("Con_{}", self.base().id),
            _ => format!("{}{}", self.label(), self.base().id),
        }
    }

    // Graphical label, e.g. "+" or "Region" or "=="
    pub fn glabel(&self) -> Cow<str> {
        match self {
            Node::Constant(_) => self.label(),
            Node::Return(_) => self.label(),
            Node::Start(_) => self.label(),
            Node::Add(_) => Cow::Borrowed("+"),
            Node::Sub(_) => Cow::Borrowed("-"),
            Node::Mul(_) => Cow::Borrowed("*"),
            Node::Div(_) => Cow::Borrowed("//"),
            Node::Minus(_) => Cow::Borrowed("-"),
            Node::Scope(_) => self.label(),
            Node::Bool(b) => Cow::Borrowed(b.op.str()),
            Node::Not(_) => Cow::Borrowed("!"),
            Node::Proj(_) => self.label(),
        }
    }

    pub fn is_unused(&self) -> bool {
        self.base().is_unused()
    }

    pub fn is_multi_node(&self) -> bool {
        match self {
            Node::Start(_) => true,
            Node::Constant(_)
            | Node::Return(_)
            | Node::Add(_)
            | Node::Sub(_)
            | Node::Mul(_)
            | Node::Div(_)
            | Node::Minus(_)
            | Node::Scope(_)
            | Node::Bool(_)
            | Node::Not(_)
            | Node::Proj(_) => false,
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
        match &self[node] {
            n if self.is_dead(node) => write!(f, "{}:DEAD", n.unique_name()),
            Node::Return(r) => {
                write!(f, "return ")?;
                self.fmt(r.expr(), f, visited)?;
                write!(f, ";")
            }
            Node::Add(add) => {
                write!(f, "(")?;
                self.fmt(add.base.inputs[1], f, visited)?;
                write!(f, "+")?;
                self.fmt(add.base.inputs[2], f, visited)?;
                write!(f, ")")
            }
            Node::Constant(c) => write!(f, "{}", c.ty()),
            n @ Node::Start(_) => write!(f, "{}", n.label()),
            Node::Sub(sub) => {
                write!(f, "(")?;
                self.fmt(sub.base.inputs[1], f, visited)?;
                write!(f, "-")?;
                self.fmt(sub.base.inputs[2], f, visited)?;
                write!(f, ")")
            }
            Node::Mul(mul) => {
                write!(f, "(")?;
                self.fmt(mul.base.inputs[1], f, visited)?;
                write!(f, "*")?;
                self.fmt(mul.base.inputs[2], f, visited)?;
                write!(f, ")")
            }
            Node::Div(div) => {
                write!(f, "(")?;
                self.fmt(div.base.inputs[1], f, visited)?;
                write!(f, "*")?;
                self.fmt(div.base.inputs[2], f, visited)?;
                write!(f, ")")
            }
            Node::Minus(minus) => {
                write!(f, "(-")?;
                self.fmt(minus.base.inputs[1], f, visited)?;
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
                        self.fmt(scope.base.inputs[*input], f, visited)?;
                    }
                    write!(f, "]")?;
                }
                Ok(())
            }
            Node::Bool(bool) => {
                write!(f, "(")?;
                self.fmt(bool.base.inputs[1], f, visited)?;
                write!(f, "{}", bool.op.str())?;
                self.fmt(bool.base.inputs[2], f, visited)?;
                write!(f, ")")
            }
            Node::Not(not) => {
                write!(f, "(!")?;
                self.fmt(not.base.inputs[1], f, visited)?;
                write!(f, ")")
            }
            Node::Proj(proj) => {
                write!(f, "{}", proj.label)
            }
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
        let scope = self.scope_mut(scope_node);
        let syms = scope.scopes.last_mut().unwrap();
        if let Some(_old) = syms.insert(name, scope.base.inputs.len()) {
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
            let old = scope.base.inputs[index];
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
            | Node::Return(_)
            | Node::Start(_)
            | Node::Minus(_)
            | Node::Scope(_)
            | Node::Not(_)
            | Node::Proj(_) => unreachable!(),
            Node::Add(n) => self.create(|id| Node::Add(AddNode::new(id, ops))),
            Node::Sub(n) => self.create(|id| Node::Sub(SubNode::new(id, ops))),
            Node::Mul(n) => self.create(|id| Node::Mul(MulNode::new(id, ops))),
            Node::Div(n) => self.create(|id| Node::Div(DivNode::new(id, ops))),
            Node::Bool(n) => {
                let op = n.op;
                self.create(|id| Node::Bool(BoolNode::new(id, ops, op)))
            }
        }
    }
}

impl NodeBase {
    fn new(id: NodeId, inputs: Vec<Option<NodeId>>) -> Self {
        Self {
            id,
            inputs,
            outputs: vec![],
        }
    }

    fn add_use(&mut self, node: NodeId) {
        self.outputs.push(node)
    }

    fn del_use(&mut self, node: NodeId) {
        if let Some(pos) = self.outputs.iter().rposition(|n| *n == node) {
            self.outputs.swap_remove(pos);
        }
    }

    fn is_unused(&self) -> bool {
        self.outputs.is_empty()
    }
}

impl<'t> StartNode<'t> {
    pub fn new(id: NodeId, args: Ty<'t>) -> Self {
        debug_assert!(matches!(&*args, Type::Tuple { .. }));
        Self {
            base: NodeBase::new(id, vec![]),
            args,
        }
    }
}

impl ReturnNode {
    pub fn new(id: NodeId, ctrl: NodeId, data: NodeId) -> Self {
        Self {
            base: NodeBase::new(id, vec![Some(ctrl), Some(data)]),
        }
    }

    pub fn ctrl(&self) -> Option<NodeId> {
        return self.base.inputs[0];
    }

    pub fn expr(&self) -> Option<NodeId> {
        return self.base.inputs[1];
    }
}

impl<'t> ConstantNode<'t> {
    pub fn new(id: NodeId, start: NodeId, ty: Ty<'t>) -> Self {
        Self {
            base: NodeBase::new(id, vec![Some(start)]),
            ty,
        }
    }

    pub fn start(&self) -> Option<NodeId> {
        self.base.inputs[0]
    }

    pub fn ty(&self) -> Ty<'t> {
        self.ty
    }

    pub fn is_constant(&self) -> bool {
        true
    }
}

pub struct AddNode {
    pub base: NodeBase,
}

impl AddNode {
    pub fn new(id: NodeId, [left, right]: [NodeId; 2]) -> Self {
        Self {
            base: NodeBase::new(id, vec![None, Some(left), Some(right)]),
        }
    }
}

pub struct SubNode {
    pub base: NodeBase,
}

impl SubNode {
    pub fn new(id: NodeId, [left, right]: [NodeId; 2]) -> Self {
        Self {
            base: NodeBase::new(id, vec![None, Some(left), Some(right)]),
        }
    }
}

pub struct MulNode {
    pub base: NodeBase,
}

impl MulNode {
    pub fn new(id: NodeId, [left, right]: [NodeId; 2]) -> Self {
        Self {
            base: NodeBase::new(id, vec![None, Some(left), Some(right)]),
        }
    }
}

pub struct DivNode {
    pub base: NodeBase,
}

impl DivNode {
    pub fn new(id: NodeId, [left, right]: [NodeId; 2]) -> Self {
        Self {
            base: NodeBase::new(id, vec![None, Some(left), Some(right)]),
        }
    }
}

pub struct MinusNode {
    pub base: NodeBase,
}

impl MinusNode {
    pub fn new(id: NodeId, expr: NodeId) -> Self {
        Self {
            base: NodeBase::new(id, vec![None, Some(expr)]),
        }
    }
}

pub struct ScopeNode {
    pub base: NodeBase,
    pub scopes: Vec<HashMap<String, usize>>,
}

impl ScopeNode {
    pub const CTRL: &'static str = "$ctrl";
    pub const ARG0: &'static str = "arg";

    pub fn new(id: NodeId) -> Self {
        Self {
            base: NodeBase::new(id, vec![]),
            scopes: vec![],
        }
    }
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

pub struct BoolNode {
    pub base: NodeBase,
    pub op: BoolOp,
}

impl BoolNode {
    pub fn new(id: NodeId, [left, right]: [NodeId; 2], op: BoolOp) -> Self {
        Self {
            base: NodeBase::new(id, vec![None, Some(left), Some(right)]),
            op,
        }
    }
}

pub struct NotNode {
    pub base: NodeBase,
}

impl NotNode {
    pub fn new(id: NodeId, input: NodeId) -> Self {
        Self {
            base: NodeBase::new(id, vec![None, Some(input)]),
        }
    }
}

pub struct ProjNode {
    pub base: NodeBase,
    pub index: usize,
    pub label: String,
}

impl ProjNode {
    pub fn new(id: NodeId, ctrl: NodeId, index: usize, label: String) -> Self {
        Self {
            base: NodeBase::new(id, vec![Some(ctrl)]),
            index,
            label,
        }
    }
}
