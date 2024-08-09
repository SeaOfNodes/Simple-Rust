use std::mem;

use crate::soup::graph_visualizer;
use crate::soup::nodes::{
    AddNode, BoolOp, ConstantNode, DivNode, MinusNode, MulNode, Node, NodeBase, NodeId, Nodes,
    ReturnNode, ScopeNode, StartNode, SubNode,
};
use crate::soup::types::{Int, Ty, Type, Types};
use crate::syntax::ast::{BinaryOperator, Block, Expression, Function, PrefixOperator, Statement};
use crate::syntax::formatter::{CodeStyle, FormatCode};

pub struct Soup<'t> {
    pub nodes: Nodes<'t>,
    pub(crate) start: NodeId,
    ctrl: NodeId,
    pub(crate) scope: NodeId,
    pub disable_peephole: bool,
    errors: Vec<String>,
    arg: Option<Ty<'t>>,
}

impl<'t> Soup<'t> {
    pub fn new() -> Self {
        Self {
            nodes: Nodes::new(),
            start: NodeId::DUMMY,
            ctrl: NodeId::DUMMY,
            scope: NodeId::DUMMY,
            disable_peephole: false,
            errors: vec![],
            arg: None,
        }
    }

    pub fn set_arg(&mut self, ty: Ty<'t>) {
        self.arg = Some(ty)
    }

    pub fn compile_function(
        &mut self,
        function: &Function,
        types: &mut Types<'t>,
    ) -> Result<(NodeId, NodeId), Vec<String>> {
        let start = self.create_peepholed(types, |id| Node::Start(StartNode::new(id)));
        self.start = start;
        self.ctrl = start;
        // if we didn't call peephole we might have to manually set the computed type like in the java constructor
        self.scope = self.create_peepholed(types, |id| Node::Scope(ScopeNode::new(id)));

        let stop = match &function.body {
            None => todo!(),
            Some(body) => self.compile_block(&body, types),
        };

        if self.errors.is_empty() {
            Ok((start, stop))
        } else {
            Err(mem::take(&mut self.errors))
        }
    }

    fn compile_block(&mut self, block: &Block, types: &mut Types<'t>) -> NodeId {
        let mut result = None;
        let mut saw_return = false;
        let mut reported_unreachable = false;

        self.nodes.scope_push(self.scope);
        for statement in &block.statements {
            if saw_return && !matches!(statement, Statement::Meta(_)) && !reported_unreachable {
                let s = statement.format(&CodeStyle::DEFAULT);
                self.errors
                    .push(format!("Unreachable statement after return: {s}"));
                reported_unreachable = true;
            }
            match statement {
                Statement::Expression(e) => result = Some(self.compile_expression(e, types)),
                Statement::Return(ret) => {
                    let data = self.compile_expression(&ret.value, types);
                    let ctrl = self.ctrl;
                    saw_return = true;
                    result = Some(self.create_peepholed(types, |id| {
                        Node::Return(ReturnNode::new(id, ctrl, data))
                    }));
                }
                Statement::If(_) => todo!(),
                Statement::Var(var) => {
                    let value = self.compile_expression(&var.expression, types);
                    if let Err(()) =
                        self.nodes
                            .scope_define(self.scope, var.name.value.clone(), value)
                    {
                        self.errors.push(format!(
                            "{:?}: Variable {} is already defined in scope",
                            var.name.location, &var.name.value
                        ));
                    }
                    result = Some(value);
                }
                Statement::Meta(ident) => {
                    if ident.value == "show_graph" {
                        println!("{}", graph_visualizer::generate_dot_output(self).unwrap());
                    }
                }
            }
        }
        self.nodes.scope_pop(self.scope);

        result.unwrap_or_else(|| self.create_unit(types))
    }

    fn create_unit(&mut self, types: &mut Types<'t>) -> NodeId {
        let ty = types.ty_zero; // TODO return Unit or something like that
        let start = self.start;
        self.create_peepholed(types, |id| Node::Constant(ConstantNode::new(id, start, ty)))
    }

    fn compile_expression(&mut self, expression: &Expression, types: &mut Types<'t>) -> NodeId {
        match expression {
            Expression::Immediate(immediate) => {
                let ty = types.get_int(*immediate);
                let start = self.start;
                self.create_peepholed(types, |id| Node::Constant(ConstantNode::new(id, start, ty)))
            }
            Expression::Identifier(identifier) => {
                match self.nodes.scope_lookup(self.scope, &identifier.value) {
                    Ok(value) => value,
                    Err(()) => {
                        self.errors.push(format!(
                            "{:?}: Variable {} not in scope",
                            identifier.location, &identifier.value
                        ));
                        self.create_unit(types)
                    }
                }
            }
            Expression::Binary {
                operator: BinaryOperator::Assign,
                location: _,
                left,
                right,
            } => {
                let Expression::Identifier(left) = left.as_ref() else {
                    todo!()
                };
                let right = self.compile_expression(right, types);

                self.nodes
                    .scope_update(self.scope, &left.value, right)
                    .unwrap_or_else(|()| {
                        self.errors.push(format!(
                            "{:?}: Variable {} not in scope",
                            left.location, &left.value
                        ));
                        self.create_unit(types)
                    })
            }
            Expression::Binary {
                operator,
                location: _,
                left,
                right,
            } => {
                let left_node = self.compile_expression(left, types);
                let right_node = self.compile_expression(right, types);
                self.create_peepholed(types, |id| match operator {
                    BinaryOperator::Plus => Node::Add(AddNode::new(id, [left_node, right_node])),
                    BinaryOperator::Minus => Node::Sub(SubNode::new(id, [left_node, right_node])),
                    BinaryOperator::Multiply => {
                        Node::Mul(MulNode::new(id, [left_node, right_node]))
                    }
                    BinaryOperator::Divide => Node::Div(DivNode::new(id, [left_node, right_node])),
                    _ => todo!(),
                })
            }
            Expression::Prefix { operator, operand } => {
                let operand = self.compile_expression(operand, types);
                self.create_peepholed(types, |id| match operator {
                    PrefixOperator::Minus => Node::Minus(MinusNode::new(id, operand)),
                    _ => todo! {},
                })
            }
            Expression::Parenthesized(inner) => self.compile_expression(inner, types),
            Expression::Block(block) => self.compile_block(block, types),
            _ => todo!("{expression:?}"),
        }
    }

    pub fn create_peepholed<F: FnOnce(NodeId) -> Node<'t>>(
        &mut self,
        types: &mut Types<'t>,
        f: F,
    ) -> NodeId {
        let id = self.nodes.create(f);
        let better = self.peephole(id, types);
        debug_assert_eq!(better, self.peephole(better, types));
        if better != id {
            // TODO: we could re-use the last slot. self.nodes.undo_create()
        }
        better
    }

    fn compute(&self, node: NodeId, types: &mut Types<'t>) -> Ty<'t> {
        match &self.nodes[node] {
            Node::Constant(c) => c.ty(),
            Node::Return(_) => types.ty_bot,
            Node::Start(_) => types.ty_bot,
            Node::Add(n) => self.compute_binary_int(&n.base, types, i64::wrapping_add),
            Node::Sub(n) => self.compute_binary_int(&n.base, types, i64::wrapping_sub),
            Node::Mul(n) => self.compute_binary_int(&n.base, types, i64::wrapping_mul),
            Node::Div(n) => {
                self.compute_binary_int(
                    &n.base,
                    types,
                    |a, b| if b == 0 { 0 } else { a.wrapping_div(b) },
                )
            }
            Node::Minus(n) => {
                let Some(input) = n.base.inputs[1].and_then(|n| self.nodes.ty(n)) else {
                    return types.ty_bot;
                };
                match &*input {
                    Type::Int(Int::Constant(v)) => types.get_int(v.wrapping_neg()),
                    _ => types.ty_bot,
                }
            }
            Node::Scope(_) => types.ty_bot,
            Node::Bool(b) => self.compute_binary_int(&b.base, types, |x, y| {
                match b.op {
                    BoolOp::EQ => x == y,
                    BoolOp::LT => x < y,
                    BoolOp::LE => x <= y,
                }
                .into()
            }),
            Node::Not(n) => {
                let Some(input) = n.base.inputs[1].and_then(|n| self.nodes.ty(n)) else {
                    return types.ty_bot;
                };
                match &*input {
                    Type::Int(Int::Constant(0)) => types.ty_one,
                    Type::Int(Int::Constant(_)) => types.ty_zero,
                    Type::Int(_) => input,
                    _ => types.ty_bot,
                }
            }
            Node::Proj(n) => {
                let Some(input) = n.base.inputs[0].and_then(|n| self.nodes.ty(n)) else {
                    return types.ty_bot;
                };
                match &*input {
                    Type::Tuple { types } => types[n.index],
                    _ => unreachable!("proj node ctrl must always be tuple, if present"),
                }
            }
        }
    }

    fn compute_binary_int<F: FnOnce(i64, i64) -> i64>(
        &self,
        base: &NodeBase,
        types: &mut Types<'t>,
        op: F,
    ) -> Ty<'t> {
        let Some(first) = base.inputs[1].and_then(|n| self.nodes.ty(n)) else {
            return types.ty_bot;
        };
        let Some(second) = base.inputs[2].and_then(|n| self.nodes.ty(n)) else {
            return types.ty_bot;
        };

        match [&*first, &*second] {
            [Type::Int(Int::Constant(v1)), Type::Int(Int::Constant(v2))] => {
                types.get_int(op(*v1, *v2))
            }
            [Type::Int(_), Type::Int(_)] => types.meet(first, second),
            _ => types.ty_bot,
        }
    }

    fn peephole(&mut self, node: NodeId, types: &mut Types<'t>) -> NodeId {
        let ty = self.compute(node, types);

        *self.nodes.get_ty_mut(node) = Some(ty);

        if self.disable_peephole {
            return node;
        }

        if !matches!(self.nodes[node], Node::Constant(_)) && ty.is_constant() {
            self.nodes.kill(node);
            return self
                .nodes
                .create(|id| Node::Constant(ConstantNode::new(id, self.start, ty)));
        }

        if let Some(n) = self.idealize(node, types) {
            return n;
        }

        node // no progress
    }

    fn idealize(&mut self, _node: NodeId, _types: &mut Types<'t>) -> Option<NodeId> {
        None
    }
}
