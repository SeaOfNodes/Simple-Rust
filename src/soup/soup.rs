use std::mem;

use crate::soup::graph_visualizer;
use crate::soup::nodes::{BoolOp, Node, NodeCreation, NodeId, Nodes, ScopeNode};
use crate::soup::types::{Ty, Types};
use crate::syntax::ast::{BinaryOperator, Block, Expression, Function, PrefixOperator, Statement};
use crate::syntax::formatter::{CodeStyle, FormatCode};

pub struct Soup<'t> {
    pub nodes: Nodes<'t>,
    pub(crate) stop: NodeId,
    pub(crate) scope: NodeId,
    errors: Vec<String>,
    arg: Option<Ty<'t>>,
}

impl<'t> Soup<'t> {
    pub fn new() -> Self {
        Self {
            nodes: Nodes::new(),
            stop: NodeId::DUMMY,
            scope: NodeId::DUMMY,
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
    ) -> Result<NodeId, Vec<String>> {
        let args = types.get_tuple(vec![types.ty_ctrl, self.arg.unwrap_or(types.ty_bot)]);
        let start = self.create_peepholed(types, Node::make_start(args));
        self.nodes.start = start;

        self.stop = self.nodes.create(Node::make_stop());

        // if we didn't call peephole we might have to manually set the computed type like in the java constructor
        self.scope = self.create_peepholed(types, Node::make_scope());

        let body = function.body.as_ref().unwrap();

        self.nodes.scope_push(self.scope);
        let ctrl = self.create_peepholed(
            types,
            Node::make_proj(start, 0, ScopeNode::CTRL.to_string()),
        );
        self.nodes
            .scope_define(self.scope, ScopeNode::CTRL.to_string(), ctrl)
            .expect("not in scope");
        let arg0 = self.create_peepholed(
            types,
            Node::make_proj(start, 1, ScopeNode::ARG0.to_string()),
        );
        self.nodes
            .scope_define(self.scope, ScopeNode::ARG0.to_string(), arg0)
            .expect("not in scope");

        let ret = self.compile_block(&body, types);

        self.nodes.scope_pop(self.scope);

        if !matches!(self.nodes[ret], Node::Return) {
            todo!("create return for value block");
        }

        self.nodes.peephole(self.stop, types);

        if self.errors.is_empty() {
            Ok(self.stop)
        } else {
            Err(mem::take(&mut self.errors))
        }
    }

    fn compile_block(&mut self, block: &Block, types: &mut Types<'t>) -> NodeId {
        let mut result = None;

        self.nodes.scope_push(self.scope);
        for statement in &block.statements {
            if self.ctrl().is_none() && !matches!(statement, Statement::Meta(_)) {
                let s = statement.format(&CodeStyle::DEFAULT);
                self.errors
                    .push(format!("Unreachable statement after return: {s}"));
                break;
            }
            match statement {
                Statement::Expression(e) => result = Some(self.compile_expression(e, types)),
                Statement::Return(ret) => {
                    let data = self.compile_expression(&ret.value, types);
                    let ctrl = self.ctrl().unwrap();
                    result = Some(self.create_peepholed(types, Node::make_return(ctrl, data)));
                    self.nodes.add_def(self.stop, result);
                    self.set_ctrl(None);
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
        // TODO return type Unit or something like that
        self.create_peepholed(types, Node::make_constant(self.nodes.start, types.ty_zero))
    }

    fn compile_expression(&mut self, expression: &Expression, types: &mut Types<'t>) -> NodeId {
        match expression {
            Expression::Immediate(immediate) => {
                let ty = types.get_int(*immediate);
                self.create_peepholed(types, Node::make_constant(self.nodes.start, ty))
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
                let lr = [left_node, right_node];
                let rl = [right_node, left_node];
                let v = self.create_peepholed(
                    types,
                    match operator {
                        BinaryOperator::Plus => Node::make_add(lr),
                        BinaryOperator::Minus => Node::make_sub(lr),
                        BinaryOperator::Multiply => Node::make_mul(lr),
                        BinaryOperator::Divide => Node::make_div(lr),
                        BinaryOperator::Equal | BinaryOperator::NotEqual => {
                            Node::make_bool(lr, BoolOp::EQ)
                        }
                        BinaryOperator::LessOrEqual => Node::make_bool(lr, BoolOp::LE),
                        BinaryOperator::LessThan => Node::make_bool(lr, BoolOp::LT),
                        BinaryOperator::GreaterOrEqual => Node::make_bool(rl, BoolOp::LE),
                        BinaryOperator::GreaterThan => Node::make_bool(rl, BoolOp::LT),
                        _ => todo!(),
                    },
                );
                if let BinaryOperator::NotEqual = operator {
                    self.create_peepholed(types, Node::make_not(v))
                } else {
                    v
                }
            }
            Expression::Prefix { operator, operand } => {
                let operand = self.compile_expression(operand, types);
                self.create_peepholed(
                    types,
                    match operator {
                        PrefixOperator::Minus => Node::make_minus(operand),
                        _ => todo! {},
                    },
                )
            }
            Expression::Parenthesized(inner) => self.compile_expression(inner, types),
            Expression::Block(block) => self.compile_block(block, types),
            _ => todo!("{expression:?}"),
        }
    }

    fn ctrl(&self) -> Option<NodeId> {
        self.nodes.inputs[self.scope][0]
    }
    fn set_ctrl(&mut self, node: Option<NodeId>) {
        self.nodes.set_def(self.scope, 0, node);
    }

    fn create_peepholed(&mut self, types: &mut Types<'t>, c: NodeCreation<'t>) -> NodeId {
        self.nodes.create_peepholed(types, c)
    }
}
