use std::mem;

use crate::soup::graph_visualizer;
use crate::soup::nodes::{BoolOp, Node, NodeCreation, NodeId, Nodes, ScopeNode};
use crate::soup::types::{Ty, Types};
use crate::syntax::ast::{
    BinaryOperator, Block, Expression, Function, If, PrefixOperator, Statement,
};
use crate::syntax::formatter::{CodeStyle, FormatCode};

pub struct Soup<'t> {
    pub nodes: Nodes<'t>,
    pub(crate) stop: NodeId,
    pub(crate) scope: NodeId,
    pub(crate) x_scopes: Vec<NodeId>,
    errors: Vec<String>,
    arg: Option<Ty<'t>>,
}

impl<'t> Soup<'t> {
    pub fn new() -> Self {
        Self {
            nodes: Nodes::new(),
            stop: NodeId::DUMMY,
            scope: NodeId::DUMMY,
            x_scopes: vec![],
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
        self.x_scopes.push(self.scope);

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
        self.x_scopes.pop();

        // TODO if the block didn't return unconditionally we have to create a return instruction

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
                Statement::If(i) => {
                    self.compile_if(i, 0, types);
                    result = Some(self.create_unit(types)); // TODO make if statements return a value
                }
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

    fn compile_if(&mut self, if_: &If, case_index: usize, types: &mut Types<'t>) {
        let (predicate, then_block) = &if_.cases[case_index];

        let predicate = self.compile_expression(predicate, types);

        let if_node = self.create_peepholed(types, Node::make_if(self.ctrl().unwrap(), predicate));

        let if_true = self.create_peepholed(types, Node::make_proj(if_node, 0, "True".to_string()));
        let if_false =
            self.create_peepholed(types, Node::make_proj(if_node, 1, "False".to_string()));

        // In if true branch, the ifT proj node becomes the ctrl
        // But first clone the scope and set it as current
        let n_defs = self.nodes.inputs[self.scope].len();
        let mut false_scope = self.nodes.scope_dup(self.scope);
        self.x_scopes.push(false_scope);

        self.set_ctrl(Some(if_true));
        self.compile_block(then_block, types);

        let true_scope = self.scope;

        self.scope = false_scope;
        self.set_ctrl(Some(if_false));

        if case_index + 1 == if_.cases.len() {
            if let Some(else_block) = &if_.else_block {
                self.compile_block(else_block, types);
            }
        } else {
            self.compile_if(if_, case_index + 1, types);
        }

        false_scope = self.scope;

        assert_eq!(
            n_defs,
            self.nodes.inputs[true_scope].len(),
            "Cannot define a new name on one arm of an if"
        );
        assert_eq!(
            n_defs,
            self.nodes.inputs[false_scope].len(),
            "Cannot define a new name on one arm of an if"
        );

        self.scope = true_scope;
        self.x_scopes.pop();

        let merged = self.nodes.scope_merge(true_scope, false_scope, types);
        self.set_ctrl(Some(merged));
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
            Expression::Boolean(b) => {
                let ty = if *b { types.ty_one } else { types.ty_zero };
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
