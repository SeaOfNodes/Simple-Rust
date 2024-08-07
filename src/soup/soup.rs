use crate::hir::types::Types;
use crate::soup::nodes::{AddNode, ConstantNode, DivNode, MinusNode, MulNode, Node, NodeId, Nodes, ReturnNode, StartNode, SubNode};
use crate::syntax::ast::{BinaryOperator, Block, Expression, Function, PrefixOperator, Statement};

pub struct Soup {
    pub nodes: Nodes,
    start: NodeId,
    ctrl: NodeId,
    pub disable_peephole: bool,
}

impl Soup {
    pub fn new() -> Self {
        Self {
            nodes: Nodes::new(),
            start: NodeId::DUMMY,
            ctrl: NodeId::DUMMY,
            disable_peephole: false,
        }
    }
    pub fn compile_function(&mut self, function: &Function, types: &mut Types) -> Result<(NodeId, NodeId), ()> {
        let start = self.nodes.create(|id| Node::StartNode(StartNode::new(id)));
        self.start = start;
        self.ctrl = start;

        let stop = match &function.body {
            None => todo!(),
            Some(body) => self.compile_block(&body, types)?,
        };
        Ok((start, stop))
    }

    fn compile_block(&mut self, block: &Block, types: &mut Types) -> Result<NodeId, ()> {
        for statement in &block.statements {
            match statement {
                Statement::Expression(_) => todo!(),
                Statement::Return(ret) => {
                    let data = self.compile_expression(&ret.value, types)?;
                    return Ok(self.nodes.create(|id| Node::ReturnNode(ReturnNode::new(id, self.ctrl, data))));
                }
                Statement::If(_) => todo!(),
                Statement::Var(_) => todo!(),
            }
        }
        Err(())
    }

    fn compile_expression(&mut self, expression: &Expression, types: &mut Types) -> Result<NodeId, ()> {
        match expression {
            Expression::Immediate(immediate) => {
                Ok(self.nodes.create(|id| Node::ConstantNode(ConstantNode::new(id, self.start, *immediate))))
            }
            Expression::Binary { operator, location: _, left, right } => {
                let left_node = self.compile_expression(left, types)?;
                let right_node = self.compile_expression(right, types)?;
                Ok(self.nodes.create(|id| match operator {
                    BinaryOperator::Plus => Node::AddNode(AddNode::new(id, [left_node, right_node])),
                    BinaryOperator::Minus => Node::SubNode(SubNode::new(id, [left_node, right_node])),
                    BinaryOperator::Multiply => Node::MulNode(MulNode::new(id, [left_node, right_node])),
                    BinaryOperator::Divide => Node::DivNode(DivNode::new(id, [left_node, right_node])),
                    _ => todo!(),
                }))
            }
            Expression::Prefix { operator, operand } => {
                let operand = self.compile_expression(operand, types)?;
                Ok(self.nodes.create(|id| match operator {
                    PrefixOperator::Minus => {
                        Node::MinusNode(MinusNode::new(id, operand))
                    }
                    _ => todo! {}
                }))
            }
            _ => todo!(),
        }
    }
}