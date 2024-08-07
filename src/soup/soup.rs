use crate::hir::types::Types;
use crate::soup::nodes::{ConstantNode, Node, NodeId, Nodes, ReturnNode, StartNode};
use crate::syntax::ast::{Block, Expression, Function, Statement};

pub struct Soup {
    pub nodes: Nodes,
    start: NodeId,
    ctrl: NodeId,
}

impl Soup {
    pub fn new() -> Self {
        Self {
            nodes: Nodes::new(),
            start: NodeId::DUMMY,
            ctrl: NodeId::DUMMY,
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
            _ => todo!(),
        }
    }
}