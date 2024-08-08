use crate::soup::nodes::{
    AddNode, ConstantNode, DivNode, MinusNode, MulNode, Node, NodeId, Nodes, ReturnNode, ScopeNode,
    StartNode, SubNode,
};
use crate::soup::types::{Ty, Type, Types};
use crate::syntax::ast::{BinaryOperator, Block, Expression, Function, PrefixOperator, Statement};

pub struct Soup<'t> {
    pub nodes: Nodes<'t>,
    pub(crate) start: NodeId,
    ctrl: NodeId,
    pub disable_peephole: bool,
}

impl<'t> Soup<'t> {
    pub fn new() -> Self {
        Self {
            nodes: Nodes::new(),
            start: NodeId::DUMMY,
            ctrl: NodeId::DUMMY,
            disable_peephole: false,
        }
    }
    pub fn compile_function(
        &mut self,
        function: &Function,
        types: &mut Types<'t>,
    ) -> Result<(NodeId, NodeId), ()> {
        let start = self.create_peepholed(types, |id| Node::StartNode(StartNode::new(id)));
        self.start = start;
        self.ctrl = start;

        let stop = match &function.body {
            None => todo!(),
            Some(body) => self.compile_block(&body, types)?,
        };
        Ok((start, stop))
    }

    fn compile_block(&mut self, block: &Block, types: &mut Types<'t>) -> Result<NodeId, ()> {
        for statement in &block.statements {
            match statement {
                Statement::Expression(_) => todo!(),
                Statement::Return(ret) => {
                    let data = self.compile_expression(&ret.value, types)?;
                    let ctrl = self.ctrl;
                    return Ok(self.create_peepholed(types, |id| {
                        Node::ReturnNode(ReturnNode::new(id, ctrl, data))
                    }));
                }
                Statement::If(_) => todo!(),
                Statement::Var(_) => todo!(),
            }
        }
        Err(())
    }

    fn compile_expression(
        &mut self,
        expression: &Expression,
        types: &mut Types<'t>,
    ) -> Result<NodeId, ()> {
        match expression {
            Expression::Immediate(immediate) => {
                let ty = types.get_int(*immediate);
                let start = self.start;
                Ok(self.create_peepholed(types, |id| {
                    Node::ConstantNode(ConstantNode::new(id, start, ty))
                }))
            }
            Expression::Binary {
                operator,
                location: _,
                left,
                right,
            } => {
                let left_node = self.compile_expression(left, types)?;
                let right_node = self.compile_expression(right, types)?;
                Ok(self.create_peepholed(types, |id| match operator {
                    BinaryOperator::Plus => {
                        Node::AddNode(AddNode::new(id, [left_node, right_node]))
                    }
                    BinaryOperator::Minus => {
                        Node::SubNode(SubNode::new(id, [left_node, right_node]))
                    }
                    BinaryOperator::Multiply => {
                        Node::MulNode(MulNode::new(id, [left_node, right_node]))
                    }
                    BinaryOperator::Divide => {
                        Node::DivNode(DivNode::new(id, [left_node, right_node]))
                    }
                    _ => todo!(),
                }))
            }
            Expression::Prefix { operator, operand } => {
                let operand = self.compile_expression(operand, types)?;
                Ok(self.create_peepholed(types, |id| match operator {
                    PrefixOperator::Minus => Node::MinusNode(MinusNode::new(id, operand)),
                    _ => todo! {},
                }))
            }
            _ => todo!(),
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
            Node::ConstantNode(c) => c.ty(),
            Node::ReturnNode(_) => types.ty_bot,
            Node::StartNode(_) => types.ty_bot,
            Node::AddNode(AddNode { base }) => {
                match self.nodes.get_many_ty([base.inputs[1], base.inputs[2]]) {
                    [Some(Type::Int {
                        value: v1,
                        constant: true,
                    }), Some(Type::Int {
                        value: v2,
                        constant: true,
                    })] => types.get_int(v1.wrapping_add(*v2)),
                    _ => types.ty_bot,
                }
            }
            Node::SubNode(SubNode { base }) => {
                match self.nodes.get_many_ty([base.inputs[1], base.inputs[2]]) {
                    [Some(Type::Int {
                        value: v1,
                        constant: true,
                    }), Some(Type::Int {
                        value: v2,
                        constant: true,
                    })] => types.get_int(v1.wrapping_sub(*v2)),
                    _ => types.ty_bot,
                }
            }
            Node::MulNode(MulNode { base }) => {
                match self.nodes.get_many_ty([base.inputs[1], base.inputs[2]]) {
                    [Some(Type::Int {
                        value: v1,
                        constant: true,
                    }), Some(Type::Int {
                        value: v2,
                        constant: true,
                    })] => types.get_int(v1.wrapping_mul(*v2)),
                    _ => types.ty_bot,
                }
            }
            Node::DivNode(DivNode { base }) => {
                match self.nodes.get_many_ty([base.inputs[1], base.inputs[2]]) {
                    [Some(Type::Int {
                        value: v1,
                        constant: true,
                    }), Some(Type::Int {
                        value: v2,
                        constant: true,
                    })] => {
                        // TODO: handle or ignore div by 0
                        types.get_int(v1.wrapping_div(*v2))
                    }
                    _ => types.ty_bot,
                }
            }
            Node::MinusNode(MinusNode { base }) => match self.nodes.get_many_ty([base.inputs[1]]) {
                [Some(Type::Int {
                    value,
                    constant: true,
                })] => types.get_int(value.wrapping_neg()),
                _ => types.ty_bot,
            },
            Node::ScopeNode(_) => types.ty_bot,
        }
    }
    fn peephole(&mut self, node: NodeId, types: &mut Types<'t>) -> NodeId {
        let ty = self.compute(node, types);

        *self.nodes.get_ty_mut(node) = Some(ty);

        if self.disable_peephole {
            return node;
        }

        if !matches!(self.nodes[node], Node::ConstantNode(_)) && ty.is_constant() {
            self.nodes.kill(node);
            return self
                .nodes
                .create(|id| Node::ConstantNode(ConstantNode::new(id, self.start, ty)));
        }

        if let Some(n) = self.idealize(node, types) {
            return n;
        }

        node // no progress
    }

    fn idealize(&mut self, node: NodeId, types: &mut Types<'t>) -> Option<NodeId> {
        None
    }
}
