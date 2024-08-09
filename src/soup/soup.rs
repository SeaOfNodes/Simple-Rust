use std::mem;

use crate::soup::graph_visualizer;
use crate::soup::nodes::{
    AddNode, BoolNode, BoolOp, ConstantNode, DivNode, MinusNode, MulNode, Node, NodeBase, NodeId,
    Nodes, NotNode, ProjNode, ReturnNode, ScopeNode, StartNode, SubNode,
};
use crate::soup::types::{Int, Ty, Type, Types};
use crate::syntax::ast::{BinaryOperator, Block, Expression, Function, PrefixOperator, Statement};
use crate::syntax::formatter::{CodeStyle, FormatCode};

pub struct Soup<'t> {
    pub nodes: Nodes<'t>,
    pub(crate) start: NodeId,
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
        let args = types.get_tuple(vec![types.ty_ctrl, self.arg.unwrap_or(types.ty_bot)]);
        let start = self.create_peepholed(types, |id| Node::Start(StartNode::new(id, args)));
        self.start = start;
        // if we didn't call peephole we might have to manually set the computed type like in the java constructor
        self.scope = self.create_peepholed(types, |id| Node::Scope(ScopeNode::new(id)));

        let body = function.body.as_ref().unwrap();

        self.nodes.scope_push(self.scope);
        let ctrl = self.create_peepholed(types, |id| {
            Node::Proj(ProjNode::new(id, start, 0, ScopeNode::CTRL.to_string()))
        });
        self.nodes
            .scope_define(self.scope, ScopeNode::CTRL.to_string(), ctrl)
            .expect("not in scope");
        let arg0 = self.create_peepholed(types, |id| {
            Node::Proj(ProjNode::new(id, start, 1, ScopeNode::ARG0.to_string()))
        });
        self.nodes
            .scope_define(self.scope, ScopeNode::ARG0.to_string(), arg0)
            .expect("not in scope");

        let stop = self.compile_block(&body, types);

        self.nodes.scope_pop(self.scope);

        if self.errors.is_empty() {
            Ok((start, stop))
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
                    result = Some(self.create_peepholed(types, |id| {
                        Node::Return(ReturnNode::new(id, ctrl, data))
                    }));
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
                let lr = [left_node, right_node];
                let rl = [right_node, left_node];
                let v = self.create_peepholed(types, |id| match operator {
                    BinaryOperator::Plus => Node::Add(AddNode::new(id, lr)),
                    BinaryOperator::Minus => Node::Sub(SubNode::new(id, lr)),
                    BinaryOperator::Multiply => Node::Mul(MulNode::new(id, lr)),
                    BinaryOperator::Divide => Node::Div(DivNode::new(id, lr)),
                    BinaryOperator::Equal | BinaryOperator::NotEqual => {
                        Node::Bool(BoolNode::new(id, lr, BoolOp::EQ))
                    }
                    BinaryOperator::LessOrEqual => Node::Bool(BoolNode::new(id, lr, BoolOp::LE)),
                    BinaryOperator::LessThan => Node::Bool(BoolNode::new(id, lr, BoolOp::LT)),
                    BinaryOperator::GreaterOrEqual => Node::Bool(BoolNode::new(id, rl, BoolOp::LE)),
                    BinaryOperator::GreaterThan => Node::Bool(BoolNode::new(id, rl, BoolOp::LT)),
                    _ => todo!(),
                });
                if let BinaryOperator::NotEqual = operator {
                    self.create_peepholed(types, |id| Node::Not(NotNode::new(id, v)))
                } else {
                    v
                }
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

    fn ctrl(&self) -> Option<NodeId> {
        self.nodes[self.scope].base().inputs[0]
    }
    fn set_ctrl(&mut self, node: Option<NodeId>) {
        self.nodes.set_def(self.scope, 0, node);
    }

    fn compute(&self, node: NodeId, types: &mut Types<'t>) -> Ty<'t> {
        match &self.nodes[node] {
            Node::Constant(c) => c.ty(),
            Node::Return(n) => {
                let ctrl = n.base.inputs[0]
                    .and_then(|n| self.nodes.ty(n))
                    .unwrap_or(types.ty_bot);
                let expr = n.base.inputs[1]
                    .and_then(|n| self.nodes.ty(n))
                    .unwrap_or(types.ty_bot);
                types.get_tuple(vec![ctrl, expr])
            }
            Node::Start(n) => n.args,
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
            Node::Bool(b) => {
                self.compute_binary_int(&b.base, types, |x, y| b.op.compute(x, y) as i64)
            }
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
            Node::If(_) => types.ty_if,
            Node::Phi(_) => types.ty_bot,
            Node::Region(_) => types.ty_ctrl,
            Node::Stop(_) => types.ty_bot,
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

    fn dead_code_elimination(&mut self, old: NodeId, new: NodeId) -> NodeId {
        if new != old && self.nodes[old].is_unused() {
            self.nodes.keep(new);
            self.nodes.kill(old);
            self.nodes.unkeep(new);
        }
        new
    }

    fn peephole(&mut self, node: NodeId, types: &mut Types<'t>) -> NodeId {
        let ty = self.compute(node, types);

        *self.nodes.get_ty_mut(node) = Some(ty);

        if self.disable_peephole {
            return node;
        }

        if !matches!(self.nodes[node], Node::Constant(_)) && ty.is_constant() {
            let start = self.start;
            let new_node =
                self.create_peepholed(types, |id| Node::Constant(ConstantNode::new(id, start, ty)));
            return self.dead_code_elimination(node, new_node);
        }

        if let Some(idealized) = self.idealize(node, types) {
            let new_node = self.peephole(idealized, types);
            return self.dead_code_elimination(node, new_node);
        }

        node // no progress
    }

    /// do not peephole directly returned values!
    fn idealize(&mut self, node: NodeId, types: &mut Types<'t>) -> Option<NodeId> {
        match &self.nodes[node] {
            Node::Add(n) => {
                let lhs = n.base.inputs[1]?;
                let rhs = n.base.inputs[2]?;
                let t1 = self.nodes.ty(lhs)?; // TODO is it safe to ignore this being None?
                let t2 = self.nodes.ty(rhs)?;

                // Already handled by peephole constant folding
                debug_assert!(!t1.is_constant() || !t2.is_constant(), "{t1} {t2}");

                // Add of 0.  We do not check for (0+x) because this will already
                // canonicalize to (x+0)
                if t2 == types.ty_zero {
                    return Some(lhs);
                }

                // Add of same to a multiply by 2
                if lhs == rhs {
                    let start = self.start;
                    let two = types.ty_two;
                    let two = self.create_peepholed(types, |id| {
                        Node::Constant(ConstantNode::new(id, start, two))
                    });
                    return Some(
                        self.nodes
                            .create(|id| Node::Mul(MulNode::new(id, [lhs, two]))),
                    );
                }

                // Goal: a left-spine set of adds, with constants on the rhs (which then fold).

                // Move non-adds to RHS
                if !matches!(self.nodes[lhs], Node::Add(_))
                    && matches!(self.nodes[rhs], Node::Add(_))
                {
                    return Some(self.nodes.swap_12(node));
                }

                // Now we might see (add add non) or (add non non) or (add add add) but never (add non add)

                // Do we have  x + (y + z) ?
                // Swap to    (x + y) + z
                // Rotate (add add add) to remove the add on RHS
                if let Node::Add(add) = &self.nodes[rhs] {
                    let x = lhs;
                    let y = add.base.inputs[1]?;
                    let z = add.base.inputs[2]?;
                    let new_lhs =
                        self.create_peepholed(types, |id| Node::Add(AddNode::new(id, [x, y])));
                    return Some(
                        self.nodes
                            .create(|id| Node::Add(AddNode::new(id, [new_lhs, z]))),
                    );
                }

                // Now we might see (add add non) or (add non non) but never (add non add) nor (add add add)
                if !matches!(self.nodes[lhs], Node::Add(_)) {
                    if self.spline_cmp(lhs, rhs) {
                        return Some(self.nodes.swap_12(node));
                    } else {
                        return None;
                    }
                }

                // Now we only see (add add non)

                // Do we have (x + con1) + con2?
                // Replace with (x + (con1+con2) which then fold the constants
                if self
                    .nodes
                    .ty(self.nodes[lhs].base().inputs[2]?)?
                    .is_constant()
                    && t2.is_constant()
                {
                    let x = self.nodes[lhs].base().inputs[1]?;
                    let con1 = self.nodes[lhs].base().inputs[2]?;
                    let con2 = rhs;

                    let new_rhs = self
                        .create_peepholed(types, |id| Node::Add(AddNode::new(id, [con1, con2])));
                    return Some(
                        self.nodes
                            .create(|id| Node::Add(AddNode::new(id, [x, new_rhs]))),
                    );
                }

                // Now we sort along the spline via rotates, to gather similar things together.

                // Do we rotate (x + y) + z
                // into         (x + z) + y ?
                if self.spline_cmp(self.nodes[lhs].base().inputs[2]?, rhs) {
                    // return new AddNode(new AddNode(lhs.in(1), rhs).peephole(), lhs.in(2));
                    let x = self.nodes[lhs].base().inputs[1]?;
                    let y = self.nodes[lhs].base().inputs[2]?;
                    let z = rhs;

                    let new_lhs =
                        self.create_peepholed(types, |id| Node::Add(AddNode::new(id, [x, z])));
                    return Some(
                        self.nodes
                            .create(|id| Node::Add(AddNode::new(id, [new_lhs, y]))),
                    );
                }

                None
            }
            Node::Sub(n) => {
                if n.base.inputs[1]? == n.base.inputs[2]? {
                    Some(self.nodes.create(|id| {
                        Node::Constant(ConstantNode::new(id, self.start, types.ty_zero))
                    }))
                } else {
                    None
                }
            }
            Node::Mul(n) => {
                let left = n.base.inputs[1]?;
                let right = n.base.inputs[2]?;
                let left_ty = self.nodes.ty(left)?; // TODO is it safe to ignore this being None?
                let right_ty = self.nodes.ty(right)?;

                if matches!(&*right_ty, Type::Int(Int::Constant(1))) {
                    Some(left)
                } else if left_ty.is_constant() && !right_ty.is_constant() {
                    self.nodes.swap_12(node);
                    Some(node)
                } else {
                    None
                }
            }
            Node::Bool(n) => {
                if n.base.inputs[1]? == n.base.inputs[2]? {
                    let value = if n.op.compute(3, 3) {
                        types.ty_one
                    } else {
                        types.ty_zero
                    };
                    Some(
                        self.nodes
                            .create(|id| Node::Constant(ConstantNode::new(id, self.start, value))),
                    )
                } else {
                    None
                }
            }
            Node::Phi(_) => todo!(),
            Node::Constant(_)
            | Node::Return(_)
            | Node::Start(_)
            | Node::Div(_)
            | Node::Minus(_)
            | Node::Scope(_)
            | Node::Not(_)
            | Node::Proj(_)
            | Node::If(_)
            | Node::Region(_)
            | Node::Stop(_) => None,
        }
    }

    // Compare two off-spline nodes and decide what order they should be in.
    // Do we rotate ((x + hi) + lo) into ((x + lo) + hi) ?
    // Generally constants always go right, then others.
    // Ties with in a category sort by node ID.
    // TRUE if swapping hi and lo.
    fn spline_cmp(&mut self, hi: NodeId, lo: NodeId) -> bool {
        if self.nodes.ty(lo).is_some_and(|t| t.is_constant()) {
            return false;
        }
        if self.nodes.ty(hi).is_some_and(|t| t.is_constant()) {
            return true;
        }
        lo.index() > hi.index()
    }
}
