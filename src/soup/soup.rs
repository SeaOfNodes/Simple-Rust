use std::mem;

use crate::datastructures::id::Id;
use crate::soup::graph_visualizer;
use crate::soup::nodes::{BoolOp, Node, NodeCreation, NodeId, Nodes, ScopeNode};
use crate::soup::types::{Int, Ty, Type, Types};
use crate::syntax::ast::{BinaryOperator, Block, Expression, Function, PrefixOperator, Statement};
use crate::syntax::formatter::{CodeStyle, FormatCode};

pub struct Soup<'t> {
    pub nodes: Nodes<'t>,
    pub(crate) start: NodeId,
    pub(crate) stop: NodeId,
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
            stop: NodeId::DUMMY,
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
    ) -> Result<NodeId, Vec<String>> {
        let args = types.get_tuple(vec![types.ty_ctrl, self.arg.unwrap_or(types.ty_bot)]);
        let start = self.create_peepholed(types, Node::make_start(args));
        self.start = start;

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

        self.peephole(self.stop, types);

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
        let ty = types.ty_zero; // TODO return Unit or something like that
        let start = self.start;
        self.create_peepholed(types, Node::make_constant(start, ty))
    }

    fn compile_expression(&mut self, expression: &Expression, types: &mut Types<'t>) -> NodeId {
        match expression {
            Expression::Immediate(immediate) => {
                let ty = types.get_int(*immediate);
                let start = self.start;
                self.create_peepholed(types, Node::make_constant(start, ty))
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

    pub fn create_peepholed(&mut self, types: &mut Types<'t>, c: NodeCreation<'t>) -> NodeId {
        let id = self.nodes.create(c);
        let better = self.peephole(id, types);
        debug_assert_eq!(better, self.peephole(better, types));
        if better != id {
            // TODO: we could re-use the dead slots: self.nodes.trim_end()
        }
        better
    }

    fn ctrl(&self) -> Option<NodeId> {
        self.nodes.inputs[self.scope][0]
    }
    fn set_ctrl(&mut self, node: Option<NodeId>) {
        self.nodes.set_def(self.scope, 0, node);
    }

    fn compute(&self, node: NodeId, types: &mut Types<'t>) -> Ty<'t> {
        match &self.nodes[node] {
            Node::Constant(ty) => *ty,
            Node::Return => {
                let ctrl = self.nodes.inputs[node][0]
                    .and_then(|n| self.nodes.ty[n])
                    .unwrap_or(types.ty_bot);
                let expr = self.nodes.inputs[node][1]
                    .and_then(|n| self.nodes.ty[n])
                    .unwrap_or(types.ty_bot);
                types.get_tuple(vec![ctrl, expr])
            }
            Node::Start { args } => *args,
            Node::Add => self.compute_binary_int(node, types, i64::wrapping_add),
            Node::Sub => self.compute_binary_int(node, types, i64::wrapping_sub),
            Node::Mul => self.compute_binary_int(node, types, i64::wrapping_mul),
            Node::Div => {
                self.compute_binary_int(
                    node,
                    types,
                    |a, b| if b == 0 { 0 } else { a.wrapping_div(b) },
                )
            }
            Node::Minus => {
                let Some(input) = self.nodes.inputs[node][1].and_then(|n| self.nodes.ty[n]) else {
                    return types.ty_bot;
                };
                match &*input {
                    Type::Int(Int::Constant(v)) => types.get_int(v.wrapping_neg()),
                    _ => types.ty_bot,
                }
            }
            Node::Scope(_) => types.ty_bot,
            Node::Bool(op) => self.compute_binary_int(node, types, |x, y| op.compute(x, y) as i64),
            Node::Not => {
                let Some(input) = self.nodes.inputs[node][1].and_then(|n| self.nodes.ty[n]) else {
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
                let Some(input) = self.nodes.inputs[node][0].and_then(|n| self.nodes.ty[n]) else {
                    return types.ty_bot;
                };
                match &*input {
                    Type::Tuple { types } => types[n.index],
                    _ => unreachable!("proj node ctrl must always be tuple, if present"),
                }
            }
            Node::If => types.ty_if,
            Node::Phi(_) => types.ty_bot,
            Node::Region => types.ty_ctrl,
            Node::Stop => types.ty_bot,
        }
    }

    fn compute_binary_int<F: FnOnce(i64, i64) -> i64>(
        &self,
        node: NodeId,
        types: &mut Types<'t>,
        op: F,
    ) -> Ty<'t> {
        let Some(first) = self.nodes.inputs[node][1].and_then(|n| self.nodes.ty[n]) else {
            return types.ty_bot;
        };
        let Some(second) = self.nodes.inputs[node][2].and_then(|n| self.nodes.ty[n]) else {
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
        if new != old && self.nodes.is_unused(old) {
            self.nodes.keep(new);
            self.nodes.kill(old);
            self.nodes.unkeep(new);
        }
        new
    }

    fn peephole(&mut self, node: NodeId, types: &mut Types<'t>) -> NodeId {
        let ty = self.compute(node, types);

        self.nodes.ty[node] = Some(ty);

        if self.disable_peephole {
            return node;
        }

        if !matches!(self.nodes[node], Node::Constant(_)) && ty.is_constant() {
            let start = self.start;
            let new_node = self.create_peepholed(types, Node::make_constant(start, ty));
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
            Node::Add => {
                let lhs = self.nodes.inputs[node][1]?;
                let rhs = self.nodes.inputs[node][2]?;
                let t1 = self.nodes.ty[lhs]?; // TODO is it safe to ignore this being None?
                let t2 = self.nodes.ty[rhs]?;

                // Already handled by peephole constant folding
                debug_assert!(!t1.is_constant() || !t2.is_constant(), "{t1} {t2}");

                // Add of 0.  We do not check for (0+x) because this will already
                // canonicalize to (x+0)
                if t2 == types.ty_zero {
                    return Some(lhs);
                }

                // Add of same to a multiply by 2
                if lhs == rhs {
                    let two =
                        self.create_peepholed(types, Node::make_constant(self.start, types.ty_two));
                    return Some(self.nodes.create(Node::make_mul([lhs, two])));
                }

                // Goal: a left-spine set of adds, with constants on the rhs (which then fold).

                // Move non-adds to RHS
                if !matches!(self.nodes[lhs], Node::Add) && matches!(self.nodes[rhs], Node::Add) {
                    return Some(self.nodes.swap_12(node));
                }

                // Now we might see (add add non) or (add non non) or (add add add) but never (add non add)

                // Do we have  x + (y + z) ?
                // Swap to    (x + y) + z
                // Rotate (add add add) to remove the add on RHS
                if let Node::Add = &self.nodes[rhs] {
                    let x = lhs;
                    let y = self.nodes.inputs[rhs][1]?;
                    let z = self.nodes.inputs[rhs][2]?;
                    let new_lhs = self.create_peepholed(types, Node::make_add([x, y]));
                    return Some(self.nodes.create(Node::make_add([new_lhs, z])));
                }

                // Now we might see (add add non) or (add non non) but never (add non add) nor (add add add)
                if !matches!(self.nodes[lhs], Node::Add) {
                    if self.spline_cmp(lhs, rhs) {
                        return Some(self.nodes.swap_12(node));
                    } else {
                        return None;
                    }
                }

                // Now we only see (add add non)

                // Do we have (x + con1) + con2?
                // Replace with (x + (con1+con2) which then fold the constants
                if self.nodes.ty[self.nodes.inputs[lhs][2]?]?.is_constant() && t2.is_constant() {
                    let x = self.nodes.inputs[lhs][1]?;
                    let con1 = self.nodes.inputs[lhs][2]?;
                    let con2 = rhs;

                    let new_rhs = self.create_peepholed(types, Node::make_add([con1, con2]));
                    return Some(self.nodes.create(Node::make_add([x, new_rhs])));
                }

                // Now we sort along the spline via rotates, to gather similar things together.

                // Do we rotate (x + y) + z
                // into         (x + z) + y ?
                if self.spline_cmp(self.nodes.inputs[lhs][2]?, rhs) {
                    // return new AddNode(new AddNode(lhs.in(1), rhs).peephole(), lhs.in(2));
                    let x = self.nodes.inputs[lhs][1]?;
                    let y = self.nodes.inputs[lhs][2]?;
                    let z = rhs;

                    let new_lhs = self.create_peepholed(types, Node::make_add([x, z]));
                    return Some(self.nodes.create(Node::make_add([new_lhs, y])));
                }

                None
            }
            Node::Sub => {
                if self.nodes.inputs[node][1]? == self.nodes.inputs[node][2]? {
                    Some(
                        self.nodes
                            .create(Node::make_constant(self.start, types.ty_zero)),
                    )
                } else {
                    None
                }
            }
            Node::Mul => {
                let left = self.nodes.inputs[node][1]?;
                let right = self.nodes.inputs[node][2]?;
                let left_ty = self.nodes.ty[left]?;
                let right_ty = self.nodes.ty[right]?;

                if matches!(&*right_ty, Type::Int(Int::Constant(1))) {
                    Some(left)
                } else if left_ty.is_constant() && !right_ty.is_constant() {
                    self.nodes.swap_12(node);
                    Some(node)
                } else {
                    None
                }
            }
            Node::Bool(op) => {
                if self.nodes.inputs[node][1]? == self.nodes.inputs[node][2]? {
                    let value = if op.compute(3, 3) {
                        types.ty_one
                    } else {
                        types.ty_zero
                    };
                    Some(self.nodes.create(Node::make_constant(self.start, value)))
                } else {
                    None
                }
            }
            Node::Phi(_) => todo!(),
            Node::Constant(_)
            | Node::Return
            | Node::Start { .. }
            | Node::Div
            | Node::Minus
            | Node::Scope(_)
            | Node::Not
            | Node::Proj(_)
            | Node::If
            | Node::Region
            | Node::Stop => None,
        }
    }

    // Compare two off-spline nodes and decide what order they should be in.
    // Do we rotate ((x + hi) + lo) into ((x + lo) + hi) ?
    // Generally constants always go right, then others.
    // Ties with in a category sort by node ID.
    // TRUE if swapping hi and lo.
    fn spline_cmp(&mut self, hi: NodeId, lo: NodeId) -> bool {
        if self.nodes.ty[lo].is_some_and(|t| t.is_constant()) {
            return false;
        }
        if self.nodes.ty[hi].is_some_and(|t| t.is_constant()) {
            return true;
        }
        lo.index() > hi.index()
    }

    fn merge_scopes(&mut self, this: NodeId, that: NodeId, types: &mut Types<'t>) {
        let c1 = self.nodes.inputs[this][0];
        let c2 = self.nodes.inputs[that][0];
        let region = self.create_peepholed(types, Node::make_region(vec![None, c1, c2]));

        let mut names = vec![None; self.nodes.inputs[this].len()];
        let this_scope = self.nodes.scope_mut(this);
        for syms in &this_scope.scopes {
            for (name, &index) in syms {
                debug_assert!(names[index].is_none());
                names[index] = Some(name.clone());
            }
        }

        // Note that we skip i==0, which is bound to '$ctrl'
        for (i, name) in names.into_iter().enumerate().skip(1) {
            let this_in = self.nodes.inputs[this][i];
            let that_in = self.nodes.inputs[that][i];
            if this_in != that_in {
                let phi = self.create_peepholed(
                    types,
                    Node::make_phi(name.unwrap(), vec![Some(region), this_in, that_in]),
                );
                self.nodes.set_def(this, i, Some(phi));
            }
        }

        self.nodes.kill(that);
    }
}
