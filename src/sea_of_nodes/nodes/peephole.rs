use crate::sea_of_nodes::nodes::{Node, NodeId, Nodes};
use crate::sea_of_nodes::types::{Int, Ty, Type, Types};

impl<'t> Nodes<'t> {
    pub fn peephole(&mut self, node: NodeId, types: &mut Types<'t>) -> NodeId {
        let ty = self.compute(node, types);

        self.ty[node] = Some(ty);

        if self.disable_peephole {
            return node;
        }

        if !matches!(self[node], Node::Constant(_)) && ty.is_constant() {
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

    fn dead_code_elimination(&mut self, old: NodeId, new: NodeId) -> NodeId {
        if new != old && self.is_unused(old) {
            self.keep(new);
            self.kill(old);
            self.unkeep(new);
        }
        new
    }

    fn compute(&self, node: NodeId, types: &mut Types<'t>) -> Ty<'t> {
        match &self[node] {
            Node::Constant(ty) => *ty,
            Node::Return => {
                let ctrl = self.inputs[node][0]
                    .and_then(|n| self.ty[n])
                    .unwrap_or(types.ty_bot);
                let expr = self.inputs[node][1]
                    .and_then(|n| self.ty[n])
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
                let Some(input) = self.inputs[node][1].and_then(|n| self.ty[n]) else {
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
                let Some(input) = self.inputs[node][1].and_then(|n| self.ty[n]) else {
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
                let Some(input) = self.inputs[node][0].and_then(|n| self.ty[n]) else {
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
        let Some(first) = self.inputs[node][1].and_then(|n| self.ty[n]) else {
            return types.ty_bot;
        };
        let Some(second) = self.inputs[node][2].and_then(|n| self.ty[n]) else {
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
}
