use crate::datastructures::id::Id;
use crate::sea_of_nodes::nodes::{BoolOp, Node, NodeId, Nodes};
use crate::sea_of_nodes::types::{Int, Type};

impl<'t> Nodes<'t> {
    /// do not peephole directly returned values!
    pub(super) fn idealize(&mut self, node: NodeId) -> Option<NodeId> {
        match &self[node] {
            Node::Add => self.idealize_add(node),
            Node::Sub => self.idealize_sub(node),
            Node::Mul => self.idealize_mul(node),
            Node::Bool(op) => self.idealize_bool(*op, node),
            Node::Phi(_) => self.idealize_phi(node),
            Node::Stop => self.idealize_stop(node),
            Node::Return => self.idealize_return(node),
            Node::Proj(p) => self.idealize_proj(node, p.index),
            Node::Region { .. } | Node::Loop => self.idealize_region(node),
            Node::Constant(_)
            | Node::Start { .. }
            | Node::Div
            | Node::Minus
            | Node::Scope(_)
            | Node::Not
            | Node::If => None,
        }
    }

    fn idealize_add(&mut self, node: NodeId) -> Option<NodeId> {
        let lhs = self.inputs[node][1]?;
        let rhs = self.inputs[node][2]?;
        let t1 = self.ty[lhs]?; // TODO is it safe to ignore this being None?
        let t2 = self.ty[rhs]?;

        // Already handled by peephole constant folding
        debug_assert!(!t1.is_constant() || !t2.is_constant(), "{t1} {t2}");

        // Add of 0.  We do not check for (0+x) because this will already
        // canonicalize to (x+0)
        if t2 == self.types.ty_zero {
            return Some(lhs);
        }

        // Add of same to a multiply by 2
        if lhs == rhs {
            let two = self.create_peepholed(Node::make_constant(self.start, self.types.ty_two));
            return Some(self.create(Node::make_mul([lhs, two])));
        }

        // Goal: a left-spine set of adds, with constants on the rhs (which then fold).

        // Move non-adds to RHS
        if !matches!(self[lhs], Node::Add) && matches!(self[rhs], Node::Add) {
            return Some(self.swap_12(node));
        }

        // Now we might see (add add non) or (add non non) or (add add add) but never (add non add)

        // Do we have  x + (y + z) ?
        // Swap to    (x + y) + z
        // Rotate (add add add) to remove the add on RHS
        if let Node::Add = &self[rhs] {
            let x = lhs;
            let y = self.inputs[rhs][1]?;
            let z = self.inputs[rhs][2]?;
            let new_lhs = self.create_peepholed(Node::make_add([x, y]));
            return Some(self.create(Node::make_add([new_lhs, z])));
        }

        // Now we might see (add add non) or (add non non) but never (add non add) nor (add add add)
        if !matches!(self[lhs], Node::Add) {
            return if self.spline_cmp(lhs, rhs) {
                Some(self.swap_12(node))
            } else {
                self.phi_con(node, true)
            };
        }

        // Now we only see (add add non)

        // Dead data cycle; comes about from dead infinite loops.  Do nothing,
        // the loop will peep as dead after a bit.
        if self.inputs[lhs][1] == Some(lhs) {
            return None;
        }

        // Do we have (x + con1) + con2?
        // Replace with (x + (con1+con2) which then fold the constants
        if self.ty[self.inputs[lhs][2]?]?.is_constant() && t2.is_constant() {
            let x = self.inputs[lhs][1]?;
            let con1 = self.inputs[lhs][2]?;
            let con2 = rhs;

            let new_rhs = self.create_peepholed(Node::make_add([con1, con2]));
            return Some(self.create(Node::make_add([x, new_rhs])));
        }

        // Do we have ((x + (phi cons)) + con) ?
        // Do we have ((x + (phi cons)) + (phi cons)) ?
        // Push constant up through the phi: x + (phi con0+con0 con1+con1...)
        let phicon = self.phi_con(node, true);
        if phicon.is_some() {
            return phicon;
        }

        // Now we sort along the spline via rotates, to gather similar things together.

        // Do we rotate (x + y) + z
        // into         (x + z) + y ?
        if self.spline_cmp(self.inputs[lhs][2]?, rhs) {
            // return new AddNode(new AddNode(lhs.in(1), rhs).peephole(), lhs.in(2));
            let x = self.inputs[lhs][1]?;
            let y = self.inputs[lhs][2]?;
            let z = rhs;

            let new_lhs = self.create_peepholed(Node::make_add([x, z]));
            return Some(self.create(Node::make_add([new_lhs, y])));
        }

        None
    }

    fn idealize_sub(&mut self, node: NodeId) -> Option<NodeId> {
        if self.inputs[node][1]? == self.inputs[node][2]? {
            Some(self.create(Node::make_constant(self.start, self.types.ty_zero)))
        } else {
            None
        }
    }

    fn idealize_mul(&mut self, node: NodeId) -> Option<NodeId> {
        let left = self.inputs[node][1]?;
        let right = self.inputs[node][2]?;
        let left_ty = self.ty[left]?;
        let right_ty = self.ty[right]?;

        if matches!(&*right_ty, Type::Int(Int::Constant(1))) {
            Some(left)
        } else if left_ty.is_constant() && !right_ty.is_constant() {
            self.swap_12(node);
            Some(node)
        } else {
            // Do we have ((x * (phi cons)) * con) ?
            // Do we have ((x * (phi cons)) * (phi cons)) ?
            // Push constant up through the phi: x * (phi con0*con0 con1*con1...)
            self.phi_con(node, true)
        }
    }

    fn idealize_bool(&mut self, op: BoolOp, node: NodeId) -> Option<NodeId> {
        if self.inputs[node][1]? == self.inputs[node][2]? {
            let value = if op.compute(3, 3) {
                self.types.ty_one
            } else {
                self.types.ty_zero
            };
            return Some(self.create(Node::make_constant(self.start, value)));
        }

        // Do we have ((x * (phi cons)) * con) ?
        // Do we have ((x * (phi cons)) * (phi cons)) ?
        // Push constant up through the phi: x * (phi con0*con0 con1*con1...)
        let phicon = self.phi_con(node, false);
        phicon
    }

    fn idealize_phi(&mut self, node: NodeId) -> Option<NodeId> {
        if self.phi_no_or_in_progress_region(node) {
            return None;
        }

        // If we have only a single unique input, become it.
        if let live @ Some(_) = self.single_unique_input(node) {
            return live;
        }

        // Pull "down" a common data op.  One less op in the world.  One more
        // Phi, but Phis do not make code.
        //   Phi(op(A,B),op(Q,R),op(X,Y)) becomes
        //     op(Phi(A,Q,X), Phi(B,R,Y)).
        let op = self.inputs[node][1].expect("not same_inputs");
        if self.inputs[op].len() == 3
            && self.inputs[op][0].is_none()
            && !self.is_cfg(op)
            && self.same_op(node)
        {
            let n_in = &self.inputs[node];

            let mut lhss = vec![None; n_in.len()];
            let mut rhss = vec![None; n_in.len()];

            // Set Region
            lhss[0] = n_in[0];
            rhss[0] = n_in[0];

            for i in 1..n_in.len() {
                lhss[i] = self.inputs[n_in[i].unwrap()][1];
                rhss[i] = self.inputs[n_in[i].unwrap()][2];
            }

            let label = self[node].phi_label().unwrap();
            let phi_lhs = Node::make_phi(label.to_string(), lhss);
            let phi_rhs = Node::make_phi(label.to_string(), rhss);

            let phi_lhs = self.create_peepholed(phi_lhs);
            let phi_rhs = self.create_peepholed(phi_rhs);

            return Some(self.create((self[op].clone(), vec![None, Some(phi_lhs), Some(phi_rhs)])));
        }
        None
    }

    fn idealize_stop(&mut self, node: NodeId) -> Option<NodeId> {
        let mut result = None;
        let mut i = 0;
        while i < self.inputs[node].len() {
            if self.ty[self.inputs[node][i].unwrap()] == Some(self.types.ty_xctrl) {
                self.del_def(node, i);
                result = Some(node);
            } else {
                i += 1;
            }
        }
        result
    }

    fn idealize_return(&mut self, node: NodeId) -> Option<NodeId> {
        self.inputs[node][0].filter(|ctrl| self.ty[*ctrl] == Some(self.types.ty_xctrl))
    }

    fn idealize_proj(&mut self, node: NodeId, index: usize) -> Option<NodeId> {
        if let Some(Type::Tuple { types: ts }) = self.ty[self.inputs[node][0]?].as_deref() {
            if ts[index] == self.types.ty_xctrl {
                return Some(
                    self.create_peepholed(Node::make_constant(self.start, self.types.ty_xctrl)),
                ); // We are dead
            }
            // Only true for IfNodes
            if ts[1 - index] == self.types.ty_xctrl {
                return self.inputs[self.inputs[node][0].unwrap()][0]; // We become our input control
            }
        }
        None
    }

    fn idealize_region(&mut self, node: NodeId) -> Option<NodeId> {
        if self.in_progress(node) {
            return None;
        }

        let path = self.find_dead_input(node)?;

        for i in 0..self.outputs[node].len() {
            let phi = self.outputs[node][i];
            if matches!(&self[phi], Node::Phi(_)) {
                self.del_def(phi, path);
            }
        }
        self.del_def(node, path);

        // If down to a single input, become that input - but also make all
        // Phis an identity on *their* single input.
        if self.inputs[node].len() == 2 {
            for i in 0..self.outputs[node].len() {
                let phi = self.outputs[node][i];
                if matches!(&self[phi], Node::Phi(_)) {
                    todo!("Currently does not happen, because no loops");
                }
            }
            return self.inputs[node][1];
        }
        Some(node)
    }

    fn find_dead_input(&self, node: NodeId) -> Option<usize> {
        (1..self.inputs[node].len())
            .find(|&i| self.ty[self.inputs[node][i].unwrap()].unwrap() == self.types.ty_xctrl)
    }

    // Compare two off-spline nodes and decide what order they should be in.
    // Do we rotate ((x + hi) + lo) into ((x + lo) + hi) ?
    // Generally constants always go right, then Phi-of-constants, then muls, then others.
    // Ties with in a category sort by node ID.
    // TRUE if swapping hi and lo.
    fn spline_cmp(&mut self, hi: NodeId, lo: NodeId) -> bool {
        if self.ty[lo].is_some_and(|t| t.is_constant()) {
            return false;
        }
        if self.ty[hi].is_some_and(|t| t.is_constant()) {
            return true;
        }

        if matches!(self[lo], Node::Phi(_)) && self.all_cons(lo) {
            return false;
        }
        if matches!(self[hi], Node::Phi(_)) && self.all_cons(hi) {
            return true;
        }

        if matches!(self[lo], Node::Phi(_)) && !matches!(self[hi], Node::Phi(_)) {
            return true;
        }
        if matches!(self[hi], Node::Phi(_)) && !matches!(self[lo], Node::Phi(_)) {
            return false;
        }

        lo.index() > hi.index()
    }

    //     // Rotation is only valid for associative ops, e.g. Add, Mul, And, Or.
    //     // Do we have ((phi cons)|(x + (phi cons)) + con|(phi cons)) ?
    //     // Push constant up through the phi: x + (phi con0+con0 con1+con1...)
    fn phi_con(&mut self, op: NodeId, rotate: bool) -> Option<NodeId> {
        let lhs = self.inputs[op][1]?;
        let rhs = self.inputs[op][2]?;

        // LHS is either a Phi of constants, or another op with Phi of constants
        let mut lphi = self.pcon(Some(lhs));
        if rotate && lphi.is_none() && self.inputs[lhs].len() > 2 {
            // Only valid to rotate constants if both are same associative ops
            if self[lhs].operation() != self[op].operation() {
                return None;
            }
            lphi = self.pcon(self.inputs[lhs][2]); // Will rotate with the Phi push
        }

        let lphi = lphi?;

        // RHS is a constant or a Phi of constants
        if !matches!(&self[rhs], Node::Constant(_)) && self.pcon(Some(rhs)).is_none() {
            return None;
        }

        // If both are Phis, must be same Region
        if matches!(&self[rhs], Node::Phi(_)) && self.inputs[lphi][0] != self.inputs[rhs][0] {
            return None;
        }

        // Note that this is the exact reverse of Phi pulling a common op down
        // to reduce total op-count.  We don't get in an endless push-up
        // push-down peephole cycle because the constants all fold first.

        let mut ns = vec![None; self.inputs[lphi].len()];
        ns[0] = self.inputs[lphi][0];

        // Push constant up through the phi: x + (phi con0+con0 con1+con1...)
        for i in 1..ns.len() {
            ns[i] = Some(self.create_peepholed((
                self[op].clone(),
                vec![
                    None,
                    self.inputs[lphi][i],
                    if matches!(&self[rhs], Node::Phi(_)) {
                        self.inputs[rhs][i]
                    } else {
                        Some(rhs)
                    },
                ],
            )));
        }

        let label = format!(
            "{}{}",
            self[lphi].phi_label().unwrap(),
            self[rhs].phi_label().unwrap_or("")
        );
        let phi = self.create_peepholed(Node::make_phi(label, ns));

        // Rotate needs another op, otherwise just the phi
        Some(if lhs == lphi {
            phi
        } else {
            self.create((
                self[lhs].clone(),
                vec![None, self.inputs[lhs][1], Some(phi)],
            ))
        })
    }
    fn pcon(&self, op: Option<NodeId>) -> Option<NodeId> {
        op.filter(|op| matches!(&self[*op], Node::Phi(_)) && self.all_cons(*op))
    }
}
