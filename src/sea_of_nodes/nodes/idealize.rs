use crate::datastructures::id::Id;
use crate::sea_of_nodes::nodes::{BoolOp, Node, NodeId, Nodes};
use crate::sea_of_nodes::types::{Int, Type, Types};

impl<'t> Nodes<'t> {
    /// do not peephole directly returned values!
    pub(super) fn idealize(&mut self, node: NodeId, types: &mut Types<'t>) -> Option<NodeId> {
        match &self[node] {
            Node::Add => self.idealize_add(node, types),
            Node::Sub => self.idealize_sub(node, types),
            Node::Mul => self.idealize_mul(node, types),
            Node::Bool(op) => self.idealize_bool(*op, node, types),
            Node::Phi(_) => self.idealize_phi(node, types),
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

    fn idealize_add(&mut self, node: NodeId, types: &mut Types<'t>) -> Option<NodeId> {
        let lhs = self.inputs[node][1]?;
        let rhs = self.inputs[node][2]?;
        let t1 = self.ty[lhs]?; // TODO is it safe to ignore this being None?
        let t2 = self.ty[rhs]?;

        // Already handled by peephole constant folding
        debug_assert!(!t1.is_constant() || !t2.is_constant(), "{t1} {t2}");

        // Add of 0.  We do not check for (0+x) because this will already
        // canonicalize to (x+0)
        if t2 == types.ty_zero {
            return Some(lhs);
        }

        // Add of same to a multiply by 2
        if lhs == rhs {
            let two = self.create_peepholed(types, Node::make_constant(self.start, types.ty_two));
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
            let new_lhs = self.create_peepholed(types, Node::make_add([x, y]));
            return Some(self.create(Node::make_add([new_lhs, z])));
        }

        // Now we might see (add add non) or (add non non) but never (add non add) nor (add add add)
        if !matches!(self[lhs], Node::Add) {
            if self.spline_cmp(lhs, rhs) {
                return Some(self.swap_12(node));
            } else {
                return None;
            }
        }

        // Now we only see (add add non)

        // Do we have (x + con1) + con2?
        // Replace with (x + (con1+con2) which then fold the constants
        if self.ty[self.inputs[lhs][2]?]?.is_constant() && t2.is_constant() {
            let x = self.inputs[lhs][1]?;
            let con1 = self.inputs[lhs][2]?;
            let con2 = rhs;

            let new_rhs = self.create_peepholed(types, Node::make_add([con1, con2]));
            return Some(self.create(Node::make_add([x, new_rhs])));
        }

        {
            let phi = self.inputs[lhs][2]?;
            if matches!(self[phi], Node::Phi(_)) && self.all_cons(phi) &&
            // Do we have ((x + (phi cons)) + con) ?
            // Do we have ((x + (phi cons)) + (phi cons)) ?
            // Push constant up through the phi: x + (phi con0+con0 con1+con1...)

            // Note that this is the exact reverse of Phi pulling a common op
            // down to reduce total op-count.  We don't get in an endless push-
            // up push-down peephole cycle because the constants all fold first.
                (t2.is_constant() || (matches!(&self[rhs], Node::Phi(_)) && self.inputs[phi][0] == self.inputs[rhs][0] && self.all_cons(rhs) ))
            {
                let mut ns = vec![None; self.inputs[phi].len()];
                ns[0] = self.inputs[phi][0];

                // Push constant up through the phi: x + (phi con0+con0 con1+con1...)
                for i in 1..ns.len() {
                    //                 ns[i] = new AddNode(phi.in(i),t2.isConstant() ? rhs : rhs.in(i)).peephole();
                    ns[i] = Some(self.create_peepholed(
                        types,
                        Node::make_add([
                            self.inputs[phi][i]?,
                            if t2.is_constant() {
                                rhs
                            } else {
                                self.inputs[rhs][i]?
                            },
                        ]),
                    ));
                }

                let label = format!(
                    "{}{}",
                    self[phi].phi_label().unwrap(),
                    self[rhs].phi_label().unwrap_or("")
                );
                let new_phi = self.create_peepholed(types, Node::make_phi(label, ns));
                return Some(self.create(Node::make_add([self.inputs[lhs][1]?, new_phi])));
            }
        }

        // Now we sort along the spline via rotates, to gather similar things together.

        // Do we rotate (x + y) + z
        // into         (x + z) + y ?
        if self.spline_cmp(self.inputs[lhs][2]?, rhs) {
            // return new AddNode(new AddNode(lhs.in(1), rhs).peephole(), lhs.in(2));
            let x = self.inputs[lhs][1]?;
            let y = self.inputs[lhs][2]?;
            let z = rhs;

            let new_lhs = self.create_peepholed(types, Node::make_add([x, z]));
            return Some(self.create(Node::make_add([new_lhs, y])));
        }

        None
    }

    fn idealize_sub(&mut self, node: NodeId, types: &mut Types<'t>) -> Option<NodeId> {
        if self.inputs[node][1]? == self.inputs[node][2]? {
            Some(self.create(Node::make_constant(self.start, types.ty_zero)))
        } else {
            None
        }
    }

    fn idealize_mul(&mut self, node: NodeId, _types: &mut Types<'t>) -> Option<NodeId> {
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
            None
        }
    }

    fn idealize_bool(&mut self, op: BoolOp, node: NodeId, types: &mut Types<'t>) -> Option<NodeId> {
        if self.inputs[node][1]? == self.inputs[node][2]? {
            let value = if op.compute(3, 3) {
                types.ty_one
            } else {
                types.ty_zero
            };
            Some(self.create(Node::make_constant(self.start, value)))
        } else {
            None
        }
    }

    fn idealize_phi(&mut self, node: NodeId, types: &mut Types<'t>) -> Option<NodeId> {
        // Remove a "junk" Phi: Phi(x,x) is just x
        if self.same_inputs(node) {
            return self.inputs[node][1];
        }

        // Pull "down" a common data op.  One less op in the world.  One more
        // Phi, but Phis do not make code.
        //   Phi(op(A,B),op(Q,R),op(X,Y)) becomes
        //     op(Phi(A,Q,X), Phi(B,R,Y)).
        let op = self.inputs[node][1].expect("not same_inputs");
        if self.inputs[op].len() == 3 && self.inputs[op][0].is_none() && !self.is_cfg(op) && self.same_op(node) {
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
            
            let phi_lhs = self.create_peepholed(types, phi_lhs);
            let phi_rhs = self.create_peepholed(types, phi_rhs);
            
            return Some(self.create((self[op].clone(), vec![None, Some(phi_lhs), Some(phi_rhs)])));
        }
        None
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
}
