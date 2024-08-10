use crate::datastructures::id::Id;
use crate::soup::nodes::{BoolOp, Node, NodeId, Nodes};
use crate::soup::types::{Int, Type, Types};

impl<'t> Nodes<'t> {
    /// do not peephole directly returned values!
    pub(super) fn idealize(&mut self, node: NodeId, types: &mut Types<'t>) -> Option<NodeId> {
        match &self[node] {
            Node::Add => self.idealize_add(node, types),
            Node::Sub => self.idealize_sub(node, types),
            Node::Mul => self.idealize_mul(node, types),
            Node::Bool(op) => self.idealize_bool(*op, node, types),
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

    // Compare two off-spline nodes and decide what order they should be in.
    // Do we rotate ((x + hi) + lo) into ((x + lo) + hi) ?
    // Generally constants always go right, then others.
    // Ties with in a category sort by node ID.
    // TRUE if swapping hi and lo.
    fn spline_cmp(&mut self, hi: NodeId, lo: NodeId) -> bool {
        if self.ty[lo].is_some_and(|t| t.is_constant()) {
            return false;
        }
        if self.ty[hi].is_some_and(|t| t.is_constant()) {
            return true;
        }
        lo.index() > hi.index()
    }
}
