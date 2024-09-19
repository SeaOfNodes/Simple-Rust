use crate::datastructures::id::Id;
use crate::sea_of_nodes::nodes::index::{Constant, If, Phi};
use crate::sea_of_nodes::nodes::node::{MemOp, MemOpKind, PhiOp};
use crate::sea_of_nodes::nodes::{BoolOp, Node, Nodes, Op};
use crate::sea_of_nodes::types::{Int, Ty, Type};

impl<'t> Nodes<'t> {
    /// do not peephole directly returned values!
    pub(super) fn idealize(&mut self, node: Node) -> Option<Node> {
        match &self[node] {
            Op::Add => self.idealize_add(node),
            Op::Sub => self.idealize_sub(node),
            Op::Mul => self.idealize_mul(node),
            Op::Bool(op) => self.idealize_bool(*op, node),
            Op::Phi(PhiOp { ty, .. }) => self.idealize_phi(self.to_phi(node).unwrap(), *ty),
            Op::Stop => self.idealize_stop(node),
            Op::Return => self.idealize_return(node),
            Op::Proj(p) => self.idealize_proj(node, p.index),
            Op::Region { .. } | Op::Loop => self.idealize_region(node),
            Op::If => self.idealize_if(node),
            Op::Cast(t) => self.idealize_cast(node, *t),
            Op::Mem(m) => match m.kind {
                MemOpKind::Load { declared_type } => self.idealize_load(node, declared_type),
                MemOpKind::Store => self.idealize_store(node),
            },
            Op::Minus => self.idealize_minus(node),
            Op::Constant(_) | Op::Start { .. } | Op::Div | Op::Scope(_) | Op::New(_) | Op::Not => {
                None
            }
        }
    }

    fn idealize_add(&mut self, node: Node) -> Option<Node> {
        let lhs = self.inputs[node][1]?;
        let rhs = self.inputs[node][2]?;
        let t2 = self.ty[rhs]?;

        // Add of 0.  We do not check for (0+x) because this will already
        // canonicalize to (x+0)
        if t2 == self.types.ty_int_zero {
            return Some(lhs);
        }

        // Add of same to a multiply by 2
        if lhs == rhs {
            let two = Constant::new(self.types.ty_int_two, self).peephole(self);
            return Some(self.create(Op::make_mul([lhs, two])));
        }

        // Goal: a left-spine set of adds, with constants on the rhs (which then fold).

        // Move non-adds to RHS
        if !matches!(self[lhs], Op::Add) && matches!(self[rhs], Op::Add) {
            return Some(node.swap_12(self));
        }

        // x+(-y) becomes x-y
        if matches!(&self[rhs], Op::Minus) {
            let y = self.inputs[rhs][1].unwrap();
            return Some(self.create(Op::make_sub([lhs, y])));
        }

        // Now we might see (add add non) or (add non non) or (add add add) but never (add non add)

        // Do we have  x + (y + z) ?
        // Swap to    (x + y) + z
        // Rotate (add add add) to remove the add on RHS
        if let Op::Add = &self[rhs] {
            let x = lhs;
            let y = self.inputs[rhs][1]?;
            let z = self.inputs[rhs][2]?;
            let new_lhs = self.create_peepholed(Op::make_add([x, y]));
            return Some(self.create(Op::make_add([new_lhs, z])));
        }

        // Now we might see (add add non) or (add non non) but never (add non add) nor (add add add)
        if !matches!(self[lhs], Op::Add) {
            return if self.spine_cmp(lhs, rhs, node) {
                Some(node.swap_12(self))
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
        // lhs.in(2) is con1 here
        // If lhs.in(2) is not a constant, we add ourselves as a dependency
        // because if it later became a constant then we could make this
        // transformation.
        self.add_dep(self.inputs[lhs][2]?, node);
        if self.ty[self.inputs[lhs][2]?]?.is_constant() && t2.is_constant() {
            let x = self.inputs[lhs][1]?;
            let con1 = self.inputs[lhs][2]?;
            let con2 = rhs;

            let new_rhs = self.create_peepholed(Op::make_add([con1, con2]));
            return Some(self.create(Op::make_add([x, new_rhs])));
        }

        // Do we have ((x + (phi cons)) + con) ?
        // Do we have ((x + (phi cons)) + (phi cons)) ?
        // Push constant up through the phi: x + (phi con0+con0 con1+con1...)
        let phicon = self.phi_con(node, true);
        if phicon.is_some() {
            return phicon;
        }

        // Now we sort along the spine via rotates, to gather similar things together.

        // Do we rotate (x + y) + z
        // into         (x + z) + y ?
        if self.spine_cmp(self.inputs[lhs][2]?, rhs, node) {
            // return new AddNode(new AddNode(lhs.in(1), rhs).peephole(), lhs.in(2));
            let x = self.inputs[lhs][1]?;
            let y = self.inputs[lhs][2]?;
            let z = rhs;

            let new_lhs = self.create_peepholed(Op::make_add([x, z]));
            return Some(self.create(Op::make_add([new_lhs, y])));
        }

        None
    }

    fn idealize_sub(&mut self, node: Node) -> Option<Node> {
        // Sub of same is 0
        if self.inputs[node][1]? == self.inputs[node][2]? {
            return Some(*Constant::new(self.types.ty_int_zero, self));
        }

        // x - (-y) is x+y
        if let Some(minus) = self.to_minus(self.inputs[node][2]) {
            return Some(self.create(Op::make_add([
                node.inputs(self)[1].unwrap(),
                minus.inputs(self)[1].unwrap(),
            ])));
        }

        // (-x) - y is -(x+y)
        if let Some(minus) = self.to_minus(node.inputs(self)[1]) {
            let add = self.create_peepholed(Op::make_add([
                minus.inputs(self)[1].unwrap(),
                node.inputs(self)[2].unwrap(),
            ]));
            return Some(self.create(Op::make_minus(add)));
        }

        None
    }

    fn idealize_mul(&mut self, node: Node) -> Option<Node> {
        let left = self.inputs[node][1]?;
        let right = self.inputs[node][2]?;
        let left_ty = self.ty[left]?;
        let right_ty = self.ty[right]?;

        if matches!(&*right_ty, Type::Int(Int::Constant(1))) {
            Some(left)
        } else if left_ty.is_constant() && !right_ty.is_constant() {
            node.swap_12(self);
            Some(node)
        } else {
            // Do we have ((x * (phi cons)) * con) ?
            // Do we have ((x * (phi cons)) * (phi cons)) ?
            // Push constant up through the phi: x * (phi con0*con0 con1*con1...)
            self.phi_con(node, true)
        }
    }

    fn idealize_bool(&mut self, op: BoolOp, node: Node) -> Option<Node> {
        if self.inputs[node][1]? == self.inputs[node][2]? {
            let value = if op.compute(3, 3) {
                self.types.ty_int_one
            } else {
                self.types.ty_int_zero
            };
            return Some(*Constant::new(value, self));
        }

        // Equals pushes constant to the right; 5==X becomes X==5.
        if op == BoolOp::EQ {
            let lhs = node.inputs(self)[1].unwrap();
            let rhs = node.inputs(self)[2].unwrap();

            if rhs.to_constant(self).is_none() {
                // con==noncon becomes noncon==con
                if lhs.to_constant(self).is_some() {
                    return Some(self.create(Op::make_bool([rhs, lhs], op)));
                } else if lhs.index() > rhs.index() {
                    // Equals sorts by NID otherwise: non.high == non.low becomes non.low == non.high
                    return Some(self.create(Op::make_bool([rhs, lhs], op)));
                }
            }
            // Equals X==0 becomes a !X
            if rhs.ty(self) == Some(self.types.ty_int_zero)
                || rhs.ty(self) == Some(self.types.ty_pointer_null)
            {
                return Some(self.create(Op::make_not(lhs)));
            }
        }

        // Do we have ((x * (phi cons)) * con) ?
        // Do we have ((x * (phi cons)) * (phi cons)) ?
        // Push constant up through the phi: x * (phi con0*con0 con1*con1...)
        let phicon = self.phi_con(node, op == BoolOp::EQ);
        phicon
    }

    fn idealize_phi(&mut self, node: Phi, declared_ty: Ty<'t>) -> Option<Node> {
        let region = self.inputs[node][0];
        if !self.instanceof_region(region) {
            return self.inputs[node][1]; // Input has collapse to e.g. starting control.
        }
        if Self::in_progress(&self.ops, &self.inputs, *node)
            || self.inputs[region.unwrap()].is_empty()
        {
            return None; // Input is in-progress
        }

        // If we have only a single unique input, become it.
        if let live @ Some(_) = self.single_unique_input(*node) {
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
            && self.same_op(*node)
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

            let label = self[node].label;
            let phi_lhs = self.create_peepholed(Op::make_phi(label, declared_ty, lhss));
            let phi_rhs = self.create_peepholed(Op::make_phi(label, declared_ty, rhss));

            return Some(self.create((self[op].clone(), vec![None, Some(phi_lhs), Some(phi_rhs)])));
        }

        // If merging Phi(N, cast(N)) - we are losing the cast JOIN effects, so just remove.
        if self.inputs[node].len() == 3 {
            if let Some(cast) = self.to_cast(self.inputs[node][1]) {
                let in_1 = self.inputs[cast][1];
                self.add_dep(in_1.unwrap(), *node);
                if in_1 == self.inputs[node][2] {
                    return self.inputs[node][2];
                }
            }
            if let Some(cast) = self.to_cast(self.inputs[node][2]) {
                let in_1 = self.inputs[cast][1];
                self.add_dep(in_1.unwrap(), *node);
                if in_1 == self.inputs[node][1] {
                    return self.inputs[node][1];
                }
            }
        }

        // If merging a null-checked null and the checked value, just use the value.
        // if( val ) ..; phi(Region,False=0/null,True=val);
        // then replace with plain val.
        if self.inputs[node].len() == 3 {
            let mut nullx = 0;

            let t1 = node.inputs(self)[1].unwrap().ty(self).unwrap();
            if Some(t1) == self.types.make_init(t1) {
                nullx = 1;
            }
            let t2 = node.inputs(self)[2].unwrap().ty(self).unwrap();
            if Some(t2) == self.types.make_init(t2) {
                nullx = 2;
            }

            if nullx != 0 {
                let val = node.inputs(self)[3 - nullx].unwrap();
                let region = node.inputs(self)[0].unwrap();
                let idom = self.idom(region);
                if let Some(iff) = self.to_if(idom) {
                    let pred = iff.inputs(self)[1].unwrap();
                    self.add_dep(pred, *node);
                    if pred == val {
                        // Must walk the idom on the null side to make sure we hit False.
                        let mut idom = region.inputs(self)[nullx].unwrap();
                        while idom.inputs(self)[0] != Some(*iff) {
                            idom = self.idom(idom).unwrap();
                        }
                        if self.to_proj(idom).is_some_and(|p| self[p].index == 1) {
                            return Some(val);
                        }
                    }
                }
            }
        }

        None
    }

    fn idealize_stop(&mut self, node: Node) -> Option<Node> {
        let mut result = None;
        let mut i = 0;
        while i < self.inputs[node].len() {
            if self.ty[self.inputs[node][i].unwrap()] == Some(self.types.ty_xctrl) {
                node.del_def(i, self);
                result = Some(node);
            } else {
                i += 1;
            }
        }
        result
    }

    fn idealize_return(&mut self, node: Node) -> Option<Node> {
        self.inputs[node][0].filter(|ctrl| self.ty[*ctrl] == Some(self.types.ty_xctrl))
    }

    fn idealize_proj(&mut self, node: Node, index: usize) -> Option<Node> {
        if let Some(Type::Tuple { types: ts }) = self.ty[self.inputs[node][0]?].as_deref() {
            if ts[index] == self.types.ty_xctrl {
                return Some(*Constant::new(self.types.ty_xctrl, self)); // We are dead
            }
            // Only true for IfNodes
            if node.inputs(self)[0].unwrap().to_if(self).is_some()
                && ts[1 - index] == self.types.ty_xctrl
            {
                return self.inputs[self.inputs[node][0].unwrap()][0]; // We become our input control
            }
        }

        // Flip a negating if-test, to remove the not
        if let Some(iff) = node.inputs(self)[0]?.to_if(self) {
            let pred = iff.inputs(self)[1].unwrap();
            self.add_dep(pred, node);
            if let Some(not) = pred.to_not(self) {
                let i = If::new(
                    iff.inputs(self)[0].unwrap(),
                    not.inputs(self)[1].unwrap(),
                    self,
                )
                .peephole(self);
                return Some(self.create(Op::make_proj(
                    i,
                    1 - index,
                    if index == 0 { "False" } else { "True" },
                )));
            }
        }

        None
    }

    fn idealize_region(&mut self, node: Node) -> Option<Node> {
        if Self::in_progress(&self.ops, &self.inputs, node) {
            return None;
        }

        // Delete dead paths into a Region
        if let Some(path) = self.find_dead_input(node) {
            // Do not delete the entry path of a loop (ok to remove the back
            // edge and make the loop a single-entry Region which folds away
            // the Loop).  Folding the entry path confused the loop structure,
            // moving the backedge to the entry point.
            if !(matches!(&self[node], Op::Loop) && self.inputs[node][1] == self.inputs[node][path])
            {
                // Cannot use the obvious output iterator here, because a Phi
                // deleting an input might recursively delete *itself*.  This
                // shuffles the output array, and we might miss iterating an
                // unrelated Phi. So on rare occasions we repeat the loop to get
                // all the Phis.
                let mut nouts = 0;
                while nouts != self.outputs[node].len() {
                    nouts = self.outputs[node].len();

                    for i in 0.. {
                        if i >= self.outputs[node].len() {
                            break;
                        }
                        let phi = self.outputs[node][i];
                        if matches!(&self[phi], Op::Phi(_))
                            && self.inputs[node].len() == self.inputs[phi].len()
                        {
                            phi.del_def(path, self);
                            for &o in &self.outputs[phi] {
                                self.iter_peeps.add(o);
                            }
                        }
                    }
                }

                return if node.is_dead(self) {
                    Some(*Constant::new(self.types.ty_xctrl, self))
                } else {
                    node.del_def(path, self);
                    Some(node)
                };
            }
        }

        // If down to a single input, become that input - but also make all
        if self.inputs[node].len() == 2 && !self.has_phi(node) {
            return self.inputs[node][1]; // Collapse if no Phis; 1-input Phis will collapse on their own
        }

        // If a CFG diamond with no merging, delete: "if( pred ) {} else {};"
        if !self.has_phi(node) {
            // No Phi users, just a control user
            if let Some(p1) = node.inputs(self)[1].and_then(|n| n.to_proj(self)) {
                if let Some(p2) = node.inputs(self)[2].and_then(|n| n.to_proj(self)) {
                    if p1.inputs(self)[0] == p2.inputs(self)[0] {
                        if let Some(iff) = p1.inputs(self)[0].and_then(|n| n.to_if(self)) {
                            return iff.inputs(self)[0];
                        }
                    }
                }
            }
        }

        None
    }

    fn has_phi(&self, node: Node) -> bool {
        self.outputs[node]
            .iter()
            .any(|phi| matches!(&self[*phi], Op::Phi(_)))
    }

    fn idealize_if(&mut self, node: Node) -> Option<Node> {
        // Hunt up the immediate dominator tree.  If we find an identical if
        // test on either the true or false branch, that side wins.
        let pred = self.inputs[node][1]?;
        if !self.ty[pred]?.is_high_or_constant() {
            let mut prior = node;
            let mut dom = self.idom(node);
            while let Some(d) = dom {
                self.add_dep(d, node);
                if matches!(&self[d], Op::If) {
                    let if_pred = self.inputs[d][1]?;
                    self.add_dep(if_pred, node);
                    if if_pred == pred {
                        if let Op::Proj(p) = &self[prior] {
                            let value = if p.index == 0 {
                                self.types.ty_int_one
                            } else {
                                self.types.ty_int_zero
                            };
                            let new_constant = Constant::new(value, self).peephole(self);
                            node.set_def(1, Some(new_constant), self);
                            return Some(node);
                        }
                    }
                }
                prior = d;
                dom = self.idom(d);
            }
        }
        None
    }

    fn idealize_cast(&mut self, node: Node, ty: Ty<'t>) -> Option<Node> {
        self.inputs[node][1].filter(|&n| self.ty[n].is_some_and(|t| self.types.isa(t, ty)))
    }

    fn idealize_load(&mut self, node: Node, declared_ty: Ty<'t>) -> Option<Node> {
        let mem = self.inputs[node][1]?;
        let ptr = self.inputs[node][2]?;

        // Simple Load-after-Store on same address.
        if let Op::Mem(MemOp {
            kind: MemOpKind::Store,
            name,
            ..
        }) = &self[mem]
        {
            let store_ptr = self.inputs[mem][2]?;
            // Must check same object
            if ptr == store_ptr {
                let Op::Mem(mem_op) = &self[node] else {
                    unreachable!()
                };
                debug_assert_eq!(name, &mem_op.name); // Equiv class aliasing is perfect
                return self.inputs[mem][3]; // store value
            }
        }

        // Push a Load up through a Phi, as long as it collapses on at least
        // one arm.  If at a Loop, the backedge MUST collapse - else we risk
        // spinning the same transform around the loop indefinitely.
        //   BEFORE (2 Sts, 1 Ld):          AFTER (1 St, 0 Ld):
        //   if( pred ) ptr.x = e0;         val = pred ? e0
        //   else       ptr.x = e1;                    : e1;
        //   val = ptr.x;                   ptr.x = val;
        if matches!(&self[mem], Op::Phi(_))
            && self.ty[self.inputs[mem][0]?] == Some(self.types.ty_ctrl)
            && self.inputs[mem].len() == 3
        {
            // Profit on RHS/Loop backedge
            if self.profit(node, mem, 2) ||
                // Else must not be a loop to count profit on LHS.
                (!matches!(self[self.inputs[mem][0].unwrap()], Op::Loop) && self.profit(node, mem, 1))
            {
                let Op::Mem(mem_op) = &self[node] else {
                    unreachable!()
                };
                let name = mem_op.name;
                let alias = mem_op.alias;

                let ld1 = self.create_peepholed(Op::make_load(
                    name,
                    alias,
                    declared_ty,
                    [self.inputs[mem][1].unwrap(), ptr],
                ));
                let ld2 = self.create_peepholed(Op::make_load(
                    name,
                    alias,
                    declared_ty,
                    [self.inputs[mem][2].unwrap(), ptr],
                ));

                return Some(self.create_peepholed(Op::make_phi(
                    name,
                    self.ty[node].unwrap(),
                    vec![self.inputs[mem][0], Some(ld1), Some(ld2)],
                )));
            }
        }

        None
    }

    /// Profitable if we find a matching Store on this Phi arm.
    fn profit(&mut self, this: Node, phi_node: Node, idx: usize) -> bool {
        let px = self.inputs[phi_node][idx];
        px.is_some_and(|px| {
            self.add_dep(px, this);
            if let Op::Mem(MemOp {
                kind: MemOpKind::Store,
                ..
            }) = &self[px]
            {
                self.inputs[this][2] == self.inputs[px][2] // same ptr
            } else {
                false
            }
        })
    }

    fn idealize_store(&mut self, node: Node) -> Option<Node> {
        // Simple store-after-store on same address.  Should pick up the
        // required init-store being stomped by a first user store.
        let mem = node.inputs(self)[1]?;
        let ptr = node.inputs(self)[2]?;

        if let Op::Mem(MemOp {
            kind: MemOpKind::Store,
            ..
        }) = self[mem]
        {
            let store_ptr = self.inputs[mem][2]?;
            // Must check same object
            if ptr == store_ptr {
                // No bother if weird dead pointers
                if let Some(Type::Pointer(_)) = ptr.ty(self).as_deref() {
                    // Must have exactly one use of "this" or you get weird
                    // non-serializable memory effects in the worse case.
                    if Self::check_no_use_beyond(mem, node, self) {
                        debug_assert!({
                            let x = node.to_mem_op(self).unwrap();
                            let y = mem.to_mem_op(self).unwrap();
                            self[x].name == self[y].name // Equiv class aliasing is perfect
                        });
                        let st_mem = mem.inputs(self)[1];
                        node.set_def(1, st_mem, self);
                        return Some(node);
                    }
                }
            }
        }
        None
    }

    // Check that `this` has no uses beyond `that`
    fn check_no_use_beyond(this: Node, that: Node, sea: &mut Nodes) -> bool {
        let n_outs = sea.outputs[this].len();
        if n_outs == 1 {
            return true;
        }
        // Add deps on the other uses (can be e.g. ScopeNode mid-parse) so that
        // when the other uses go away we can retry.
        for i in 0..n_outs {
            let use_ = sea.outputs[this][i];
            if use_ != that {
                sea.add_dep(use_, that);
            }
        }
        false
    }

    fn idealize_minus(&mut self, node: Node) -> Option<Node> {
        // -(-x) is x
        let in_1 = self.inputs[node][1]?;
        if self.to_minus(in_1).is_some() {
            return self.inputs[in_1][1];
        }
        None
    }

    fn find_dead_input(&self, node: Node) -> Option<usize> {
        (1..self.inputs[node].len())
            .find(|&i| self.ty[self.inputs[node][i].unwrap()].unwrap() == self.types.ty_xctrl)
    }

    // Compare two off-spine nodes and decide what order they should be in.
    // Do we rotate ((x + hi) + lo) into ((x + lo) + hi) ?
    // Generally constants always go right, then Phi-of-constants, then muls, then others.
    // Ties with in a category sort by node ID.
    // TRUE if swapping hi and lo.
    fn spine_cmp(&mut self, hi: Node, lo: Node, dep: Node) -> bool {
        if self.ty[lo].is_some_and(|t| t.is_constant()) {
            return false;
        }
        if self.ty[hi].is_some_and(|t| t.is_constant()) {
            return true;
        }

        if matches!(self[lo], Op::Phi(_))
            && self.ty[self.inputs[lo][0].unwrap()] == Some(self.types.ty_xctrl)
        {
            return false;
        }
        if matches!(self[hi], Op::Phi(_))
            && self.ty[self.inputs[hi][0].unwrap()] == Some(self.types.ty_xctrl)
        {
            return false;
        }

        if matches!(self[lo], Op::Phi(_)) && self.all_cons(lo, dep) {
            return false;
        }
        if matches!(self[hi], Op::Phi(_)) && self.all_cons(hi, dep) {
            return true;
        }

        if matches!(self[lo], Op::Phi(_)) && !matches!(self[hi], Op::Phi(_)) {
            return true;
        }
        if matches!(self[hi], Op::Phi(_)) && !matches!(self[lo], Op::Phi(_)) {
            return false;
        }

        lo.index() > hi.index()
    }

    //     // Rotation is only valid for associative ops, e.g. Add, Mul, And, Or.
    //     // Do we have ((phi cons)|(x + (phi cons)) + con|(phi cons)) ?
    //     // Push constant up through the phi: x + (phi con0+con0 con1+con1...)
    fn phi_con(&mut self, op: Node, rotate: bool) -> Option<Node> {
        let lhs = self.inputs[op][1]?;
        let rhs = self.inputs[op][2]?;

        // LHS is either a Phi of constants, or another op with Phi of constants
        let mut lphi = self.pcon(Some(lhs), op);
        if rotate && lphi.is_none() && self.inputs[lhs].len() > 2 {
            // Only valid to rotate constants if both are same associative ops
            if self[lhs].operation() != self[op].operation() {
                return None;
            }
            lphi = self.pcon(self.inputs[lhs][2], op); // Will rotate with the Phi push
        }

        let lphi = lphi?;
        let lphi = self.to_phi(lphi).unwrap();

        // RHS is a constant or a Phi of constants
        if !matches!(&self[rhs], Op::Constant(_)) && self.pcon(Some(rhs), op).is_none() {
            return None;
        }

        // If both are Phis, must be same Region
        if matches!(&self[rhs], Op::Phi(_)) && self.inputs[lphi][0] != self.inputs[rhs][0] {
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
                    if matches!(&self[rhs], Op::Phi(_)) {
                        self.inputs[rhs][i]
                    } else {
                        Some(rhs)
                    },
                ],
            )));
        }

        let label = format!(
            "{}{}",
            self[lphi].label,
            self.to_phi(rhs).map(|p| self[p].label).unwrap_or("")
        );
        let label = self.types.get_str(&label);
        let ty = self[lphi].ty;
        let phi = self.create_peepholed(Op::make_phi(label, ty, ns));

        // Rotate needs another op, otherwise just the phi
        Some(if lhs == *lphi {
            phi
        } else {
            self.create((
                self[lhs].clone(),
                vec![None, self.inputs[lhs][1], Some(phi)],
            ))
        })
    }

    /// Tests if the op is a phi and has all constant inputs.
    /// If not, returns null.
    /// If op is a phi, but its inputs are not all constants, then dep is added as
    /// a dependency to the phi's non-const input, because if later the input turn into a constant
    /// dep can make progress.
    fn pcon(&mut self, op: Option<Node>, dep: Node) -> Option<Node> {
        op.filter(|op| matches!(&self[*op], Op::Phi(_)) && self.all_cons(*op, dep))
    }
}
