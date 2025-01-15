use crate::datastructures::id::Id;
use crate::sea_of_nodes::nodes::index::{
    Add, Bool, CProj, Cast, Constant, Div, If, Load, Minus, Mul, Not, Phi, Return, Stop, Store,
    Sub, TypedNode,
};
use crate::sea_of_nodes::nodes::node::IfOp;
use crate::sea_of_nodes::nodes::{BoolOp, Node, Nodes, Op};
use crate::sea_of_nodes::types::{Int, Type};

impl Node {
    /// do not peephole directly returned values!
    pub(super) fn idealize(self, sea: &mut Nodes) -> Option<Node> {
        let s = self;
        match self.downcast(&sea.ops) {
            TypedNode::Add(n) => n.idealize_add(sea),
            TypedNode::Sub(n) => n.idealize_sub(sea),
            TypedNode::Mul(n) => n.idealize_mul(sea),
            TypedNode::Bool(n) => n.idealize_bool(sea),
            TypedNode::Phi(n) => n.idealize_phi(sea),
            TypedNode::Stop(n) => n.idealize_stop(sea),
            TypedNode::Return(n) => n.idealize_return(sea),
            TypedNode::Proj(_) => None,
            TypedNode::CProj(n) => n.idealize_cproj(sea),
            TypedNode::Region(_) | TypedNode::Loop(_) => sea.idealize_region(s),
            TypedNode::If(_) => sea.idealize_if(s),
            TypedNode::Cast(n) => n.idealize_cast(sea),
            TypedNode::Load(n) => n.idealize_load(sea),
            TypedNode::Store(n) => n.idealize_store(sea),
            TypedNode::Minus(n) => n.idealize_minus(sea),
            TypedNode::Div(n) => n.idealize_div(sea),
            TypedNode::Constant(_)
            | TypedNode::XCtrl(_)
            | TypedNode::Start(_)
            | TypedNode::Scope(_)
            | TypedNode::New(_)
            | TypedNode::Not(_) => None,
        }
    }
}

impl Add {
    fn idealize_add(self, sea: &mut Nodes) -> Option<Node> {
        let lhs = self.inputs(sea)[1]?;
        let rhs = self.inputs(sea)[2]?;
        let t2 = rhs.ty(sea)?;

        // Add of 0.  We do not check for (0+x) because this will already
        // canonicalize to (x+0)
        if t2 == sea.types.ty_int_zero {
            return Some(lhs);
        }

        // Add of same to a multiply by 2
        if lhs == rhs {
            let two = Constant::new(sea.types.ty_int_two, sea).peephole(sea);
            return Some(*Mul::new(lhs, two, sea));
        }

        // Goal: a left-spine set of adds, with constants on the rhs (which then fold).

        // Move non-adds to RHS
        if !matches!(sea[lhs], Op::Add) && matches!(sea[rhs], Op::Add) {
            return Some(self.swap_12(sea));
        }

        // x+(-y) becomes x-y
        if matches!(&sea[rhs], Op::Minus) {
            let y = rhs.inputs(sea)[1].unwrap();
            return Some(*Sub::new(lhs, y, sea));
        }

        // Now we might see (add add non) or (add non non) or (add add add) but never (add non add)

        // Do we have  x + (y + z) ?
        // Swap to    (x + y) + z
        // Rotate (add add add) to remove the add on RHS
        if let Op::Add = &sea[rhs] {
            let x = lhs;
            let y = rhs.inputs(sea)[1]?;
            let z = rhs.inputs(sea)[2]?;
            return Some(*Add::new(Add::new(x, y, sea).peephole(sea), z, sea));
        }

        // Now we might see (add add non) or (add non non) but never (add non add) nor (add add add)
        if !matches!(sea[lhs], Op::Add) {
            return if sea.spine_cmp(lhs, rhs, *self) {
                Some(self.swap_12(sea))
            } else {
                sea.phi_con(*self, true)
            };
        }

        // Now we only see (add add non)

        // Dead data cycle; comes about from dead infinite loops.  Do nothing,
        // the loop will peep as dead after a bit.
        if lhs.inputs(sea)[1] == Some(lhs) {
            return None;
        }

        // Do we have (x + con1) + con2?
        // Replace with (x + (con1+con2) which then fold the constants
        // lhs.in(2) is con1 here
        // If lhs.in(2) is not a constant, we add ourselves as a dependency
        // because if it later became a constant then we could make this
        // transformation.
        lhs.inputs(sea)[2]?.add_dep(*self, sea);
        if lhs.inputs(sea)[2].unwrap().ty(sea).unwrap().is_constant() && t2.is_constant() {
            let x = lhs.inputs(sea)[1]?;
            let con1 = lhs.inputs(sea)[2]?;
            let con2 = rhs;
            return Some(*Add::new(x, Add::new(con1, con2, sea).peephole(sea), sea));
        }

        // Do we have ((x + (phi cons)) + con) ?
        // Do we have ((x + (phi cons)) + (phi cons)) ?
        // Push constant up through the phi: x + (phi con0+con0 con1+con1...)
        let phicon = sea.phi_con(*self, true);
        if phicon.is_some() {
            return phicon;
        }

        // Now we sort along the spine via rotates, to gather similar things together.

        // Do we rotate (x + y) + z
        // into         (x + z) + y ?
        if sea.spine_cmp(lhs.inputs(sea)[2]?, rhs, *self) {
            // return new AddNode(new AddNode(lhs.in(1), rhs).peephole(), lhs.in(2));
            let x = lhs.inputs(sea)[1]?;
            let y = lhs.inputs(sea)[2]?;
            let z = rhs;
            return Some(*Add::new(Add::new(x, z, sea).peephole(sea), y, sea));
        }

        None
    }
}

impl Sub {
    fn idealize_sub(self, sea: &mut Nodes) -> Option<Node> {
        // x - (-y) is x+y
        if let Some(minus) = self.inputs(sea)[2].and_then(|n| n.to_minus(sea)) {
            return Some(*Add::new(
                self.inputs(sea)[1].unwrap(),
                minus.inputs(sea)[1].unwrap(),
                sea,
            ));
        }

        // (-x) - y is -(x+y)
        if let Some(minus) = self.inputs(sea)[1].and_then(|n| n.to_minus(sea)) {
            return Some(*Minus::new(
                Add::new(
                    minus.inputs(sea)[1].unwrap(),
                    self.inputs(sea)[2].unwrap(),
                    sea,
                )
                .peephole(sea),
                sea,
            ));
        }

        None
    }
}

impl Mul {
    fn idealize_mul(self, sea: &mut Nodes) -> Option<Node> {
        let left = self.inputs(sea)[1]?;
        let right = self.inputs(sea)[2]?;
        let left_ty = left.ty(sea)?;
        let right_ty = right.ty(sea)?;

        if matches!(&*right_ty, Type::Int(Int::Constant(1))) {
            Some(left)
        } else if left_ty.is_constant() && !right_ty.is_constant() {
            self.swap_12(sea);
            Some(*self)
        } else {
            // Do we have ((x * (phi cons)) * con) ?
            // Do we have ((x * (phi cons)) * (phi cons)) ?
            // Push constant up through the phi: x * (phi con0*con0 con1*con1...)
            sea.phi_con(*self, true)
        }
    }
}

impl Bool {
    fn idealize_bool(self, sea: &mut Nodes) -> Option<Node> {
        let op = sea[self];
        if self.inputs(sea)[1]? == self.inputs(sea)[2]? {
            let value = if op.compute(3, 3) {
                sea.types.ty_int_one
            } else {
                sea.types.ty_int_zero
            };
            return Some(*Constant::new(value, sea));
        }

        // Equals pushes constant to the right; 5==X becomes X==5.
        if op == BoolOp::EQ {
            let lhs = self.inputs(sea)[1].unwrap();
            let rhs = self.inputs(sea)[2].unwrap();

            if rhs.to_constant(sea).is_none() {
                // con==noncon becomes noncon==con
                if lhs.to_constant(sea).is_some() {
                    return Some(*Bool::new(rhs, lhs, op, sea));
                } else if lhs.index() > rhs.index() {
                    // Equals sorts by NID otherwise: non.high == non.low becomes non.low == non.high
                    return Some(*Bool::new(rhs, lhs, op, sea));
                }
            }
            // Equals X==0 becomes a !X
            if rhs.ty(sea) == Some(sea.types.ty_int_zero)
                || rhs.ty(sea) == Some(sea.types.ty_pointer_null)
            {
                return Some(*Not::new(lhs, sea));
            }
        }

        // Do we have ((x * (phi cons)) * con) ?
        // Do we have ((x * (phi cons)) * (phi cons)) ?
        // Push constant up through the phi: x * (phi con0*con0 con1*con1...)
        sea.phi_con(*self, op == BoolOp::EQ)
    }
}

impl Phi {
    fn idealize_phi(self, sea: &mut Nodes) -> Option<Node> {
        let region = self.inputs(sea)[0];
        if !sea.instanceof_region(region) {
            return self.inputs(sea)[1]; // Input has collapse to e.g. starting control.
        }
        if Nodes::in_progress(&sea.ops, &sea.inputs, *self)
            || region.unwrap().inputs(sea).is_empty()
        {
            return None; // Input is in-progress
        }

        // If we have only a single unique input, become it.
        if let live @ Some(_) = self.single_unique_input(sea) {
            return live;
        }

        // Pull "down" a common data op.  One less op in the world.  One more
        // Phi, but Phis do not make code.
        //   Phi(op(A,B),op(Q,R),op(X,Y)) becomes
        //     op(Phi(A,Q,X), Phi(B,R,Y)).
        let op = self.inputs(sea)[1].expect("not same_inputs");
        if op.inputs(sea).len() == 3
            && op.inputs(sea)[0].is_none()
            && !op.is_cfg(sea)
            && self.same_op(sea)
        {
            let n_in = &self.inputs(sea);

            let mut lhss = vec![None; n_in.len()];
            let mut rhss = vec![None; n_in.len()];

            // Set Region
            lhss[0] = n_in[0];
            rhss[0] = n_in[0];

            for i in 1..n_in.len() {
                lhss[i] = n_in[i].unwrap().inputs(sea)[1];
                rhss[i] = n_in[i].unwrap().inputs(sea)[2];
            }

            let label = sea[self].label;
            let declared_ty = sea[self].ty;
            let phi_lhs = Phi::new(label, declared_ty, lhss, sea).peephole(sea);
            let phi_rhs = Phi::new(label, declared_ty, rhss, sea).peephole(sea);
            return Some(sea.create((sea[op].clone(), vec![None, Some(phi_lhs), Some(phi_rhs)])));
        }

        // If merging Phi(N, cast(N)) - we are losing the cast JOIN effects, so just remove.
        if self.inputs(sea).len() == 3 {
            if let Some(cast) = self.inputs(sea)[1].unwrap().to_cast(sea) {
                let in_1 = cast.inputs(sea)[1];
                in_1.unwrap().add_dep(*self, sea);
                if in_1 == self.inputs(sea)[2] {
                    return self.inputs(sea)[2];
                }
            }
            if let Some(cast) = self.inputs(sea)[2].unwrap().to_cast(sea) {
                let in_1 = cast.inputs(sea)[1];
                in_1.unwrap().add_dep(*self, sea);
                if in_1 == self.inputs(sea)[1] {
                    return self.inputs(sea)[1];
                }
            }
        }

        // If merging a null-checked null and the checked value, just use the value.
        // if( val ) ..; phi(Region,False=0/null,True=val);
        // then replace with plain val.
        if self.inputs(sea).len() == 3 {
            let mut nullx = 0;

            let t1 = self.inputs(sea)[1].unwrap().ty(sea).unwrap();
            if Some(t1) == sea.types.make_init(t1) {
                nullx = 1;
            }
            let t2 = self.inputs(sea)[2].unwrap().ty(sea).unwrap();
            if Some(t2) == sea.types.make_init(t2) {
                nullx = 2;
            }

            if nullx != 0 {
                let val = self.inputs(sea)[3 - nullx].unwrap();
                let region = self.inputs(sea)[0].unwrap();
                let idom = region.to_cfg(&sea.ops).unwrap().idom(sea).unwrap();
                if let Some(iff) = sea.to_if(idom.node()) {
                    let pred = iff.inputs(sea)[1].unwrap();
                    pred.add_dep(*self, sea);
                    if pred == val {
                        // Must walk the idom on the null side to make sure we hit False.
                        let mut idom = region.inputs(sea)[nullx].unwrap().to_cfg(&sea.ops).unwrap();
                        while idom.node().inputs(sea)[0] != Some(*iff) {
                            idom = idom.idom(sea).unwrap();
                        }
                        if sea.to_cproj(idom.node()).is_some_and(|p| sea[p].index == 1) {
                            return Some(val);
                        }
                    }
                }
            }
        }

        None
    }
}

impl Stop {
    fn idealize_stop(self, sea: &mut Nodes) -> Option<Node> {
        let mut result = None;
        let mut i = 0;
        while i < self.inputs(sea).len() {
            if self.inputs(sea)[i].unwrap().ty(sea) == Some(sea.types.ty_xctrl) {
                self.del_def(i, sea);
                result = Some(*self);
            } else {
                i += 1;
            }
        }
        result
    }
}

impl Return {
    fn idealize_return(self, sea: &mut Nodes) -> Option<Node> {
        self.inputs(sea)[0].filter(|ctrl| ctrl.ty(sea) == Some(sea.types.ty_xctrl))
    }
}

impl CProj {
    fn idealize_cproj(self, sea: &mut Nodes) -> Option<Node> {
        let index = sea[self].index;
        if let Some(iff) = self.inputs(sea)[0]?.to_if(sea) {
            if let Some(Type::Tuple { types: ts }) = iff.ty(sea).as_deref() {
                if ts[1 - index] == sea.types.ty_xctrl {
                    return iff.inputs(sea)[0]; // We become our input control
                }
            }

            // Flip a negating if-test, to remove the not
            if let Some(not) = iff.inputs(sea)[1]?.add_dep(*self, sea).to_not(sea) {
                return Some(*CProj::new(
                    If::new(
                        iff.inputs(sea)[0].unwrap(),
                        Some(not.inputs(sea)[1].unwrap()),
                        sea,
                    )
                    .peephole(sea),
                    1 - index,
                    if index == 0 { "False" } else { "True" },
                    sea,
                ));
            }
        }

        None
    }
}

impl<'t> Nodes<'t> {
    fn idealize_region(&mut self, node: Node) -> Option<Node> {
        if Self::in_progress(&self.ops, &self.inputs, node) {
            return None;
        }

        // Delete dead paths into a Region
        if let Some(path) = node.find_dead_input(self) {
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
                    Some(*self.xctrl)
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
        if matches!(self[node], Op::If(IfOp::Never)) {
            return None;
        }
        // Hunt up the immediate dominator tree.  If we find an identical if
        // test on either the true or false branch, that side wins.
        let pred = self.inputs[node][1]?;
        if !self.ty[pred]?.is_high_or_constant() {
            let mut prior = node;
            let mut dom = node.to_cfg(&self.ops).unwrap().idom(self);
            while let Some(cfg) = dom {
                let d = cfg.node();
                d.add_dep(node, self);
                if matches!(&self[d], Op::If(_)) {
                    let if_pred = self.inputs[d][1]?;
                    if_pred.add_dep(node, self);
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
                dom = cfg.idom(self);
            }
        }
        None
    }
}
impl Cast {
    fn idealize_cast(self, sea: &mut Nodes) -> Option<Node> {
        self.inputs(sea)[1].filter(|&n| n.ty(sea).is_some_and(|t| sea.types.isa(t, sea[self])))
    }
}

impl Load {
    fn idealize_load(self, sea: &mut Nodes) -> Option<Node> {
        let mem = self.mem(sea)?;
        let ptr = self.ptr(sea)?;

        // Simple Load-after-Store on same address.
        if let Some(mem) = mem.to_store(sea) {
            // Must check same object
            if ptr == mem.ptr(sea)? {
                debug_assert_eq!(sea[mem].name, sea[self].name); // Equiv class aliasing is perfect
                return mem.inputs(sea)[3]; // store value
            }
        }

        // Push a Load up through a Phi, as long as it collapses on at least
        // one arm.  If at a Loop, the backedge MUST collapse - else we risk
        // spinning the same transform around the loop indefinitely.
        //   BEFORE (2 Sts, 1 Ld):          AFTER (1 St, 0 Ld):
        //   if( pred ) ptr.x = e0;         val = pred ? e0
        //   else       ptr.x = e1;                    : e1;
        //   val = ptr.x;                   ptr.x = val;
        if let Some(mem) = mem.to_phi(sea) {
            if mem.inputs(sea)[0]?.ty(sea) == Some(sea.types.ty_ctrl) && mem.inputs(sea).len() == 3
            {
                // Profit on RHS/Loop backedge
                if self.profit(mem, 2, sea) ||
                    // Else must not be a loop to count profit on LHS.
                    (mem.inputs(sea)[0].unwrap().to_loop(sea).is_none() && self.profit(mem, 1, sea))
                {
                    let name = sea[self].name;
                    let alias = sea[self].alias;
                    let declared_ty = sea[self].declared_type;

                    let ld1 = Load::new(
                        name,
                        alias,
                        declared_ty,
                        [mem.inputs(sea)[1].unwrap(), ptr],
                        sea,
                    )
                    .peephole(sea);
                    let ld2 = Load::new(
                        name,
                        alias,
                        declared_ty,
                        [mem.inputs(sea)[2].unwrap(), ptr],
                        sea,
                    )
                    .peephole(sea);

                    return Some(*Phi::new(
                        name,
                        self.ty(sea).unwrap(),
                        vec![mem.inputs(sea)[0], Some(ld1), Some(ld2)],
                        sea,
                    ));
                }
            }
        }

        None
    }

    /// Profitable if we find a matching Store on this Phi arm.
    fn profit(self, phi_node: Phi, idx: usize, sea: &mut Nodes) -> bool {
        phi_node.inputs(sea)[idx].is_some_and(|px| {
            px.add_dep(*self, sea)
                .to_store(sea)
                .is_some_and(|px| self.ptr(sea) == px.ptr(sea))
        })
    }
}

impl Store {
    fn idealize_store(self, sea: &mut Nodes) -> Option<Node> {
        // Simple store-after-store on same address.  Should pick up the
        // required init-store being stomped by a first user store.
        let mem = self.mem(sea)?;
        let ptr = self.ptr(sea)?;

        if let Some(mem) = mem.to_store(sea) {
            // Must check same object
            if ptr == mem.ptr(sea)? {
                // No bother if weird dead pointers
                if let Some(Type::Pointer(_)) = ptr.ty(sea).as_deref() {
                    // Must have exactly one use of "this" or you get weird
                    // non-serializable memory effects in the worse case.
                    if mem.check_no_use_beyond(*self, sea) {
                        debug_assert_eq!(
                            sea[self].name, sea[mem].name,
                            "Equiv class aliasing is perfect"
                        );
                        self.set_def(1, mem.mem(sea), sea);
                        return Some(*self);
                    }
                }
            }
        }
        None
    }

    // Check that `this` has no uses beyond `that`
    fn check_no_use_beyond(self, that: Node, sea: &mut Nodes) -> bool {
        let n_outs = sea.outputs[self].len();
        if n_outs == 1 {
            return true;
        }
        // Add deps on the other uses (can be e.g. ScopeNode mid-parse) so that
        // when the other uses go away we can retry.
        for i in 0..n_outs {
            let use_ = sea.outputs[self][i];
            if use_ != that {
                use_.add_dep(that, sea);
            }
        }
        false
    }
}

impl Minus {
    fn idealize_minus(self, sea: &mut Nodes) -> Option<Node> {
        // -(-x) is x
        if let Some(m) = self.inputs(sea)[1]?.to_minus(sea) {
            return m.inputs(sea)[1];
        }
        None
    }
}

impl Div {
    fn idealize_div(self, sea: &mut Nodes) -> Option<Node> {
        // Div of 1.
        if let Some(Type::Int(Int::Constant(1))) =
            self.inputs(sea)[2].and_then(|n| n.ty(sea)).as_deref()
        {
            return self.inputs(sea)[1];
        }
        None
    }
}

impl Node {
    fn find_dead_input(self, sea: &Nodes) -> Option<usize> {
        (1..self.inputs(sea).len())
            .find(|&i| self.inputs(sea)[i].unwrap().ty(sea).unwrap() == sea.types.ty_xctrl)
    }
}

impl<'t> Nodes<'t> {
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

        if matches!(self[lo], Op::Phi(_)) && lo.all_cons(dep, self) {
            return false;
        }
        if matches!(self[hi], Op::Phi(_)) && hi.all_cons(dep, self) {
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

    // Rotation is only valid for associative ops, e.g. Add, Mul, And, Or.
    // Do we have ((phi cons)|(x + (phi cons)) + con|(phi cons)) ?
    // Push constant up through the phi: x + (phi con0+con0 con1+con1...)
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
            ns[i] = Some(
                self.create((
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
                ))
                .peephole(self),
            );
        }

        let label = format!(
            "{}{}",
            self[lphi].label,
            self.to_phi(rhs).map(|p| self[p].label).unwrap_or("")
        );
        let label = self.types.get_str(&label);
        let ty = self[lphi].ty;
        let phi = Phi::new(label, ty, ns, self).peephole(self);

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
    fn pcon(&mut self, op: Option<Node>, dep: Node) -> Option<Phi> {
        op?.to_phi(self).filter(|phi| phi.all_cons(dep, self))
    }
}
