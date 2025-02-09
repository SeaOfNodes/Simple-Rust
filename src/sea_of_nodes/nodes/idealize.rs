use crate::datastructures::id::Id;
use crate::sea_of_nodes::nodes::node::{
    Add, AddF, Bool, CProj, Cast, Constant, Div, DivF, If, Load, Minus, MinusF, Mul, MulF, Not,
    Phi, ReadOnly, Return, RoundF32, Sar, Shl, Shr, Stop, Store, Sub, SubF, TypedNode,
};
use crate::sea_of_nodes::nodes::node::{IfOp, Region};
use crate::sea_of_nodes::nodes::{BoolOp, Node, Nodes};
use crate::sea_of_nodes::types::{Ty, TyFloat, TyInt};

impl Node {
    /// do not peephole directly returned values!
    pub(super) fn idealize(self, sea: &mut Nodes) -> Option<Node> {
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
            TypedNode::Region(n) => n.idealize_region(sea),
            TypedNode::Loop(n) => n.idealize_region(sea), // super
            TypedNode::If(n) => n.idealize_if(sea),
            TypedNode::Cast(n) => n.idealize_cast(sea),
            TypedNode::Load(n) => n.idealize_load(sea),
            TypedNode::Store(n) => n.idealize_store(sea),
            TypedNode::Minus(n) => n.idealize_minus(sea),
            TypedNode::MinusF(n) => n.idealize_minusf(sea),
            TypedNode::Div(n) => n.idealize_div(sea),
            TypedNode::ReadOnly(n) => n.idealize_read_only(sea),
            TypedNode::RoundF32(n) => n.idealize_round_f32(sea),
            TypedNode::Shl(n) => n.idealize_shl(sea),
            TypedNode::Shr(n) => n.idealize_shr(sea),
            TypedNode::Sar(n) => n.idealize_sar(sea),
            TypedNode::AddF(n) => n.idealize_addf(sea),
            TypedNode::DivF(n) => n.idealize_divf(sea),
            TypedNode::MulF(n) => n.idealize_mulf(sea),
            TypedNode::SubF(n) => n.idealize_subf(sea),
            TypedNode::Constant(_)
            | TypedNode::XCtrl(_)
            | TypedNode::Start(_)
            | TypedNode::Scope(_)
            | TypedNode::ScopeMin(_)
            | TypedNode::Struct(_)
            | TypedNode::New(_)
            | TypedNode::ToFloat(_)
            | TypedNode::Not(_) => None,
            TypedNode::Cfg(_) => unreachable!(),
            n => todo!("{n:?}"),
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
        if t2 == *sea.types.int_zero {
            return Some(lhs);
        }

        // Add of same to a multiply by 2
        if lhs == rhs {
            let two = Constant::new(*sea.types.int_two, sea).peephole(sea);
            return Some(*Mul::new(lhs, two, sea));
        }

        // Goal: a left-spine set of adds, with constants on the rhs (which then fold).

        // Move non-adds to RHS
        if !lhs.is_add(sea) && rhs.is_add(sea) {
            return Some(self.swap_12(sea));
        }

        // x+(-y) becomes x-y
        if rhs.is_minus(sea) {
            let y = rhs.inputs(sea)[1].unwrap();
            return Some(*Sub::new(lhs, y, sea));
        }

        // Now we might see (add add non) or (add non non) or (add add add) but never (add non add)

        // Do we have  x + (y + z) ?
        // Swap to    (x + y) + z
        // Rotate (add add add) to remove the add on RHS
        if rhs.is_add(sea) {
            let x = lhs;
            let y = rhs.inputs(sea)[1]?;
            let z = rhs.inputs(sea)[2]?;
            return Some(*Add::new(Add::new(x, y, sea).peephole(sea), z, sea));
        }

        // Now we might see (add add non) or (add non non) but never (add non add) nor (add add add)
        if !lhs.is_add(sea) {
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
        if lhs.inputs(sea)[2]
            .unwrap()
            .add_dep(self, sea)
            .ty(sea)
            .unwrap()
            .is_constant(sea.tys)
            && t2.is_constant(sea.tys)
        {
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

        if right_ty == *sea.types.int_one {
            Some(left)
        } else if left_ty.is_constant(sea.tys) && !right_ty.is_constant(sea.tys) {
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
        if self.inputs(sea)[1] == self.inputs(sea)[2] {
            let value = sea.types.get_bool(!matches!(op, BoolOp::LT | BoolOp::LTF));
            return Some(*Constant::new(*value, sea));
        }

        // Equals pushes constant to the right; 5==X becomes X==5.
        if op == BoolOp::EQ || op == BoolOp::EQF {
            let lhs = self.inputs(sea)[1].unwrap();
            let rhs = self.inputs(sea)[2].unwrap();

            if !rhs.is_constant(sea) {
                // con==noncon becomes noncon==con
                if lhs.is_constant(sea) || lhs.index() > rhs.index() {
                    // Equals sorts by NID otherwise: non.high == non.low becomes non.low == non.high
                    return Some(*Bool::new(rhs, lhs, op, sea));
                }
            }
            // Equals X==0 becomes a !X
            if rhs.ty(sea) == Some(*sea.types.int_zero) || rhs.ty(sea) == Some(*sea.types.ptr_null)
            {
                return Some(*Not::new(lhs, sea));
            }
        }

        // Do we have ((x * (phi cons)) * con) ?
        // Do we have ((x * (phi cons)) * (phi cons)) ?
        // Push constant up through the phi: x * (phi con0*con0 con1*con1...)
        sea.phi_con(*self, op == BoolOp::EQ || op == BoolOp::EQF)
    }
}

impl Phi {
    fn idealize_phi(self, sea: &mut Nodes) -> Option<Node> {
        let Some(r) = self.region(sea).to_region(sea) else {
            return self.inputs(sea)[1]; // Input has collapse to e.g. starting control.
        };
        if Nodes::in_progress(&sea.ops, &sea.inputs, **r) || r.inputs(sea).len() <= 1 {
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
        if op.inputs(sea).len() == 3 && op.inputs(sea)[0].is_none() && self.same_op(sea) {
            debug_assert!(!op.is_cfg(sea));
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
                let in_1 = cast.inputs(sea)[1].unwrap().add_dep(self, sea);
                if Some(in_1) == self.inputs(sea)[2] {
                    return self.inputs(sea)[2];
                }
            }
            if let Some(cast) = self.inputs(sea)[2].unwrap().to_cast(sea) {
                let in_1 = cast.inputs(sea)[1].unwrap().add_dep(self, sea);
                if Some(in_1) == self.inputs(sea)[1] {
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
                if let Some(iff) = r.idom(sea).unwrap().add_dep(self, sea).to_if(sea) {
                    if iff.pred(sea).unwrap().add_dep(self, sea) == val {
                        // Must walk the idom on the null side to make sure we hit False.
                        let mut idom = r.cfg(nullx, sea).unwrap();
                        while !idom.inputs(sea).is_empty() && idom.inputs(sea)[0] != Some(**iff) {
                            idom = idom.idom(sea).unwrap();
                        }
                        if idom.to_cproj(sea).is_some_and(|p| sea[p].index == 1) {
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
            if self.inputs(sea)[i].unwrap().ty(sea) == Some(sea.types.xctrl) {
                self.del_def(i, sea);
                result = Some(**self);
            } else {
                i += 1;
            }
        }
        result
    }
}

impl Return {
    fn idealize_return(self, sea: &mut Nodes) -> Option<Node> {
        self.inputs(sea)[0].filter(|ctrl| ctrl.ty(sea) == Some(sea.types.xctrl))
    }
}

impl CProj {
    fn idealize_cproj(self, sea: &mut Nodes) -> Option<Node> {
        let ctrl = self.cfg(0, sea).unwrap();
        let idx = sea[self].index;

        if let Some(tt) = ctrl.ty(sea).and_then(Ty::to_tuple) {
            if tt.data()[idx] == sea.types.xctrl {
                return Some(**sea.xctrl); // We are dead
            }
            // Only true for IfNodes
            if ctrl.is_if(sea) && tt.data()[1 - idx] == sea.types.xctrl {
                return ctrl.inputs(sea)[0]; // We become our input control
            }
        }
        // Flip a negating if-test, to remove the not
        if let Some(iff) = ctrl.to_if(sea) {
            if let Some(not) = iff.pred(sea).unwrap().add_dep(self, sea).to_not(sea) {
                return Some(**CProj::new(
                    If::new(
                        iff.inputs(sea)[0].unwrap(),
                        Some(not.inputs(sea)[1].unwrap()),
                        sea,
                    )
                    .peephole(sea),
                    1 - idx,
                    if idx == 0 { "False" } else { "True" },
                    sea,
                ));
            }
        }
        None
    }
}

impl Region {
    fn idealize_region(self, sea: &mut Nodes) -> Option<Node> {
        if Nodes::in_progress(&sea.ops, &sea.inputs, **self) {
            return None;
        }

        // Delete dead paths into a Region
        if let Some(path) = self.find_dead_input(sea) {
            // Do not delete the entry path of a loop (ok to remove the back
            // edge and make the loop a single-entry Region which folds away
            // the Loop).  Folding the entry path confused the loop structure,
            // moving the backedge to the entry point.
            if !(self.is_loop(sea) && self.inputs(sea)[1] == self.inputs(sea)[path]) {
                // Cannot use the obvious output iterator here, because a Phi
                // deleting an input might recursively delete *itself*.  This
                // shuffles the output array, and we might miss iterating an
                // unrelated Phi. So on rare occasions we repeat the loop to get
                // all the Phis.
                let mut nouts = 0;
                while nouts != sea.outputs[self].len() {
                    nouts = sea.outputs[self].len();

                    for i in 0.. {
                        if i >= sea.outputs[self].len() {
                            break;
                        }
                        let phi = sea.outputs[self][i];
                        if phi.is_phi(sea) && self.inputs(sea).len() == phi.inputs(sea).len() {
                            phi.del_def(path, sea);
                            for &o in &sea.outputs[phi] {
                                sea.iter_peeps.add(o);
                            }
                        }
                    }
                }

                return if self.is_dead(sea) {
                    Some(**sea.xctrl)
                } else {
                    self.del_def(path, sea);
                    Some(**self)
                };
            }
        }

        // If down to a single input, become that input - but also make all
        if self.inputs(sea).len() == 2 && !self.has_phi(sea) {
            return self.inputs(sea)[1]; // Collapse if no Phis; 1-input Phis will collapse on their own
        }

        // If a CFG diamond with no merging, delete: "if( pred ) {} else {};"
        if !self.has_phi(sea) {
            // No Phi users, just a control user
            if let Some(p1) = self.inputs(sea)[1].and_then(|n| n.to_cproj(sea)) {
                if let Some(p2) = self.inputs(sea)[2].and_then(|n| n.to_cproj(sea)) {
                    if p1.inputs(sea)[0].unwrap().add_dep(self, sea)
                        == p2.inputs(sea)[0].unwrap().add_dep(self, sea)
                    {
                        if let Some(iff) = p1.inputs(sea)[0].unwrap().to_if(sea) {
                            return iff.inputs(sea)[0];
                        }
                    }
                }
            }
        }

        None
    }

    fn has_phi(self, sea: &Nodes) -> bool {
        sea.outputs[self].iter().any(|phi| phi.is_phi(sea))
    }
}

impl If {
    fn idealize_if(self, sea: &mut Nodes) -> Option<Node> {
        if matches!(sea[self], IfOp::Never) {
            return None;
        }
        // Hunt up the immediate dominator tree.  If we find an identical if
        // test on either the true or false branch, that side wins.
        let pred = self.pred(sea).unwrap();
        if !pred.ty(sea).unwrap().is_high_or_constant(sea.tys) {
            let mut prior = *self;
            let mut dom = self.idom(sea);
            while let Some(d) = dom {
                if let Some(d) = d.add_dep(self, sea).to_if(sea) {
                    if d.pred(sea).unwrap().add_dep(self, sea) == pred {
                        if let Some(prior) = prior.to_cproj(sea) {
                            let c = Constant::new(*sea.types.get_bool(sea[prior].index == 0), sea);
                            self.set_def(1, c.peephole(sea), sea);
                            return Some(**self);
                        }
                    }
                }
                prior = d;
                dom = d.idom(sea);
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
        let mut ptr = self.ptr(sea);

        // Simple Load-after-Store on same address.
        if let Some(st) = self.mem(sea).and_then(|m| m.to_store(sea)) {
            // Must check same object
            if ptr == st.ptr(sea) && self.off(sea) == st.off(sea) {
                debug_assert_eq!(
                    sea[self].name, sea[st].name,
                    "Equiv class aliasing is perfect"
                );
                return st.val(sea);
            }
        }

        // Simple Load-after-New on same address.
        if let Some(p) = self.mem(sea).and_then(|m| m.to_proj(sea)) {
            if let Some(nnn) = p.inputs(sea)[0].and_then(|i| i.to_new(sea)) {
                // Must check same object
                if ptr == nnn.proj(1, sea).map(|p| p.to_node()) {
                    return nnn.inputs(sea)[nnn.find_alias(sea[self].alias, sea)];
                    // Load from New init
                }
            }
        }

        // Load-after-Store on same address, but bypassing provably unrelated
        // stores.  This is a more complex superset of the above two peeps.
        // "Provably unrelated" is really weak.
        if let Some(ro) = ptr.and_then(|p| p.to_ronly(sea)) {
            ptr = ro.inputs(sea)[1];
        }
        let mut mem = self.mem(sea);
        loop {
            match mem.unwrap().downcast(&sea.ops) {
                TypedNode::Store(st) => {
                    if ptr == st.ptr(sea) && self.off(sea) == st.off(sea) {
                        return Some(self.cast_ro(st.val(sea).unwrap(), sea)); // Proved equal
                    }
                    // Can we prove unequal?  Offsets do not overlap?
                    let off_ty = self.off(sea).unwrap().ty(sea).unwrap();
                    if !off_ty
                        .join(st.off(sea).unwrap().ty(sea).unwrap(), sea.tys)
                        .is_high(sea.tys)
                        && !Self::never_alias(ptr.unwrap(), st.ptr(sea).unwrap(), sea)
                    {
                        break; // Cannot tell, stop trying
                    }
                    // Pointers cannot overlap
                    mem = st.mem(sea); // Proved never equal
                }
                TypedNode::Phi(_) => break,      // Assume related
                TypedNode::Constant(_) => break, // Assume shortly dead
                TypedNode::Proj(mproj) => {
                    if let Some(nnn1) = mproj.inputs(sea)[0].and_then(|i| i.to_new(sea)) {
                        if let Some(pproj) = ptr.and_then(|p| p.to_proj(sea)) {
                            if pproj.inputs(sea)[0] == mproj.inputs(sea)[0] {
                                // return castRO(nnn1.in(nnn1.findAlias(_alias))); // Load from New init
                            }
                        }
                        if !ptr.and_then(|p| p.to_proj(sea)).is_some_and(|pproj| {
                            pproj.inputs(sea)[0].is_some_and(|i| i.is_new(sea))
                        }) {
                            break; // Cannot tell, ptr not related to New
                        }
                        mem = nnn1.inputs(sea)[sea[self].alias as usize]; // Bypass unrelated New
                    } else if mproj.inputs(sea)[0].is_some_and(|i| i.is_start(sea)) {
                        break;
                    } else {
                        todo!()
                    }
                }
                _ => todo!(),
            }
        }

        // Push a Load up through a Phi, as long as it collapses on at least
        // one arm.  If at a Loop, the backedge MUST collapse - else we risk
        // spinning the same transform around the loop indefinitely.
        //   BEFORE (2 Sts, 1 Ld):          AFTER (1 St, 0 Ld):
        //   if( pred ) ptr.x = e0;         val = pred ? e0
        //   else       ptr.x = e1;                    : e1;
        //   val = ptr.x;                   ptr.x = val;
        if let Some(memphi) = self.mem(sea).and_then(|m| m.to_phi(sea)) {
            if memphi.region(sea).ty(sea) == Some(sea.tys.ctrl) && memphi.inputs(sea).len() == 3 &&
                // Offset can be hoisted
                self.off(sea).is_some_and(|o| o.is_constant(sea)) &&
                // Pointer can be hoisted
                Self::hoist_ptr(ptr.unwrap(), memphi, sea)
            {
                // Profit on RHS/Loop backedge
                if self.profit(memphi, 2, sea) ||
                    // Else must not be a loop to count profit on LHS.
                    (!memphi.region(sea).is_loop(sea) && self.profit(memphi, 1, sea))
                {
                    let ld1 = self.ld(1, sea);
                    let ld2 = self.ld(2, sea);
                    let op = &sea[self];
                    return Some(*Phi::new(
                        op.name,
                        op.declared_type,
                        vec![Some(*memphi.region(sea)), Some(ld1), Some(ld2)],
                        sea,
                    ));
                }
            }
        }
        None
    }

    fn ld(self, idx: usize, sea: &mut Nodes) -> Node {
        let mem = self.mem(sea).unwrap();
        let ptr = self.ptr(sea).unwrap();
        let off = self.off(sea).unwrap();
        let op = &sea[self];
        Load::new(
            op.name,
            op.alias,
            op.declared_type,
            [
                mem.inputs(sea)[idx].unwrap(),
                if ptr.inputs(sea)[0] == mem.inputs(sea)[0] {
                    ptr.inputs(sea)[idx].unwrap()
                } else {
                    ptr
                },
                off,
            ],
            sea,
        )
        .peephole(sea)
    }

    fn never_alias(ptr1: Node, ptr2: Node, sea: &mut Nodes) -> bool {
        let p1i0 = ptr1.inputs(sea)[0].unwrap();
        let p2i0 = ptr2.inputs(sea)[0].unwrap();
        p1i0 != p2i0 &&
            // Unrelated allocations
            ptr1.is_proj(sea) && p1i0.is_new(sea) &&
            ptr2.is_proj(sea) && p2i0.is_new(sea)
    }

    fn hoist_ptr(ptr: Node, memphi: Phi, sea: &mut Nodes) -> bool {
        // Can I hoist ptr above the Region?
        let Some(r) = memphi.region(sea).to_region(sea) else {
            return false; // Dead or dying Region/Phi
        };

        // If ptr from same Region, then yes, just use hoisted split pointers
        if ptr.to_phi(sea).is_some_and(|p| p.region(sea) == *r) {
            return true;
        }

        // No, so can we lift this ptr?
        if let Some(cptr) = ptr.inputs(sea)[0].and_then(|n| n.to_cfg(sea)) {
            // Pointer is controlled high
            // TODO: Really needs to be the LCA of all inputs is high
            return cptr.idepth(sea) <= r.idepth(sea);
        }
        // Dunno without a longer walk
        false
    }

    /// Profitable if we find a matching Store on this Phi arm.
    fn profit(self, phi: Phi, idx: usize, sea: &mut Nodes) -> bool {
        let Some(px) = phi.inputs(sea)[idx] else {
            return false;
        };
        if px
            .ty(sea)
            .and_then(Ty::to_mem)
            .is_some_and(|m| m.data().t.is_high_or_constant(sea.tys))
        {
            px.add_dep(self, sea);
            return true;
        }
        if px
            .to_store(sea)
            .is_some_and(|s| self.ptr(sea) == s.ptr(sea) && self.off(sea) == s.off(sea))
        {
            px.add_dep(self, sea);
            return true;
        }
        false
    }

    /// Read-Only is a deep property, and cannot be cast-away
    fn cast_ro(self, rez: Node, sea: &mut Nodes) -> Node {
        if self.ptr(sea).unwrap().ty(sea).unwrap().is_final() && !rez.ty(sea).unwrap().is_final() {
            ReadOnly::new(rez, sea).peephole(sea)
        } else {
            rez
        }
    }
}

impl Store {
    fn idealize_store(self, sea: &mut Nodes) -> Option<Node> {
        // Simple store-after-store on same address.  Should pick up the
        // required init-store being stomped by a first user store.
        if let Some(st) = self.mem(sea).and_then(|m| m.to_store(sea)) {
            // Must check same object
            // And same offset (could be "same alias" but this handles arrays to same index)
            if self.ptr(sea) == st.ptr(sea) && self.off(sea) == st.off(sea) {
                // No bother if weird dead pointers
                if self.ptr(sea).unwrap().ty(sea).is_some_and(Ty::is_mem_ptr) {
                    // Must have exactly one use of "this" or you get weird
                    // non-serializable memory effects in the worse case.
                    if self.check_only_use(*st, sea) {
                        debug_assert_eq!(
                            sea[self].name, sea[st].name,
                            "Equiv class aliasing is perfect"
                        );
                        self.set_def(1, st.mem(sea), sea);
                        return Some(*self);
                    }
                }
            }
        }

        // Simple store-after-new on same address.  Should pick up the
        // an init-store being stomped by a first user store.
        if let Some(st) = self.mem(sea).and_then(|m| m.to_proj(sea)) {
            if let Some(nnn) = st.inputs(sea)[0].and_then(|n| n.to_new(sea)) {
                if let Some(ptr) = self.ptr(sea).and_then(|m| m.to_proj(sea)) {
                    if ptr.inputs(sea)[0] == Some(*nnn) {
                        // No bother if weird dead pointers
                        if let Some(tmp) = ptr.ty(sea).and_then(Ty::to_mem_ptr) {
                            // Cannot fold a store of a single element over the array body initializer value
                            if !(tmp.data().to.is_ary()
                                && tmp.data().to.fields()[1].alias == sea[self].alias)
                            {
                                // Very sad strong cutout: val has to be legal to hoist to a New
                                // input, which means it cannot depend on the New.  Many many
                                // things are legal here but difficult to check without doing a
                                // full dominator check.  Example failure:
                                // "struct C { C? c; }; C self = new C { c=self; }"
                                if self
                                    .val(sea)
                                    .unwrap()
                                    .ty(sea)
                                    .unwrap()
                                    .is_high_or_constant(sea.tys)
                                {
                                    // Must have exactly one use of "this" or you get weird
                                    // non-serializable memory effects in the worse case.
                                    if self.check_only_use(*st, sea) {
                                        // Folding away a broken store
                                        if self.err(sea).is_none() {
                                            nnn.set_def(
                                                nnn.find_alias(sea[self].alias, sea),
                                                self.val(sea),
                                                sea,
                                            );
                                            // Must retype the NewNode
                                            sea.ty[nnn] = Some(nnn.compute(sea));
                                            let mem = self.mem(sea).unwrap();
                                            sea.ty[mem] = Some(mem.compute(sea));
                                            return Some(*st);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        None
    }

    // Check that `mem` has no uses except `self`
    fn check_only_use(self, mem: Node, sea: &mut Nodes) -> bool {
        let n_outs = sea.outputs[mem].len();
        if n_outs == 1 {
            return true;
        }
        // Add deps on the other uses (can be e.g. ScopeNode mid-parse) so that
        // when the other uses go away we can retry.
        for i in 0..n_outs {
            let use_ = sea.outputs[mem][i];
            if use_ != *self {
                use_.add_dep(self, sea);
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
impl MinusF {
    fn idealize_minusf(self, sea: &mut Nodes) -> Option<Node> {
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
        if Some(*sea.tys.int_one) == self.inputs(sea)[2].and_then(|n| n.ty(sea)) {
            return self.inputs(sea)[1];
        }
        None
    }
}

impl ReadOnly {
    fn idealize_read_only(self, sea: &mut Nodes) -> Option<Node> {
        let in1 = self.inputs(sea)[1].unwrap();
        if let Some(tmp) = in1.ty(sea).and_then(Ty::to_mem_ptr) {
            if tmp.is_final() {
                return Some(in1);
            }
        }
        None
    }
}

impl RoundF32 {
    fn idealize_round_f32(self, sea: &mut Nodes) -> Option<Node> {
        let lhs = self.inputs(sea)[1].unwrap();

        // RoundF32 of float
        if let Some(t) = lhs.ty(sea).and_then(Ty::to_float) {
            if t.sz() == 32 {
                return Some(lhs);
            }
        }
        None
    }
}

impl Shl {
    fn idealize_shl(self, sea: &mut Nodes) -> Option<Node> {
        let lhs = self.inputs(sea)[1].unwrap();
        let rhs = self.inputs(sea)[1].unwrap();

        if let Some(shl) = rhs.ty(sea).and_then(Ty::to_int) {
            if let Some(i) = shl.value() {
                // Shl of 0.
                if i & 63 == 0 {
                    return Some(lhs);
                }
                // (x + c) << i  =>  (x << i) + (c << i)
                if let Some(add) = lhs.to_add(sea) {
                    if let Some(c) = add.add_dep(self, sea).inputs(sea)[2]
                        .unwrap()
                        .ty(sea)
                        .and_then(Ty::to_int)
                    {
                        if let Some(c) = c.value() {
                            if c != 0 {
                                let sum = c << i;
                                if i32::try_from(sum).is_ok() {
                                    return Some(*Add::new(
                                        Shl::new(add.inputs(sea)[1].unwrap(), Some(rhs), sea)
                                            .peephole(sea),
                                        Constant::new(*sea.types.get_int(sum), sea).peephole(sea),
                                        sea,
                                    ));
                                }
                            }
                        }
                    }
                }
            }
        }
        // TODO: x << 3 << (y ? 1 : 2) ==> x << (y ? 4 : 5)
        None
    }
}

impl Shr {
    fn idealize_shr(self, sea: &mut Nodes) -> Option<Node> {
        let lhs = self.inputs(sea)[1].unwrap();
        let rhs = self.inputs(sea)[1].unwrap();

        // Shr of 0.
        if let Some(shr) = rhs.ty(sea).and_then(Ty::to_int).and_then(TyInt::value) {
            if shr & 63 == 0 {
                return Some(lhs);
            }
        }
        // TODO: x >>> 3 >>> (y ? 1 : 2) ==> x >>> (y ? 4 : 5)
        None
    }
}
impl Sar {
    fn idealize_sar(self, sea: &mut Nodes) -> Option<Node> {
        let lhs = self.inputs(sea)[1].unwrap();
        let rhs = self.inputs(sea)[1].unwrap();

        // Sar of 0.
        if let Some(shr) = rhs.ty(sea).and_then(Ty::to_int).and_then(TyInt::value) {
            if shr & 63 == 0 {
                return Some(lhs);
            }
        }
        // TODO: x >> 3 >> (y ? 1 : 2) ==> x >> (y ? 4 : 5)
        None
    }
}

impl AddF {
    fn idealize_addf(self, sea: &mut Nodes) -> Option<Node> {
        // Add of 0.
        if self.inputs(sea)[2].unwrap().ty(sea) == Some(*sea.tys.float_zero) {
            return self.inputs(sea)[1];
        }
        None
    }
}
impl DivF {
    fn idealize_divf(self, sea: &mut Nodes) -> Option<Node> {
        let t2 = self.inputs(sea)[2].unwrap().ty(sea);

        // Add of 0.
        if t2.and_then(Ty::to_float).and_then(TyFloat::value) == Some(1.0) {
            return self.inputs(sea)[1];
        }
        None
    }
}

impl MulF {
    fn idealize_mulf(self, sea: &mut Nodes) -> Option<Node> {
        let lhs = self.inputs(sea)[1].unwrap();
        let rhs = self.inputs(sea)[2].unwrap();
        let t1 = lhs.ty(sea);
        let t2 = rhs.ty(sea);

        // Mul of 1.  We do not check for (1*x) because this will already
        // canonicalize to (x*1)
        if t2.and_then(Ty::to_float).and_then(TyFloat::value) == Some(1.0) {
            return Some(lhs);
        }

        // Move constants to RHS: con*arg becomes arg*con
        if t1.is_some_and(|t| t.is_constant(sea.tys)) && !t2.is_some_and(|t| t.is_constant(sea.tys))
        {
            return Some(self.swap_12(sea));
        }
        None
    }
}

impl SubF {
    fn idealize_subf(self, sea: &mut Nodes) -> Option<Node> {
        // Sub of 0.
        if self.inputs(sea)[2].unwrap().ty(sea) == Some(*sea.tys.float_zero) {
            return self.inputs(sea)[1];
        }
        None
    }
}

impl Node {
    fn find_dead_input(self, sea: &Nodes) -> Option<usize> {
        (1..self.inputs(sea).len())
            .find(|&i| self.inputs(sea)[i].unwrap().ty(sea).unwrap() == sea.types.xctrl)
    }
}

impl<'t> Nodes<'t> {
    // Compare two off-spine nodes and decide what order they should be in.
    // Do we rotate ((x + hi) + lo) into ((x + lo) + hi) ?
    // Generally constants always go right, then Phi-of-constants, then muls, then others.
    // Ties with in a category sort by node ID.
    // TRUE if swapping hi and lo.
    fn spine_cmp(&mut self, hi: Node, lo: Node, dep: Node) -> bool {
        if lo.ty(self).is_some_and(|t| t.is_constant(self.tys)) {
            return false;
        }
        if hi.ty(self).is_some_and(|t| t.is_constant(self.tys)) {
            return true;
        }

        if lo.is_phi(self) && lo.inputs(self)[0].unwrap().ty(self) == Some(self.types.xctrl) {
            return false;
        }
        if hi.is_phi(self) && hi.inputs(self)[0].unwrap().ty(self) == Some(self.types.xctrl) {
            return false;
        }

        if lo.is_phi(self) && lo.all_cons(dep, self) {
            return false;
        }
        if hi.is_phi(self) && hi.all_cons(dep, self) {
            return true;
        }

        if lo.is_phi(self) && !hi.is_phi(self) {
            return true;
        }
        if hi.is_phi(self) && !lo.is_phi(self) {
            return false;
        }

        lo.index() > hi.index()
    }

    // Rotation is only valid for associative ops, e.g. Add, Mul, And, Or, Xor.
    // Do we have ((phi cons)|(x + (phi cons)) + con|(phi cons)) ?
    // Push constant up through the phi: x + (phi con0+con0 con1+con1...)
    fn phi_con(&mut self, op: Node, rotate: bool) -> Option<Node> {
        let lhs = op.inputs(self)[1]?;
        let rhs = op.inputs(self)[2]?;
        if rhs.ty(self) == Some(*self.types.int_top) {
            return None;
        }

        // LHS is either a Phi of constants, or another op with Phi of constants
        let mut lphi = self.pcon(Some(lhs), op);
        if rotate && lphi.is_none() && lhs.inputs(self).len() > 2 {
            // Only valid to rotate constants if both are same associative ops
            if self[lhs].operation() != self[op].operation() {
                return None;
            }
            lphi = self.pcon(lhs.inputs(self)[2], op); // Will rotate with the Phi push
        }

        let lphi = lphi?;

        // RHS is a constant or a Phi of constants
        if !rhs.is_constant(self) && self.pcon(Some(rhs), op).is_none() {
            return None;
        }

        // If both are Phis, must be same Region
        if rhs.is_phi(self) && lphi.inputs(self)[0] != rhs.inputs(self)[0] {
            return None;
        }

        // Note that this is the exact reverse of Phi pulling a common op down
        // to reduce total op-count.  We don't get in an endless push-up
        // push-down peephole cycle because the constants all fold first.

        let mut ns = vec![None; lphi.inputs(self).len()];
        ns[0] = lphi.inputs(self)[0];

        // Push constant up through the phi: x + (phi con0+con0 con1+con1...)
        for i in 1..ns.len() {
            ns[i] = Some(
                self.create((
                    self[op].clone(),
                    vec![
                        None,
                        lphi.inputs(self)[i],
                        if rhs.is_phi(self) {
                            rhs.inputs(self)[i]
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
                vec![None, lhs.inputs(self)[1], Some(phi)],
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
