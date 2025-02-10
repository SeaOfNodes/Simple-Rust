use crate::sea_of_nodes::nodes::node::{Constant, Shl, Shr, XCtrl};
use crate::sea_of_nodes::nodes::node::{IfOp, Sar};
use crate::sea_of_nodes::nodes::{BoolOp, Node, Nodes, Op};
use crate::sea_of_nodes::types::{Field, Ty, Type};

impl<'t> Node {
    /// Try to peephole at this node and return a better replacement Node.
    /// Always returns some not-null Node (often this).
    #[must_use]
    pub fn peephole(self, sea: &mut Nodes<'t>) -> Node {
        if sea.disable_peephole {
            sea.ty[self] = Some(self.compute(sea));
            self // Peephole optimizations turned off
        } else if let Some(n) = self.peephole_opt(sea) {
            let n = n.peephole(sea);
            self.dead_code_elimination(n, sea);
            n
        } else {
            self // Cannot return null for no-progress
        }
    }

    /// Try to peephole at this node and return a better replacement Node if
    /// possible.  We compute a {@link Type} and then check and replace:
    /// <ul>
    /// <li>if the Type {@link Type#isConstant}, we replace with a {@link ConstantNode}</li>
    /// <li>in a future chapter we will look for a
    /// <a href="https://en.wikipedia.org/wiki/Common_subexpression_elimination">Common Subexpression</a>
    /// to eliminate.</li>
    /// <li>we ask the Node for a better replacement.  The "better replacement"
    /// is things like {@code (1+2)} becomes {@code 3} and {@code (1+(x+2))} becomes
    /// {@code (x+(1+2))}.  By canonicalizing expressions we fold common addressing
    /// math constants, remove algebraic identities and generally simplify the
    /// code. </li>
    ///
    /// Unlike peephole above, this explicitly returns null for no-change, or not-null
    /// for a better replacement (which can be this).
    /// </ul>
    #[must_use]
    pub fn peephole_opt(self, sea: &mut Nodes) -> Option<Node> {
        sea.iter_cnt += 1;

        // Compute initial or improved Type
        let ty = self.compute(sea);
        let old = self.set_type(ty, sea);

        // System.out.printf("%-3d %-8s %s->%s    %s%n", _nid, label(), old, _type, print());
        // println!("{:<3} {:<8} {}->{}    {}", self, &sea[self].label(), old.map(|t| t.to_string()).unwrap_or("null".to_string()), ty, self.print(sea));

        // Replace constant computations from non-constants with a constant node
        if !self.is_constant(sea) && !self.is_xctrl(sea) && ty.is_high_or_constant(sea.tys) {
            return if ty == sea.types.xctrl {
                **XCtrl::new(sea)
            } else {
                *Constant::new(ty, sea)
            }
            .peephole_opt(sea);
        }

        // Global Value Numbering
        if let Some(replacement) = self.global_value_numbering(sea) {
            // Because of random worklist ordering, the two equal nodes
            // might have different types.  Because of monotonicity, both
            // types are valid.  To preserve monotonicity, the resulting
            // shared Node has to have the best of both types.
            replacement.set_type(sea.types.join(replacement.ty(sea).unwrap(), ty), sea);
            self.dead_code_elimination(replacement, sea);
            return Some(replacement);
        }

        // Ask each node for a better replacement
        if let idealized @ Some(_) = self.idealize(sea) {
            return idealized; // Something changes, report progress
        }

        if old == self.ty(sea) {
            sea.iter_nop_cnt += 1;
            None
        } else {
            Some(self) // Report progress
        }
    }

    /// Set the type.  Assert monotonic progress.
    /// If changing, add users to worklist.
    pub(crate) fn set_type(self, ty: Ty<'t>, sea: &mut Nodes<'t>) -> Option<Ty<'t>> {
        let old = self.ty(sea);
        if let Some(old) = old {
            debug_assert!(sea.types.isa(ty, old), "{ty} isn't a {old}");
        }
        if old == Some(ty) {
            return old;
        }
        sea.ty[self] = Some(ty); // Set _type late for easier assert debugging

        for o in &sea.outputs[self] {
            sea.iter_peeps.add(*o);
        }
        self.move_deps_to_worklist(sea);
        old
    }

    fn dead_code_elimination(self, new: Node, sea: &mut Nodes) {
        if new != self && self.is_unused(sea) && !self.is_dead(sea) {
            new.keep(sea);
            self.kill(sea);
            new.unkeep(sea);
        }
    }

    pub(crate) fn compute(self, sea: &mut Nodes<'t>) -> Ty<'t> {
        let types = sea.types;
        let in_ty = |i: usize| self.inputs(sea)[i].unwrap().ty(sea).unwrap();
        let high = |t: Ty<'t>| t.is_high(types);

        match &sea[self] {
            Op::Constant(ty) => *ty,
            Op::XCtrl => types.xctrl,
            Op::Return => {
                let ctrl = self.inputs(sea)[0].unwrap().ty(sea).unwrap();
                let expr = self.inputs(sea)[1].unwrap().ty(sea).unwrap();
                *types.get_tuple_from_array([ctrl, expr])
            }
            Op::Start(s) => *s.args,
            Op::Add => {
                let (t1, t2) = (in_ty(1), in_ty(2));
                if high(t1) || high(t2) {
                    return *types.int_top;
                }
                if let (Some(t1), Some(t2)) = (t1.to_int(), t2.to_int()) {
                    if let (Some(c1), Some(c2)) = (t1.value(), t2.value()) {
                        return *types.get_int(c1.wrapping_add(c2));
                    }
                    // Fold ranges like {0-1} + {2-3} into {2-4}.
                    if let Some(min) = t1.min().checked_add(t2.min()) {
                        if let Some(max) = t1.max().checked_add(t2.max()) {
                            return *types.make_int(min, max);
                        }
                    }
                }
                *types.int_bot
            }
            Op::Sub => {
                let (t1, t2) = (in_ty(1), in_ty(2));
                if high(t1) || high(t2) {
                    return *types.int_top;
                }
                if let (Some(t1), Some(t2)) = (t1.to_int(), t2.to_int()) {
                    if let (Some(c1), Some(c2)) = (t1.value(), t2.value()) {
                        return *types.get_int(c1.wrapping_sub(c2));
                    }
                    // Fold ranges like {0-1} + {2-3} into {2-4}.
                    if let Some(min) = t1.min().checked_sub(t2.max()) {
                        if let Some(max) = t1.max().checked_sub(t2.min()) {
                            return *types.make_int(min, max);
                        }
                    }
                }
                // Sub of same is 0
                if self.inputs(sea)[1] == self.inputs(sea)[2] {
                    *sea.types.int_zero
                } else {
                    *types.int_bot
                }
            }
            Op::Mul => {
                let (t1, t2) = (in_ty(1), in_ty(2));
                if high(t1) || high(t2) {
                    return *types.int_top;
                }
                if let (Some(t1), Some(t2)) = (t1.to_int(), t2.to_int()) {
                    if t1 == types.int_zero || t2 == types.int_zero {
                        return *types.int_zero;
                    }
                    if let (Some(c1), Some(c2)) = (t1.value(), t2.value()) {
                        return *types.get_int(c1.wrapping_mul(c2));
                    }
                }
                *types.int_bot
            }
            Op::Div => {
                let (t1, t2) = (in_ty(1), in_ty(2));
                if high(t1) || high(t2) {
                    return *types.int_top;
                }
                if let (Some(t1), Some(t2)) = (t1.to_int(), t2.to_int()) {
                    if let (Some(c1), Some(c2)) = (t1.value(), t2.value()) {
                        return if c2 == 0 {
                            *types.int_zero
                        } else {
                            *types.get_int(c1.wrapping_div(c2))
                        };
                    }
                }
                *types.int_bot
            }
            Op::Minus => {
                let t = in_ty(1);
                if let Some(t) = t.to_int() {
                    return *if high(*t) {
                        types.int_top
                    } else if t == types.int_bot || t.min() == i64::MIN || t.max() == i64::MIN {
                        types.int_bot
                    } else {
                        types.make_int(-t.max(), -t.min())
                    };
                }
                types.int_top.meet(t, types)
            }
            Op::MinusF => {
                *if let Some(t) = in_ty(1).to_float() {
                    t.value().map(|v| types.get_float(-v)).unwrap_or(t)
                } else {
                    types.float_bot
                }
            }
            Op::Scope(_) | Op::ScopeMin => *types.mem_bot,
            Op::Bool(op) => {
                let (t1, t2) = (in_ty(1), in_ty(2));
                if high(t1) || high(t2) {
                    return types.int_bool.dual(types);
                }
                if let (Some(t1), Some(t2)) = (t1.to_int(), t2.to_int()) {
                    return *match op {
                        BoolOp::EQ => {
                            if t1 == t2 && t1.is_constant() {
                                types.int_true
                            } else if t1.max() < t2.min() || t1.min() > t2.max() {
                                types.int_false
                            } else {
                                types.int_bool
                            }
                        }
                        BoolOp::LT => {
                            if t1.max() < t2.min() {
                                types.int_true
                            } else if t1.min() >= t2.max() {
                                types.int_false
                            } else {
                                types.int_bool
                            }
                        }
                        BoolOp::LE => {
                            if t1.max() <= t2.min() {
                                types.int_true
                            } else if t1.min() > t2.max() {
                                types.int_false
                            } else {
                                types.int_bool
                            }
                        }
                        _ => unreachable!(),
                    };
                }
                if let (Some(t1), Some(t2)) = (t1.to_float(), t2.to_float()) {
                    if let (Some(c1), Some(c2)) = (t1.value(), t2.value()) {
                        return *types.get_bool(match op {
                            BoolOp::EQF => c1 == c2,
                            BoolOp::LTF => c1 < c2,
                            BoolOp::LEF => c1 <= c2,
                            _ => unreachable!(),
                        });
                    }
                }
                *types.int_bool
            }
            Op::Not => {
                let t = in_ty(1);
                if high(t) {
                    return types.int_bool.dual(types);
                }
                match &*t {
                    Type::Int(i) => match (i.min, i.max) {
                        _ if i.max < 0 || i.min > 0 => *types.int_false,
                        (0, 0) => *types.int_true,
                        _ => *types.int_bool,
                    },
                    Type::Float(f) => {
                        if f.sz == 0 {
                            if f.con() == 0.0 {
                                *types.int_true
                            } else {
                                *types.int_false
                            }
                        } else {
                            t
                        }
                    }
                    Type::MemPtr(p) => {
                        // top->top, bot->bot, null->1, *void->0, not-null ptr->0, ptr/nil->bot
                        // If input in null then true
                        // If input is not null ptr then false
                        *if t == *types.ptr_null {
                            types.int_true
                        } else if !p.nil {
                            types.int_false
                        } else {
                            types.int_bool
                        }
                    }
                    _ => {
                        // Only doing NOT on ints and ptrs
                        assert!(t == types.top || t == types.bot);
                        *types.int_bool
                    }
                }
            }
            Op::Proj(n) | Op::CProj(n) => {
                match self.inputs(sea)[0].and_then(|n| n.ty(sea)).as_deref() {
                    Some(Type::Tuple(types)) => types[n.index],
                    _ => types.bot,
                }
            }
            Op::If(IfOp::Never) => *types.if_both,
            Op::If(IfOp::Cond) => {
                let s = self.to_if(sea).unwrap();

                // If the If node is not reachable then neither is any following Proj
                let ctrl_ty = self.inputs(sea)[0].unwrap().ty(sea);
                if ctrl_ty != Some(types.ctrl) && ctrl_ty != Some(types.bot) {
                    return *types.if_neither;
                }

                let t = s.pred(sea).unwrap().ty(sea).unwrap();

                // High types mean NEITHER side is reachable.
                // Wait until the type falls to decide which way to go.
                if t == types.top || t == *types.int_top {
                    return *types.if_neither;
                }

                // If constant is 0 then false branch is reachable
                // Else true branch is reachable
                if t.is_constant(types) {
                    return *if t.make_init(types) == t {
                        types.if_false
                    } else {
                        types.if_true
                    };
                }

                // Integers allow non-zero ranges: "1-65535"
                if t.make_zero(types).meet(t, types) != t {
                    return *types.if_true;
                }
                *types.if_both
            }
            Op::Phi(_) => {
                let region = self.inputs(sea)[0].unwrap();
                if !region.is_region(sea) {
                    let t = region.ty(sea).unwrap();
                    if t == types.xctrl {
                        if self.ty(sea).is_some_and(|t| t.is_mem()) {
                            *types.mem_top
                        } else {
                            types.top
                        }
                    } else {
                        t
                    }
                } else if Nodes::in_progress(&sea.ops, &sea.inputs, self) {
                    // During parsing Phis have to be computed type pessimistically.
                    sea[self.to_phi(sea).unwrap()].ty
                } else {
                    // Set type to local top of the starting type
                    let mut t = sea[self.to_phi(sea).unwrap()].ty.lub(sea.types);

                    for i in 1..self.inputs(sea).len() {
                        // If the region's control input is live, add this as a dependency
                        // to the control because we can be peeped should it become dead.
                        if region.inputs(sea)[i].unwrap().add_dep(self, sea).ty(sea)
                            != Some(types.xctrl)
                        {
                            t = self.inputs(sea)[i].unwrap().ty(sea).unwrap().meet(t, types);
                        }
                    }
                    t
                }
            }
            Op::Region { .. } => {
                if Nodes::in_progress(&sea.ops, &sea.inputs, self) {
                    types.ctrl
                } else {
                    self.inputs(sea).iter().skip(1).fold(types.xctrl, |t, n| {
                        t.meet(n.unwrap().ty(sea).unwrap(), types)
                    })
                }
            }
            Op::Loop => {
                if Nodes::in_progress(&sea.ops, &sea.inputs, self) {
                    types.ctrl
                } else {
                    self.inputs(sea)[1].unwrap().ty(sea).unwrap()
                }
            }
            Op::Stop => types.bot,
            Op::Cast(t) => in_ty(1).join(*t, types),
            Op::Load(_) => {
                let s = self.to_load(sea).unwrap();
                if let Some(mem) = s.mem(sea).unwrap().ty(sea).and_then(|t| t.to_mem()) {
                    // Update declared forward ref to the actual
                    if sea[s].declared_type.is_fref() {
                        if let Some(tmp) = mem.data().t.to_mem_ptr() {
                            if !tmp.is_fref() {
                                sea[s].declared_type = *tmp;
                            }
                        }
                    }
                    // No lifting if ptr might null-check
                    if s.err(sea).is_none() {
                        return sea[s].declared_type.join(mem.data().t, types);
                    }
                }
                sea[s].declared_type
            }
            Op::Store(_) => {
                let s = self.to_store(sea).unwrap();
                let mut val = s.val(sea).unwrap().ty(sea).unwrap();
                let mem = s.mem(sea).unwrap().ty(sea).unwrap().to_mem().unwrap(); // Invariant
                let mut t = types.bot; // No idea on field contents

                // Same alias, lift val to the declared type and then meet into other fields
                if mem.data().alias == sea[s].alias {
                    // Update declared forward ref to the actual
                    if sea[s].declared_ty.is_fref() {
                        if let Some(tmp) = val.to_mem_ptr() {
                            if !tmp.is_fref() {
                                sea[s].declared_ty = *tmp;
                            }
                        }
                    }
                    val = val.join(sea[s].declared_ty, types);
                    t = val.meet(mem.data().t, types);
                }
                *types.get_mem(sea[s].alias, t)
            }
            Op::New(ptr) => {
                let fs = ptr.data().to.fields();
                let mut ts = Vec::with_capacity(fs.len() + 2);
                ts.push(types.ctrl);
                ts.push(**ptr);
                for (i, f) in fs.iter().enumerate() {
                    let mem = in_ty(i + 2).as_mem();
                    let tfld = in_ty(2 + fs.len() + i).meet(mem.data().t, types);
                    ts.push(*types.get_mem(f.alias, tfld));
                }
                *types.get_tuple_from_slice(&ts)
            }
            Op::AddF => self.compute_binary_float(|a, b| a + b, sea),
            Op::DivF => self.compute_binary_float(|a, b| a / b, sea),
            Op::MulF => self.compute_binary_float(|a, b| a * b, sea),
            Op::SubF => self.compute_binary_float(|a, b| a - b, sea),
            Op::And => {
                let (t1, t2) = (in_ty(1), in_ty(2));
                if high(t1) || high(t2) {
                    return *types.int_top;
                }
                if let (Some(t1), Some(t2)) = (t1.to_int(), t2.to_int()) {
                    if let (Some(c1), Some(c2)) = (t1.value(), t2.value()) {
                        return *types.get_int(c1 & c2);
                    }
                    // Sharpen allowed bits if either value is narrowed
                    let mask = t1.mask(types) & t2.mask(types);
                    return *if mask < 0 {
                        types.int_bot
                    } else {
                        types.make_int(0, mask)
                    };
                }
                *types.int_bot
            }
            Op::Or => {
                let (t1, t2) = (in_ty(1), in_ty(2));
                if high(t1) || high(t2) {
                    return *types.int_top;
                }
                if let (Some(t1), Some(t2)) = (t1.to_int(), t2.to_int()) {
                    if let (Some(c1), Some(c2)) = (t1.value(), t2.value()) {
                        return *types.get_int(c1 | c2);
                    }
                }
                *types.int_bot
            }
            Op::Xor => {
                let (t1, t2) = (in_ty(1), in_ty(2));
                if high(t1) || high(t2) {
                    return *types.int_top;
                }
                if let (Some(t1), Some(t2)) = (t1.to_int(), t2.to_int()) {
                    if let (Some(c1), Some(c2)) = (t1.value(), t2.value()) {
                        return *types.get_int(c1 ^ c2);
                    }
                }
                *types.int_bot
            }
            Op::Sar => {
                let (t1, t2) = (in_ty(1), in_ty(2));
                if high(t1) || high(t2) {
                    return *types.int_top;
                }
                if let (Some(t1), Some(t2)) = (t1.to_int(), t2.to_int()) {
                    if t1 == types.int_zero {
                        return *t1;
                    }
                    if let (Some(c1), Some(c2)) = (t1.value(), t2.value()) {
                        return *types.get_int(Sar::op(c1, c2));
                    }
                    if let Some(log) = t2.value() {
                        let s = (63 - log) & 63;
                        return *types.make_int(-1 << s, (1 << s) - 1);
                    }
                }
                *types.int_bot
            }
            Op::Shl => {
                let (t1, t2) = (in_ty(1), in_ty(2));
                if high(t1) || high(t2) {
                    return *types.int_top;
                }
                if let (Some(t1), Some(t2)) = (t1.to_int(), t2.to_int()) {
                    if t1 == types.int_zero {
                        return *t1;
                    }
                    if let (Some(c1), Some(c2)) = (t1.value(), t2.value()) {
                        return *types.get_int(Shl::op(c1, c2));
                    }
                }
                *types.int_bot
            }
            Op::Shr => {
                let (t1, t2) = (in_ty(1), in_ty(2));
                if high(t1) || high(t2) {
                    return *types.int_top;
                }
                if let (Some(t1), Some(t2)) = (t1.to_int(), t2.to_int()) {
                    if t1 == types.int_zero {
                        return *t1;
                    }
                    if let (Some(c1), Some(c2)) = (t1.value(), t2.value()) {
                        return *types.get_int(Shr::op(c1, c2));
                    }
                }
                *types.int_bot
            }
            Op::Cfg => unreachable!(),
            Op::ReadOnly => {
                let t = in_ty(1);
                t.to_mem_ptr().map(|t| t.make_ro(types)).unwrap_or(t)
            }
            Op::RoundF32 => {
                let t = in_ty(1);
                t.to_float()
                    .and_then(|t| t.value().map(|v| *types.get_float(v as f32 as f64)))
                    .unwrap_or(t)
            }
            Op::ToFloat => *in_ty(1)
                .to_int()
                .and_then(|t| t.value().map(|v| types.get_float(v as f64)))
                .unwrap_or(types.float_bot),
            Op::Struct(s) => match s.data().fields {
                None => *types.struct_bot,
                Some(fs) => {
                    let new_fs = fs
                        .iter()
                        .enumerate()
                        .map(|(i, f)| Field {
                            ty: self.inputs(sea)[i]
                                .map(|i| i.ty(sea).unwrap())
                                .unwrap_or(types.top),
                            ..*f
                        })
                        .collect::<Vec<_>>();
                    *types.get_struct(s.name(), &new_fs)
                }
            },
        }
    }

    fn compute_binary_float<F: FnOnce(f64, f64) -> f64>(self, op: F, sea: &Nodes<'t>) -> Ty<'t> {
        let t1 = self.inputs(sea)[1].unwrap().ty(sea).unwrap();
        let t2 = self.inputs(sea)[2].unwrap().ty(sea).unwrap();

        if let Some(v1) = t1.to_float().and_then(|t| t.value()) {
            if let Some(v2) = t2.to_float().and_then(|t| t.value()) {
                return *sea.tys.get_float(op(v1, v2));
            }
        }
        t1.meet(t2, sea.tys)
    }
}
