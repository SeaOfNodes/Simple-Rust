use crate::sea_of_nodes::nodes::index::Constant;
use crate::sea_of_nodes::nodes::node::MemOpKind;
use crate::sea_of_nodes::nodes::{Node, Nodes, Op};
use crate::sea_of_nodes::types::{Int, Ty, Type};

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

        // System.out.printf("%-3d %-8s %s->%s%n", this._nid, this.label(), old, compute());
        // println!("{:<3} {:<8} {}->{}", self, &sea[self].label(), old.map(|t| t.to_string()).unwrap_or("null".to_string()), ty);

        // Replace constant computations from non-constants with a constant node
        if self.to_constant(sea).is_none() && ty.is_high_or_constant() {
            return Constant::new(ty, sea).peephole_opt(sea);
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
            debug_assert!(sea.types.isa(ty, old));
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
        match &sea[self] {
            Op::Constant(ty) => *ty,
            Op::Return => {
                let ctrl = self.inputs(sea)[0]
                    .and_then(|n| n.ty(sea))
                    .unwrap_or(types.ty_bot);
                let expr = self.inputs(sea)[1]
                    .and_then(|n| n.ty(sea))
                    .unwrap_or(types.ty_bot);
                types.get_tuple_from_array([ctrl, expr])
            }
            Op::Start(s) => s.args,
            Op::Add => self.compute_binary_int(i64::wrapping_add, sea),
            Op::Sub => self.compute_binary_int(i64::wrapping_sub, sea),
            Op::Mul => self.compute_binary_int(i64::wrapping_mul, sea),
            Op::Div => {
                self.compute_binary_int(|a, b| if b == 0 { 0 } else { a.wrapping_div(b) }, sea)
            }
            Op::Minus => {
                let Some(input) = self.inputs(sea)[1].and_then(|n| n.ty(sea)) else {
                    return types.ty_bot;
                };
                match &*input {
                    Type::Int(Int::Constant(v)) => types.get_int(v.wrapping_neg()),
                    _ => types.meet(types.ty_top, input),
                }
            }
            Op::Scope(_) => types.ty_bot,
            Op::Bool(op) => self.compute_binary_int(|x, y| op.compute(x, y) as i64, sea),
            Op::Not => {
                let t0 = self.inputs(sea)[1].unwrap().ty(sea).unwrap();
                match &*t0 {
                    Type::Int(i) => match i {
                        Int::Constant(0) => types.ty_int_one,
                        Int::Constant(_) => types.ty_int_zero,
                        _ => t0,
                    },
                    Type::Pointer(p) => {
                        // top->top, bot->bot, null->1, *void->0, not-null ptr->0, ptr/nil->bot
                        // If input in null then true
                        // If input is not null ptr then false
                        if t0 == types.ty_pointer_top {
                            types.ty_int_top
                        } else if t0 == types.ty_pointer_null {
                            types.ty_int_one
                        } else if !p.nil {
                            types.ty_int_zero
                        } else {
                            types.ty_int_bot
                        }
                    }
                    _ => {
                        // Only doing NOT on ints and ptrs
                        assert!(t0 == types.ty_top || t0 == types.ty_bot);
                        if t0 == types.ty_top {
                            t0
                        } else {
                            types.ty_bot
                        }
                    }
                }
            }
            Op::Proj(n) => {
                let Some(input) = self.inputs(sea)[0].and_then(|n| n.ty(sea)) else {
                    return types.ty_bot;
                };
                match &*input {
                    Type::Tuple { types } => types[n.index],
                    _ => unreachable!("proj node ctrl must always be tuple, if present"),
                }
            }
            Op::If => {
                // If the If node is not reachable then neither is any following Proj
                let ctrl_ty = self.inputs(sea)[0].unwrap().ty(sea);
                if ctrl_ty != Some(types.ty_ctrl) && ctrl_ty != Some(types.ty_bot) {
                    return types.ty_if_neither;
                }

                let pred = self.inputs(sea)[1].unwrap();
                let t = pred.ty(sea).unwrap();

                // High types mean NEITHER side is reachable.
                // Wait until the type falls to decide which way to go.
                if t == types.ty_top || t == types.ty_int_top {
                    return types.ty_if_neither;
                }

                // If constant is 0 then false branch is reachable
                // Else true branch is reachable
                if let Type::Int(Int::Constant(c)) = &*t {
                    return if *c == 0 {
                        types.ty_if_false
                    } else {
                        types.ty_if_true
                    };
                }

                // Hunt up the immediate dominator tree.  If we find an identical if
                // test on either the true or false branch, then this test matches.

                let mut dom = sea.idom(self);
                let mut prior = self;
                while let Some(d) = dom {
                    if matches!(&sea[d], Op::If) && d.inputs(sea)[1].unwrap() == pred {
                        return if let Op::Proj(proj) = &sea[prior] {
                            // Repeated test, dominated on one side.  Test result is the same.
                            if proj.index == 0 {
                                types.ty_if_true
                            } else {
                                types.ty_if_false
                            }
                        } else {
                            // Repeated test not dominated on one side
                            d.ty(sea).unwrap()
                        };
                    }

                    prior = d;
                    dom = sea.idom(d);
                }

                types.ty_if_both
            }
            Op::Phi(_) => {
                let region = self.inputs(sea)[0].unwrap();
                if !sea.instanceof_region(Some(region)) {
                    if region.ty(sea) == Some(types.ty_xctrl) {
                        types.ty_top
                    } else {
                        self.ty(sea).unwrap()
                    }
                } else if Nodes::in_progress(&sea.ops, &sea.inputs, self) {
                    // During parsing Phis have to be computed type pessimistically.
                    sea[self.to_phi(sea).unwrap()].ty
                } else {
                    // Set type to local top of the starting type
                    let mut t = sea
                        .types
                        .dual(sea.types.glb(sea[self.to_phi(sea).unwrap()].ty));

                    for i in 1..self.inputs(sea).len() {
                        // If the region's control input is live, add this as a dependency
                        // to the control because we can be peeped should it become dead.
                        let r_in_i = region.inputs(sea)[i].unwrap();
                        r_in_i.add_dep(self, sea);
                        let in_i = self.inputs(sea)[i].unwrap();
                        if r_in_i.ty(sea) != Some(types.ty_xctrl) {
                            t = types.meet(t, in_i.ty(sea).unwrap())
                        }
                    }
                    t
                }
            }
            Op::Region { .. } => {
                if Nodes::in_progress(&sea.ops, &sea.inputs, self) {
                    types.ty_ctrl
                } else {
                    self.inputs(sea)
                        .iter()
                        .skip(1)
                        .fold(types.ty_xctrl, |t, n| {
                            types.meet(t, n.unwrap().ty(sea).unwrap())
                        })
                }
            }
            Op::Loop => {
                if Nodes::in_progress(&sea.ops, &sea.inputs, self) {
                    types.ty_ctrl
                } else {
                    self.inputs(sea)[1].unwrap().ty(sea).unwrap()
                }
            }
            Op::Stop => types.ty_bot,
            Op::Cast(t) => types.join(self.inputs(sea)[1].unwrap().ty(sea).unwrap(), *t),
            Op::Mem(m) => match m.kind {
                MemOpKind::Load { declared_type } => declared_type,
                MemOpKind::Store => types.get_mem(m.alias),
            },
            Op::New(t) => *t,
        }
    }

    fn compute_binary_int<F: FnOnce(i64, i64) -> i64>(self, op: F, sea: &Nodes<'t>) -> Ty<'t> {
        let types = sea.types;
        let Some(first) = self.inputs(sea)[1].and_then(|n| n.ty(sea)) else {
            return types.ty_bot;
        };
        let Some(second) = self.inputs(sea)[2].and_then(|n| n.ty(sea)) else {
            return types.ty_bot;
        };

        match [&*first, &*second] {
            [Type::Int(Int::Constant(v1)), Type::Int(Int::Constant(v2))] => {
                types.get_int(op(*v1, *v2))
            }
            [_, _] => types.meet(first, second),
        }
    }
}
