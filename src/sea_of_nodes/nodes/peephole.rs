use crate::sea_of_nodes::nodes::gvn::GvnEntry;
use crate::sea_of_nodes::nodes::{Node, NodeId, Nodes};
use crate::sea_of_nodes::types::{Int, Ty, Type};
use std::collections::hash_map::RawEntryMut;
use std::hash::BuildHasher;

impl<'t> Nodes<'t> {
    /// Try to peephole at this node and return a better replacement Node.
    /// Always returns some not-null Node (often this).
    #[must_use]
    pub fn peephole(&mut self, node: NodeId) -> NodeId {
        if self.disable_peephole {
            self.ty[node] = Some(self.compute(node));
            node // Peephole optimizations turned off
        } else if let Some(n) = self.peephole_opt(node) {
            let n = self.peephole(n);
            self.dead_code_elimination(node, n)
        } else {
            node // Cannot return null for no-progress
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
    pub fn peephole_opt(&mut self, node: NodeId) -> Option<NodeId> {
        self.iter_cnt += 1;

        // Compute initial or improved Type
        let ty = self.compute(node);
        let old = self.set_type(node, ty);

        // Replace constant computations from non-constants with a constant node
        if !matches!(self[node], Node::Constant(_)) && ty.is_high_or_constant() {
            let constant = self.create(Node::make_constant(self.start, ty));
            return self.peephole_opt(constant);
        }

        // Global Value Numbering
        if self.hash[node].is_none() {
            let entry = GvnEntry {
                hash: self.hash_code(node),
                node,
            };
            let h = self.gvn.hasher().hash_one(entry);
            match self.gvn.raw_entry_mut().from_hash(h, |o| {
                entry.hash == o.hash && Self::equals(&self.nodes, &self.inputs, entry.node, o.node)
            }) {
                RawEntryMut::Vacant(v) => {
                    v.insert(entry, ()); // Put in table now
                    self.hash[node] = Some(entry.hash);
                }
                RawEntryMut::Occupied(o) => {
                    // Because of random worklist ordering, the two equal nodes
                    // might have different types.  Because of monotonicity, both
                    // types are valid.  To preserve monotonicity, the resulting
                    // shared Node has to have the best of both types.
                    let n = o.key().node;
                    self.set_type(n, self.types.join(self.ty[n].unwrap(), ty));

                    return Some(self.dead_code_elimination(node, n)); // Return previous; does Common Subexpression Elimination
                }
            }
        }

        // Ask each node for a better replacement
        if let idealized @ Some(_) = self.idealize(node) {
            return idealized; // Something changes, report progress
        }

        if old == self.ty[node] {
            self.iter_nop_cnt += 1;
            None
        } else {
            Some(node) // Report progress
        }
    }

    /// Set the type.  Assert monotonic progress.
    /// If changing, add users to worklist.
    pub(crate) fn set_type(&mut self, node: NodeId, ty: Ty<'t>) -> Option<Ty<'t>> {
        let old = self.ty[node];
        if let Some(old) = old {
            debug_assert!(self.types.isa(ty, old));
        }
        if old == Some(ty) {
            return old;
        }
        self.ty[node] = Some(ty); // Set _type late for easier assert debugging

        for o in &self.outputs[node] {
            self.iter_peeps.add(*o);
        }
        self.move_deps_to_worklist(node);
        old
    }

    fn dead_code_elimination(&mut self, old: NodeId, new: NodeId) -> NodeId {
        if new != old && self.is_unused(old) {
            self.keep(new);
            self.kill(old);
            self.unkeep(new);
        }
        new
    }

    pub(crate) fn compute(&mut self, node: NodeId) -> Ty<'t> {
        let types = self.types;
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
            Node::Add => self.compute_binary_int(node, i64::wrapping_add),
            Node::Sub => self.compute_binary_int(node, i64::wrapping_sub),
            Node::Mul => self.compute_binary_int(node, i64::wrapping_mul),
            Node::Div => {
                self.compute_binary_int(node, |a, b| if b == 0 { 0 } else { a.wrapping_div(b) })
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
            Node::Bool(op) => self.compute_binary_int(node, |x, y| op.compute(x, y) as i64),
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
            Node::If => {
                // If the If node is not reachable then neither is any following Proj
                let ctrl_ty = self.ty[self.inputs[node][0].unwrap()];
                if ctrl_ty != Some(types.ty_ctrl) && ctrl_ty != Some(types.ty_bot) {
                    return types.ty_if_neither;
                }

                let pred = self.inputs[node][1].unwrap();

                // If constant is 0 then false branch is reachable
                // Else true branch is reachable
                if let Some(Type::Int(Int::Constant(c))) = self.ty[pred].as_deref() {
                    return if *c == 0 {
                        types.ty_if_false
                    } else {
                        types.ty_if_true
                    };
                }

                // Hunt up the immediate dominator tree.  If we find an identical if
                // test on either the true or false branch, then this test matches.

                let mut dom = self.idom(node);
                let mut prior = node;
                while let Some(d) = dom {
                    if matches!(&self[d], Node::If) && self.inputs[d][1].unwrap() == pred {
                        return if let Node::Proj(proj) = &self[prior] {
                            // Repeated test, dominated on one side.  Test result is the same.
                            if proj.index == 0 {
                                types.ty_if_true
                            } else {
                                types.ty_if_false
                            }
                        } else {
                            // Repeated test not dominated on one side
                            self.ty[d].unwrap()
                        };
                    }

                    prior = d;
                    dom = self.idom(d);
                }

                types.ty_if_both
            }
            Node::Phi(_) => {
                if self.phi_no_or_in_progress_region(node) {
                    types.ty_bot
                } else {
                    self.inputs[node].iter().skip(1).fold(types.ty_top, |t, n| {
                        types.meet(t, self.ty[n.unwrap()].unwrap())
                    })
                }
            }
            Node::Region { .. } => {
                if Self::in_progress(&self.nodes, &self.inputs, node) {
                    types.ty_ctrl
                } else {
                    self.inputs[node]
                        .iter()
                        .skip(1)
                        .fold(types.ty_xctrl, |t, n| {
                            types.meet(t, self.ty[n.unwrap()].unwrap())
                        })
                }
            }
            Node::Loop => {
                if Self::in_progress(&self.nodes, &self.inputs, node) {
                    types.ty_ctrl
                } else {
                    let entry = self.inputs[node][1].unwrap();
                    self.ty[entry].unwrap()
                }
            }
            Node::Stop => types.ty_bot,
        }
    }

    fn compute_binary_int<F: FnOnce(i64, i64) -> i64>(&self, node: NodeId, op: F) -> Ty<'t> {
        let types = self.types;
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
            [_, _] => types.meet(first, second),
        }
    }
}
