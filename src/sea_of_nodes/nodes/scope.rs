use std::collections::HashMap;

use crate::sea_of_nodes::nodes::index::ScopeId;
use crate::sea_of_nodes::nodes::{Node, NodeId, Nodes};
use crate::sea_of_nodes::types::Ty;

#[derive(Clone, Debug)]
pub struct ScopeNode<'t> {
    pub scopes: Vec<HashMap<String, (usize, Ty<'t>)>>,
}

impl<'t> ScopeNode<'t> {
    pub const CTRL: &'static str = "$ctrl";
    pub const ARG0: &'static str = "arg";

    fn lookup(&self, name: &str) -> Option<&(usize, Ty<'t>)> {
        self.scopes.iter().rev().flat_map(|x| x.get(name)).next()
    }
}

impl<'t> Nodes<'t> {
    pub fn scope_push(&mut self, scope: ScopeId) {
        self[scope].scopes.push(HashMap::new());
    }

    pub fn scope_pop(&mut self, scope: ScopeId) {
        let last = self[scope].scopes.pop().unwrap();
        self.pop_n(*scope, last.len());
    }

    pub fn scope_define(
        &mut self,
        scope: ScopeId,
        name: String,
        declared_ty: Ty<'t>,
        value: NodeId,
    ) -> Result<(), ()> {
        let len = self.inputs[scope].len();
        let syms = self[scope].scopes.last_mut().unwrap();
        if let Some(_old) = syms.insert(name, (len, declared_ty)) {
            return Err(());
        }
        self.add_def(*scope, Some(value));
        Ok(())
    }

    pub fn scope_lookup(&mut self, scope: ScopeId, name: &str) -> Result<NodeId, ()> {
        let nesting_level = self[scope].scopes.len() - 1;
        self.scope_lookup_update(scope, name, None, nesting_level)
            .ok_or(())
    }

    pub fn scope_update(
        &mut self,
        scope: ScopeId,
        name: &str,
        value: NodeId,
    ) -> Result<NodeId, ()> {
        let nesting_level = self[scope].scopes.len() - 1;
        self.scope_lookup_update(scope, name, Some(value), nesting_level)
            .ok_or(())
    }

    fn scope_lookup_update(
        &mut self,
        scope: ScopeId,
        name: &str,
        value: Option<NodeId>,
        nesting_level: usize,
    ) -> Option<NodeId> {
        let syms = &mut self[scope].scopes[nesting_level];
        if let Some((index, declared_ty)) = syms.get(name).copied() {
            let mut old = self.inputs[scope][index];

            if let Some(loop_) = old {
                if let Some(loop_) = self.to_scope(loop_) {
                    // Lazy Phi!
                    let loop_phi = self.inputs[loop_][index];
                    old = if loop_phi.is_some_and(|p| {
                        matches!(&self[p], Node::Phi(_))
                            && self.inputs[loop_][0] == self.inputs[p][0]
                    }) {
                        // Loop already has a real Phi, use it
                        loop_phi
                    } else {
                        // Set real Phi in the loop head
                        // The phi takes its one input (no backedge yet) from a recursive
                        // lookup, which might have insert a Phi in every loop nest.

                        let recursive = self.scope_lookup_update(loop_, name, None, nesting_level);
                        let new_phi = self.create_peepholed(Node::make_phi(
                            name.to_string(),
                            declared_ty,
                            vec![self.inputs[loop_][0], recursive, None],
                        ));

                        self.set_def(*loop_, index, Some(new_phi));
                        Some(new_phi)
                    };
                    self.set_def(*scope, index, old);
                }
            }

            // Not lazy, so this is the answer
            if value.is_some() {
                self.set_def(*scope, index, value);
                value
            } else {
                old
            }
        } else if nesting_level > 0 {
            self.scope_lookup_update(scope, name, value, nesting_level - 1)
        } else {
            None
        }
    }

    pub fn scope_dup(&mut self, scope: ScopeId, lazy_loop_phis: bool) -> ScopeId {
        let clone = self[scope].clone();
        let dup = self.create((Node::Scope(clone), vec![]));
        let dup = self.to_scope(dup).unwrap();

        self.add_def(*dup, self.inputs[scope][0]); // ctrl
        for i in 1..self.inputs[scope].len() {
            // For lazy phis on loops we use a sentinel
            // that will trigger phi creation on update
            self.add_def(
                *dup,
                if lazy_loop_phis {
                    Some(*scope)
                } else {
                    self.inputs[scope][i]
                },
            );
        }
        dup
    }

    pub fn scope_reverse_names(&self, scope: ScopeId) -> Vec<Option<String>> {
        let mut names = vec![None; self.inputs[scope].len()];
        for syms in &self[scope].scopes {
            for (name, &(index, _)) in syms {
                debug_assert!(names[index].is_none());
                names[index] = Some(name.clone());
            }
        }
        names
    }

    pub fn scope_merge(&mut self, this: ScopeId, that: ScopeId) -> NodeId {
        let c1 = self.inputs[this][0];
        let c2 = self.inputs[that][0];
        let region = self.create(Node::make_region(vec![None, c1, c2]));
        self.keep(region);
        self.set_def(*this, 0, Some(region)); // set ctrl

        let names = self.scope_reverse_names(this);

        // Note that we skip i==0, which is bound to '$ctrl'
        for (i, name) in names.into_iter().enumerate().skip(1) {
            let this_in = self.inputs[this][i];
            let that_in = self.inputs[that][i];
            if this_in != that_in {
                // If we are in lazy phi mode we need to a lookup
                // by name as it will trigger a phi creation
                let name = name.unwrap();
                let this_l = self.scope_lookup(this, &name).unwrap();
                let that_l = self.scope_lookup(that, &name).unwrap();

                let declared_ty = self[this].lookup(&name).unwrap().1;
                let phi = self.create_peepholed(Node::make_phi(
                    name,
                    declared_ty,
                    vec![Some(region), Some(this_l), Some(that_l)],
                ));
                self.set_def(*this, i, Some(phi));
            }
        }

        self.kill(*that);

        self.unkeep(region);
        self.peephole(region)
    }

    /// Merge the backedge scope into this loop head scope
    /// We set the second input to the phi from the back edge (i.e. loop body)
    pub fn scope_end_loop(&mut self, head: ScopeId, back: ScopeId, exit: ScopeId) {
        let ctrl = self.inputs[head][0].unwrap();
        assert!(matches!(&self[ctrl], Node::Loop));
        assert!(Self::in_progress(&self.nodes, &self.inputs, ctrl));

        self.set_def(ctrl, 2, self.inputs[back][0]);

        for i in 1..self.inputs[head].len() {
            if self.inputs[back][i] != Some(*head) {
                let phi = self.inputs[head][i].unwrap();
                assert_eq!(self.inputs[phi][0], Some(ctrl));
                assert_eq!(self.inputs[phi][2], None);
                self.set_def(phi, 2, self.inputs[back][i]);
            }
            if self.inputs[exit][i] == Some(*head) {
                // Replace a lazy-phi on the exit path also
                self.set_def(*exit, i, self.inputs[head][i])
            }
        }

        self.kill(*back); // Loop backedge is dead

        // Now one-time do a useless-phi removal
        for i in 1..self.inputs[head].len() {
            if let Some(phi) = self.inputs[head][i] {
                if let Node::Phi(_) = &self[phi] {
                    // Do an eager useless-phi removal
                    let in_ = self.peephole(phi);
                    for &o in &self.outputs[phi] {
                        self.iter_peeps.add(o);
                    }
                    self.move_deps_to_worklist(phi);
                    if in_ != phi {
                        self.subsume(phi, in_);
                        self.set_def(*head, i, Some(in_)); // Set the update back into Scope
                    }
                }
            }
        }
    }
}
