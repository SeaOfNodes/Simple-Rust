use std::collections::HashMap;

use crate::sea_of_nodes::nodes::{Node, NodeId, Nodes};
use crate::sea_of_nodes::types::Types;

#[derive(Clone, Debug)]
pub struct ScopeNode {
    pub scopes: Vec<HashMap<String, usize>>,
}

impl ScopeNode {
    pub const CTRL: &'static str = "$ctrl";
    pub const ARG0: &'static str = "arg";
}

impl<'t> Nodes<'t> {
    pub(crate) fn scope(&self, scope_node: NodeId) -> &ScopeNode {
        let Node::Scope(scope) = &self[scope_node] else {
            panic!("Must be called with a scope node id")
        };
        scope
    }
    pub(crate) fn scope_mut(&mut self, scope_node: NodeId) -> &mut ScopeNode {
        let Node::Scope(scope) = &mut self[scope_node] else {
            panic!("Must be called with a scope node id")
        };
        scope
    }
    pub fn scope_push(&mut self, scope_node: NodeId) {
        self.scope_mut(scope_node).scopes.push(HashMap::new());
    }

    pub fn scope_pop(&mut self, scope_node: NodeId) {
        let last = self.scope_mut(scope_node).scopes.pop().unwrap();
        self.pop_n(scope_node, last.len());
    }

    pub fn scope_define(
        &mut self,
        scope_node: NodeId,
        name: String,
        value: NodeId,
    ) -> Result<(), ()> {
        let len = self.inputs[scope_node].len();
        let scope = self.scope_mut(scope_node);
        let syms = scope.scopes.last_mut().unwrap();
        if let Some(_old) = syms.insert(name, len) {
            return Err(());
        }
        self.add_def(scope_node, Some(value));
        Ok(())
    }

    pub fn scope_lookup(&mut self, scope_node: NodeId, name: &str, types: &mut Types<'t>) -> Result<NodeId, ()> {
        let nesting_level = self.scope_mut(scope_node).scopes.len() - 1;
        self.scope_lookup_update(scope_node, name, None, nesting_level, types)
            .ok_or(())
    }

    pub fn scope_update(
        &mut self,
        scope_node: NodeId,
        name: &str,
        value: NodeId,
        types: &mut Types<'t>,
    ) -> Result<NodeId, ()> {
        let nesting_level = self.scope_mut(scope_node).scopes.len() - 1;
        self.scope_lookup_update(scope_node, name, Some(value), nesting_level, types)
            .ok_or(())
    }

    fn scope_lookup_update(
        &mut self,
        scope_node: NodeId,
        name: &str,
        value: Option<NodeId>,
        nesting_level: usize,
        types: &mut Types<'t>,
    ) -> Option<NodeId> {
        let scope = self.scope_mut(scope_node);
        let syms = &mut scope.scopes[nesting_level];
        if let Some(index) = syms.get(name).copied() {
            let mut old = self.inputs[scope_node][index];

            if let Some(loop_) = old {
                if matches!(self[loop_], Node::Scope(_)) {
                    // Lazy Phi!
                    let loop_phi = self.inputs[loop_][index];
                    old = if loop_phi.is_some_and(|p| matches!(&self[p], Node::Phi(_)) && self.inputs[loop_][0] == self.inputs[p][0]) {
                        // Loop already has a real Phi, use it
                        loop_phi
                    } else {
                        // Set real Phi in the loop head
                        // The phi takes its one input (no backedge yet) from a recursive
                        // lookup, which might have insert a Phi in every loop nest.

                        let recursive = self.scope_lookup_update(loop_, name, None, nesting_level, types);
                        let new_phi = self.create_peepholed(types, Node::make_phi(name.to_string(), vec![self.inputs[loop_][0], recursive, None]));

                        self.set_def(loop_, index, Some(new_phi));
                        Some(new_phi)
                    };
                    self.set_def(scope_node, index, old);
                }
            }

            // Not lazy, so this is the answer
            if value.is_some() {
                self.set_def(scope_node, index, value);
                value
            } else {
                old
            }
        } else if nesting_level > 0 {
            self.scope_lookup_update(scope_node, name, value, nesting_level - 1, types)
        } else {
            None
        }
    }

    pub fn scope_dup(
        &mut self,
        scope_node: NodeId,
        lazy_loop_phis: bool,
    ) -> NodeId {
        let clone = self.scope_mut(scope_node).clone();
        let dup = self.create((Node::Scope(clone), vec![]));
        self.add_def(dup, self.inputs[scope_node][0]); // ctrl
        for i in 1..self.inputs[scope_node].len() {
            // For lazy phis on loops we use a sentinel
            // that will trigger phi creation on update
            self.add_def(dup, if lazy_loop_phis { Some(scope_node) } else { self.inputs[scope_node][i] });
        }
        dup
    }

    pub fn scope_reverse_names(&self, scope: NodeId) -> Vec<Option<String>> {
        let mut names = vec![None; self.inputs[scope].len()];
        let Node::Scope(this_scope) = &self[scope] else {
            panic!("expected scope");
        };
        for syms in &this_scope.scopes {
            for (name, &index) in syms {
                debug_assert!(names[index].is_none());
                names[index] = Some(name.clone());
            }
        }
        names
    }

    pub fn scope_merge(&mut self, this: NodeId, that: NodeId, types: &mut Types<'t>) -> NodeId {
        let c1 = self.inputs[this][0];
        let c2 = self.inputs[that][0];
        let region = self.create(Node::make_region(vec![None, c1, c2]));
        self.keep(region);

        let names = self.scope_reverse_names(this);

        // Note that we skip i==0, which is bound to '$ctrl'
        for (i, name) in names.into_iter().enumerate().skip(1) {
            let this_in = self.inputs[this][i];
            let that_in = self.inputs[that][i];
            if this_in != that_in {
                // If we are in lazy phi mode we need to a lookup
                // by name as it will trigger a phi creation
                let name = name.unwrap();
                let this_l = self.scope_lookup(this, &name, types).unwrap();
                let that_l = self.scope_lookup(that, &name, types).unwrap();

                let phi = self.create_peepholed(
                    types,
                    Node::make_phi(name, vec![Some(region), Some(this_l), Some(that_l)]),
                );
                self.set_def(this, i, Some(phi));
            }
        }

        self.kill(that);

        self.unkeep(region);
        self.peephole(region, types)
    }

    /// Merge the backedge scope into this loop head scope
    /// We set the second input to the phi from the back edge (i.e. loop body)
    pub fn scope_end_loop(
        &mut self,
        head: NodeId,
        back: NodeId,
        exit: NodeId,
        types: &mut Types<'t>,
    ) {
        let ctrl = self.inputs[head][0].unwrap();
        assert!(matches!(&self[ctrl], Node::Loop));
        assert!(self.in_progress(ctrl));

        self.set_def(ctrl, 2, self.inputs[back][0]);

        for i in 1..self.inputs[head].len() {
            if self.inputs[back][i] != Some(head) {
                let phi = self.inputs[head][i].unwrap();
                assert_eq!(self.inputs[phi][0], Some(ctrl));
                assert_eq!(self.inputs[phi][2], None);
                self.set_def(phi, 2, self.inputs[back][i]);
            }
            if self.inputs[exit][i] == Some(head) {
                // Replace a lazy-phi on the exit path also
                self.set_def(exit, i, self.inputs[head][i])
            }
        }

        self.kill(back); // Loop backedge is dead

        // Now one-time do a useless-phi removal
        for i in 1..self.inputs[head].len() {
            if let Some(phi) = self.inputs[head][i] {
                if let Node::Phi(_) = &self[phi] {
                    // Do an eager useless-phi removal
                    let in_ = self.peephole(phi, types);
                    if in_ != phi {
                        self.subsume(phi, in_);
                        self.set_def(head, i, Some(in_)); // Set the update back into Scope
                    }
                }
            }
        }
    }
}
