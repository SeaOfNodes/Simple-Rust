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

    pub fn scope_lookup(&mut self, scope_node: NodeId, name: &str) -> Result<NodeId, ()> {
        let nesting_level = self.scope_mut(scope_node).scopes.len() - 1;
        self.scope_lookup_update(scope_node, name, None, nesting_level)
            .ok_or(())
    }

    pub fn scope_update(
        &mut self,
        scope_node: NodeId,
        name: &str,
        value: NodeId,
    ) -> Result<NodeId, ()> {
        let nesting_level = self.scope_mut(scope_node).scopes.len() - 1;
        self.scope_lookup_update(scope_node, name, Some(value), nesting_level)
            .ok_or(())
    }

    fn scope_lookup_update(
        &mut self,
        scope_node: NodeId,
        name: &str,
        value: Option<NodeId>,
        nesting_level: usize,
    ) -> Option<NodeId> {
        let scope = self.scope_mut(scope_node);
        let syms = &mut scope.scopes[nesting_level];
        if let Some(index) = syms.get(name).copied() {
            let old = self.inputs[scope_node][index];
            if value.is_some() {
                self.set_def(scope_node, index, value);
                value
            } else {
                old
            }
        } else if nesting_level > 0 {
            self.scope_lookup_update(scope_node, name, value, nesting_level - 1)
        } else {
            None
        }
    }

    pub fn scope_dup(
        &mut self,
        scope_node: NodeId,
        at_loop: bool,
        types: &mut Types<'t>,
    ) -> NodeId {
        let clone = self.scope_mut(scope_node).clone();
        let dup = self.create((Node::Scope(clone), vec![]));
        self.add_def(dup, self.inputs[scope_node][0]); // ctrl
        for i in 1..self.inputs[scope_node].len() {
            if !at_loop {
                self.add_def(dup, self.inputs[scope_node][i])
            } else {
                let names = self.scope_reverse_names(scope_node);

                let ctrl = self.inputs[scope_node][0];
                let in_i = self.inputs[scope_node][i];
                let phi = self.create_peepholed(
                    types,
                    Node::make_phi(
                        names.into_iter().nth(i).unwrap().unwrap(),
                        vec![ctrl, in_i, None],
                    ),
                );
                self.add_def(dup, Some(phi));

                self.set_def(scope_node, i, self.inputs[dup][i]);
            }
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
                let phi = self.create_peepholed(
                    types,
                    Node::make_phi(name.unwrap(), vec![Some(region), this_in, that_in]),
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
        _exit: NodeId,
        types: &mut Types<'t>,
    ) {
        let ctrl = self.inputs[head][0].unwrap();
        assert!(matches!(&self[ctrl], Node::Loop));
        assert!(self.in_progress(ctrl));

        self.set_def(ctrl, 2, self.inputs[back][0]);

        for i in 1..self.inputs[head].len() {
            let phi = self.inputs[head][i].unwrap();
            assert_eq!(self.inputs[phi][0], Some(ctrl));
            assert_eq!(self.inputs[phi][2], None);
            self.set_def(phi, 2, self.inputs[back][i]);
            // Do an eager useless-phi removal
            let in_ = self.peephole(phi, types);
            if in_ != phi {
                self.subsume(phi, in_);
            }
        }

        self.kill(back); // Loop backedge is dead
    }
}
