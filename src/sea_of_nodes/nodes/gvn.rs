use crate::datastructures::id_vec::IdVec;
use crate::sea_of_nodes::nodes::{Node, NodeId, Nodes};
use std::collections::hash_map::RawEntryMut;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::num::NonZeroU64;

impl<'t> Nodes<'t> {
    /// Two nodes are equal if they have the same inputs and the same "opcode"
    /// which means the same Java class, plus same internal parts.
    pub fn equals(
        nodes: &IdVec<NodeId, Node<'t>>,
        inputs: &IdVec<NodeId, Vec<Option<NodeId>>>,
        this: NodeId,
        that: NodeId,
    ) -> bool {
        if this == that {
            return true;
        }

        if nodes[this].operation() != nodes[that].operation() {
            return false;
        }

        if inputs[this] != inputs[that] {
            return false;
        }

        match (&nodes[this], &nodes[that]) {
            (Node::Constant(c1), Node::Constant(c2)) => c1 == c2,
            (Node::Phi(_) | Node::Region { .. } | Node::Loop, _) => {
                !Self::in_progress(nodes, inputs, this)
            }
            (Node::Proj(p1), Node::Proj(p2)) => p1.index == p2.index,
            _ => false,
        }
    }

    /// If the _hash is set, then the Node is in the GVN table; remove it.
    pub fn unlock(&mut self, node: NodeId) {
        if let Some(hash) = self.hash[node].take() {
            match self.gvn.raw_entry_mut().from_hash(hash.get(), |n| {
                Self::equals(&self.nodes, &self.inputs, node, *n)
            }) {
                RawEntryMut::Occupied(o) => {
                    assert_eq!(o.key(), &node);
                    o.remove();
                }
                RawEntryMut::Vacant(_) => unreachable!(),
            }
        }
    }

    /// Hash of opcode and inputs
    pub fn hash_code(&mut self, node: NodeId) -> NonZeroU64 {
        if let Some(hash) = self.hash[node] {
            return hash;
        }

        let h = &mut DefaultHasher::new();
        self[node].operation().hash(h);
        match &self[node] {
            Node::Constant(c) => c.hash(h),
            Node::Proj(p) => p.index.hash(h),
            _ => {}
        };
        self.inputs[node].hash(h);

        let mut hash = h.finish();
        if hash == 0 {
            hash = 0xDEADBEEF; // Bad hash, so use some junky thing
        }

        let hash = NonZeroU64::new(hash).unwrap();
        hash
    }
}
