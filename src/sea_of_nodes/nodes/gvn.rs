use crate::sea_of_nodes::nodes::{Node, NodeId, Nodes};
use std::hash::{DefaultHasher, Hash, Hasher};
use std::num::NonZeroU64;

impl<'t> Nodes<'t> {
    /// Two nodes are equal if they have the same inputs and the same "opcode"
    /// which means the same Java class, plus same internal parts.
    pub fn equals(&self, this: NodeId, that: NodeId) -> bool {
        if this == that {
            return true;
        }

        if self[this].operation() != self[that].operation() {
            return false;
        }

        if self.inputs[this] != self.inputs[that] {
            return false;
        }

        match (&self[this], &self[that]) {
            (Node::Constant(c1), Node::Constant(c2)) => c1 == c2,
            (Node::Phi(_) | Node::Region { .. } | Node::Loop, _) => !self.in_progress(this),
            (Node::Proj(p1), Node::Proj(p2)) => p1.index == p2.index,
            _ => false,
        }
    }

    /// If the _hash is set, then the Node is in the GVN table; remove it.
    pub fn unlock(&mut self, node: NodeId) {
        if self.hash[node].take().is_some() {
            let was_present = self.gvn.remove(&node);
            assert!(was_present);
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

        self.hash[node] = Some(hash);
        hash
    }
}
