use crate::datastructures::id_vec::IdVec;
use crate::sea_of_nodes::nodes::{Node, NodeId, Nodes};
use std::hash::{DefaultHasher, Hash, Hasher};
use std::num::NonZeroU32;

/// NOTE: we derive Eq for re-hashing and removal, but for deduplication lookups we use the more relaxed `Self::equals`
#[derive(Copy, Clone, Eq, PartialEq)]
pub struct GvnEntry {
    pub hash: NonZeroU32,
    pub node: NodeId,
}

impl Hash for GvnEntry {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.hash.hash(state);
    }
}

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
            self.gvn
                .remove(&GvnEntry { hash, node })
                .expect("was present");
        }
    }

    /// Hash of opcode and inputs
    pub fn hash_code(&mut self, node: NodeId) -> NonZeroU32 {
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

        let hash = h.finish();
        let mut hash = (hash >> 32) as u32 ^ hash as u32;
        if hash == 0 {
            hash = 0xDEADBEEF; // Bad hash, so use some junky thing
        }

        let hash = NonZeroU32::new(hash).unwrap();
        hash
    }
}
