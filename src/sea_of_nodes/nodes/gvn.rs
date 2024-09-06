use crate::datastructures::id_vec::IdVec;
use crate::sea_of_nodes::nodes::{Node, NodeId, NodeVec, Nodes};
use std::collections::hash_map::RawEntryMut;
use std::hash::{BuildHasher, DefaultHasher, Hash, Hasher};
use std::num::NonZeroU32;

/// NOTE: we derive Eq for re-hashing and removal, but for deduplication lookups we use the more relaxed `Self::equals`
#[derive(Copy, Clone, Eq, PartialEq)]
pub struct GvnEntry {
    hash: NonZeroU32,
    node: NodeId,
}

impl Hash for GvnEntry {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.hash.hash(state);
    }
}

impl<'t> Nodes<'t> {
    /// Returns a different semantically equal node, if one is present in the gvn table
    /// or inserts this one if not already present.
    pub fn global_value_numbering(&mut self, node: NodeId) -> Option<NodeId> {
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
                    self.hash[node] = Some(entry.hash); // hash is set iff in table
                }
                RawEntryMut::Occupied(o) => {
                    return Some(o.key().node);
                }
            }
        }
        None
    }

    /// If the _hash is set, then the Node is in the GVN table; remove it.
    pub fn unlock(&mut self, node: NodeId) {
        if let Some(hash) = self.hash[node].take() {
            self.gvn
                .remove(&GvnEntry { hash, node })
                .expect("hash is set iff in table");
        }
    }

    /// Two nodes are equal if they have the same inputs and the same "opcode"
    /// which means the same Java class, plus same internal parts.
    fn equals(
        nodes: &NodeVec<'t>,
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
            _ => true,
        }
    }

    /// Hash of opcode and inputs
    fn hash_code(&mut self, node: NodeId) -> NonZeroU32 {
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
