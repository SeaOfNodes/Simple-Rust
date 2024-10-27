use crate::datastructures::id_vec::IdVec;
use crate::sea_of_nodes::nodes::{Node, Nodes, Op, OpVec};
use std::collections::hash_map::RawEntryMut;
use std::hash::{BuildHasher, DefaultHasher, Hash, Hasher};
use std::num::NonZeroU32;

/// NOTE: we derive Eq for re-hashing and removal, but for deduplication lookups we use the more relaxed `Self::equals`
#[derive(Copy, Clone, Eq, PartialEq)]
pub struct GvnEntry {
    hash: NonZeroU32,
    node: Node,
}

impl Hash for GvnEntry {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.hash.hash(state);
    }
}

impl Node {
    /// Returns a different semantically equal node, if one is present in the gvn table
    /// or inserts this one if not already present.
    pub fn global_value_numbering(self, sea: &mut Nodes) -> Option<Node> {
        if sea.hash[self].is_none() {
            let entry = GvnEntry {
                hash: self.hash_code(sea),
                node: self,
            };
            let h = sea.gvn.hasher().hash_one(entry);
            match sea.gvn.raw_entry_mut().from_hash(h, |o| {
                entry.hash == o.hash && entry.node.equals(o.node, &sea.ops, &sea.inputs)
            }) {
                RawEntryMut::Vacant(v) => {
                    v.insert(entry, ()); // Put in table now
                    sea.hash[self] = Some(entry.hash); // hash is set iff in table
                }
                RawEntryMut::Occupied(o) => {
                    return Some(o.key().node);
                }
            }
        }
        None
    }

    /// If the _hash is set, then the Node is in the GVN table; remove it.
    pub fn unlock(self, sea: &mut Nodes) {
        if let Some(hash) = sea.hash[self].take() {
            sea.gvn
                .remove(&GvnEntry { hash, node: self })
                .expect("hash is set iff in table");
        }
    }

    /// Two nodes are equal if they have the same inputs and the same "opcode"
    /// which means the same Java class, plus same internal parts.
    fn equals(self, that: Node, ops: &OpVec, inputs: &IdVec<Node, Vec<Option<Node>>>) -> bool {
        if self == that {
            return true;
        }

        if ops[self].operation() != ops[that].operation() {
            return false;
        }

        if inputs[self] != inputs[that] {
            return false;
        }

        match (&ops[self], &ops[that]) {
            (Op::Constant(c1), Op::Constant(c2)) => c1 == c2,
            (Op::Phi(_) | Op::Region { .. } | Op::Loop, _) => {
                !Nodes::in_progress(ops, inputs, self)
            }
            (Op::Proj(p1), Op::Proj(p2)) => p1.index == p2.index,
            (Op::Load(m1), Op::Load(m2)) => m1.alias == m2.alias,
            (Op::Store(m1), Op::Store(m2)) => m1.alias == m2.alias,
            (Op::New(_), Op::New(_)) => false,
            _ => true,
        }
    }

    /// Hash of opcode and inputs
    fn hash_code(self, sea: &mut Nodes) -> NonZeroU32 {
        if let Some(hash) = sea.hash[self] {
            return hash;
        }

        let h = &mut DefaultHasher::new();
        sea[self].operation().hash(h);
        match &sea[self] {
            Op::Constant(c) => c.hash(h),
            Op::Proj(p) => p.index.hash(h),
            Op::Load(m) => m.alias.hash(h),
            Op::Store(m) => m.alias.hash(h),
            _ => {}
        };
        self.inputs(sea).hash(h);

        let hash = h.finish();
        let mut hash = (hash >> 32) as u32 ^ hash as u32;
        if hash == 0 {
            hash = 0xDEADBEEF; // Bad hash, so use some junky thing
        }

        NonZeroU32::new(hash).unwrap()
    }
}
