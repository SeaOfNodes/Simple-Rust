use std::fmt;
use std::fmt::{Debug, Display};
use std::num::NonZeroU32;

use crate::datastructures::id::Id;
use crate::datastructures::id_vec::IdVec;
use crate::sea_of_nodes::nodes::{Nodes, Op};
use crate::sea_of_nodes::types::Ty;

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct Node(pub(super) NonZeroU32);

impl Node {
    pub const DUMMY: Node = Node(NonZeroU32::MAX);

    /// attemt to make code read more like the op version:
    ///
    /// node.in(1).in(2)._type // java
    /// self[self.inputs[self.inputs[node][1]?][2]?]
    /// node.get(&self.inputs)[1]?.get(&self.inputs)[2]?.get(&self.ty)
    /// node.inputs(self)[1]?.inputs(self)[2]?.ty(self) // doesn't allow separate borrowing
    pub fn get<T>(self, vec: &IdVec<Self, T>) -> &T {
        &vec[self]
    }

    pub fn inputs<'a>(self, nodes: &'a Nodes) -> &'a Vec<Option<Node>> {
        self.get(&nodes.inputs)
    }

    pub fn node<'a, 't>(self, nodes: &'a Nodes<'t>) -> &'a Op<'t> {
        &nodes[self]
    }

    pub fn ty<'t>(self, nodes: &Nodes<'t>) -> Option<Ty<'t>> {
        *self.get(&nodes.ty)
    }
}

impl Id for Node {
    fn index(&self) -> usize {
        self.0.get() as usize
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0.get(), f)
    }
}
impl Debug for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.0.get(), f)
    }
}
