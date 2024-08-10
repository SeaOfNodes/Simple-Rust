use std::fmt;
use std::fmt::{Debug, Display};
use std::num::NonZeroU32;

use crate::datastructures::id::Id;

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct NodeId(pub(super) NonZeroU32);

impl NodeId {
    pub const DUMMY: NodeId = NodeId(NonZeroU32::MAX);

    pub fn index(self) -> usize {
        self.0.get() as usize
    }
}

impl Id for NodeId {
    fn index(&self) -> usize {
        NodeId::index(*self)
    }
}

impl Display for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0.get(), f)
    }
}
impl Debug for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.0.get(), f)
    }
}
