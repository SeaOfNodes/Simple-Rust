use std::num::NonZeroU32;
use std::ops::Index;

use crate::hir::types::Ty;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct StackSlotIndex(NonZeroU32);

#[derive(Debug)]
pub struct StackSlots<'t> {
    pub entries: Vec<StackSlot<'t>>,
}

impl<'t> StackSlots<'t> {
    pub fn new() -> Self {
        Self { entries: vec![] }
    }

    pub fn add(&mut self, ty: Ty<'t>) -> StackSlotIndex {
        let index32 = u32::try_from(self.entries.len() + 1).unwrap();
        let index = StackSlotIndex(NonZeroU32::new(index32).unwrap());
        self.entries.push(StackSlot {
            ty,
            disp: 0,
            size: 0,
        });
        index
    }
}

#[derive(Debug)]
pub struct StackSlot<'a> {
    pub ty: Ty<'a>,
    pub disp: i32,
    pub size: i32,
}

impl<'a> Index<StackSlotIndex> for StackSlots<'a> {
    type Output = StackSlot<'a>;

    fn index(&self, index: StackSlotIndex) -> &Self::Output {
        &self.entries[index.0.get() as usize - 1]
    }
}
