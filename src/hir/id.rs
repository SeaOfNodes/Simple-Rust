use std::fmt::{Debug, Display, Formatter};
use std::num::NonZeroU32;

/// Identifies an instruction
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id {
    /// 32 bit ought to be enough for everyone
    /// We store it as NonZero to allow niche optimizations
    /// because rustc_layout_scalar_valid_range_end is unstable.
    index: NonZeroU32,
}

impl From<usize> for Id {
    fn from(value: usize) -> Self {
        assert!(value < u32::MAX as usize);
        return Id {
            index: NonZeroU32::new((value + 1) as u32).unwrap(),
        };
    }
}

impl Id {
    pub fn index(self) -> usize {
        self.index.get() as usize - 1
    }
}

impl Debug for Id {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.index(), f)
    }
}

impl Display for Id {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.index(), f)
    }
}
