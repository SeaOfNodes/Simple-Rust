use crate::sea_of_nodes::types::ty::TyInt;
use crate::sea_of_nodes::types::Types;

#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug)]
pub struct Int {
    pub min: i64,
    pub max: i64,
}

impl<'t> TyInt<'t> {
    pub fn min(self) -> i64 {
        self.data().min
    }

    pub fn max(self) -> i64 {
        self.data().max
    }

    pub fn value(self) -> Option<i64> {
        let d = self.data();
        (d.min == d.max).then_some(d.min)
    }

    /// AND-mask of forced zeros.  e.g. unsigned types will return their mask;
    /// u8 will return 0xFF.  But also a range of 16-18 (0x10-0x12) will return
    /// 0x13 - no value in the range {16,17,18} will allow bit 0x04 to be set.
    pub fn mask(self, tys: &Types<'t>) -> i64 {
        if self.is_high(tys) {
            return 0;
        }
        if let Some(v) = self.value() {
            return v;
        }
        //if( _min<0 ) return -1L;
        //if( _max==Long.MAX_VALUE ) return -1L;
        // Those bit positions which differ min to max
        let x = self.min() ^ self.max();
        // Highest '1' bit in the differ set.  Since the range is from min to
        // max, all values below here are possible.
        let highest_one_bit = |i: i64| i & (u64::MIN >> i.leading_zeros()) as i64;
        let ff1 = highest_one_bit(x);
        // Make a all-1's mask from ff1, and set over the same bits (either min
        // or max is ok).
        self.min() | (ff1 - 1) | ff1
    }
}
