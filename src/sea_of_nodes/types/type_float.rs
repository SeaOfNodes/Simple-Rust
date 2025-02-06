use crate::sea_of_nodes::types::ty::TyFloat;
use crate::sea_of_nodes::types::Types;

#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug)]
pub struct Float {
    pub sz: i8,
    /// f64 bits as integer for easier eq/hashing
    con: u64,
}

impl Float {
    pub fn new(sz: i8, con: f64) -> Self {
        Self {
            sz,
            con: con.to_bits(),
        }
    }

    pub fn con(&self) -> f64 {
        f64::from_bits(self.con)
    }
}

impl<'t> TyFloat<'t> {
    pub fn sz(self) -> i8 {
        self.data().sz
    }
    pub fn is_f32(&self) -> bool {
        let v = self.data().con();
        v as f32 as f64 == v
    }

    pub fn meet(self, that: TyFloat<'t>, tys: &Types<'t>) -> TyFloat<'t> {
        // Larger size in i1, smaller in i0
        let (i0, i1) = if self.sz() < that.sz() {
            (self, that)
        } else {
            (that, self)
        };

        if i1.sz() == 64 {
            tys.float_bot
        } else if i0.sz() == -64 {
            i1
        } else if i1.sz() == 32 {
            if i0.sz() == 0 && !i0.is_f32() {
                tys.float_bot
            } else {
                tys.float_b32
            }
        } else if i1.sz() != 0 {
            i1
        } else if i0.sz() == -32 {
            // i1 is a constant
            if i1.is_f32() {
                i1
            } else {
                tys.float_bot
            }
        } else {
            // Since both are constants, and are never equals (contract) unequals
            // constants fall to bottom
            if i0.is_f32() && i1.is_f32() {
                tys.float_b32
            } else {
                tys.float_bot
            }
        }
    }
}
