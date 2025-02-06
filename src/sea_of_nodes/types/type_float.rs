use crate::sea_of_nodes::types::ty::TyFloat;

#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug)]
pub struct Float {
    sz: i8,
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
    pub fn is_f32(&self) -> bool {
        let v = self.data().con();
        v as f32 as f64 == v
    }
}
