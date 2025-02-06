use crate::sea_of_nodes::types::ty::TyInt;

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

    pub fn value(self) -> i64 {
        debug_assert!(self.is_constant());
        self.min()
    }

    pub fn mask(self) -> i64 {
        todo!()
    }
}
