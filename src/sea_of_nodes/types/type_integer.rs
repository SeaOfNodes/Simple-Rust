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

    pub fn value(self) -> Option<i64> {
        let d = self.data();
        (d.min == d.max).then_some(d.min)
    }

    pub fn mask(self) -> i64 {
        todo!()
    }
}
