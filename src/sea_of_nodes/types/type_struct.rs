use crate::sea_of_nodes::types::{Field, TyStruct};

#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug)]
pub struct Struct<'t> {
    pub name: &'t str,
    pub fields: &'t [Field<'t>],
}

impl<'t> TyStruct<'t> {
    pub fn name(self) -> &'t str {
        self.data().name
    }

    pub fn fields(self) -> &'t [Field<'t>] {
        self.data().fields
    }

    pub fn find(self, fname: &str) -> Option<usize> {
        self.data().fields.iter().position(|f| f.fname == fname)
    }

    pub fn find_alias(self, alias: u32) -> Option<usize> {
        self.data().fields.iter().position(|f| f.alias == alias)
    }

    pub fn is_ary(self) -> bool {
        self.fields().len() == 2 && self.fields()[1].fname == "[]"
    }

    pub fn ary_base(self) -> i64 {
        todo!()
    }

    pub fn ary_scale(self) -> i64 {
        todo!()
    }

    pub fn offset(self, _index: usize) -> i64 {
        todo!()
    }
}
