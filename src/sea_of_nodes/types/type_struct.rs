use crate::sea_of_nodes::types::{Field, TyStruct, Types};

#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug)]
pub struct Struct<'t> {
    pub name: &'t str,
    /// None means that this is a forward reference
    pub fields: Option<&'t [Field<'t>]>,
}

impl<'t> TyStruct<'t> {
    pub fn name(self) -> &'t str {
        self.data().name
    }

    pub fn fields(self) -> &'t [Field<'t>] {
        self.data().fields.unwrap()
    }

    pub fn find(self, fname: &str) -> Option<usize> {
        self.fields().iter().position(|f| f.fname == fname)
    }

    pub fn find_alias(self, alias: u32) -> Option<usize> {
        self.fields().iter().position(|f| f.alias == alias)
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

    pub fn meet(self, that: TyStruct<'t>, tys: &Types<'t>) -> TyStruct<'t> {
        if self == tys.struct_bot || that == tys.struct_top {
            return self;
        }
        if that == tys.struct_bot || self == tys.struct_top {
            return that;
        }
        // Within the same compilation unit, struct names are unique.  If the
        // names differ, its different structs.  Across many compilation units,
        // structs with the same name but different field layouts can be
        // interned... which begs the question:
        // "What is the meet of structs from two different compilation units?"
        // And the answer is: "don't ask".
        if self.name() != that.name() {
            return tys.struct_bot; // It's a struct; that's about all we know
        }
        let Some(s_fs) = self.data().fields else {
            return that;
        };
        let Some(t_fs) = that.data().fields else {
            return self;
        };
        if s_fs.len() != t_fs.len() {
            return tys.struct_bot;
        }

        // Just do field meets
        let mut flds = Vec::with_capacity(s_fs.len());
        for (f0, f1) in s_fs.iter().zip(t_fs) {
            if f0.fname != f1.fname || f0.alias != f1.alias {
                return tys.struct_bot;
            }
            flds.push(Field {
                fname: f0.fname,
                ty: f0.ty.meet(f1.ty, tys),
                alias: f0.alias,
                final_field: f0.final_field | f1.final_field,
            })
        }
        tys.get_struct(self.name(), &flds)
    }

    pub fn dual(self, tys: &Types<'t>) -> TyStruct<'t> {
        if self == tys.struct_top {
            tys.struct_bot
        } else if self == tys.struct_bot {
            tys.struct_top
        } else if let Some(fields) = self.data().fields {
            let fields = fields
                .iter()
                .map(|f| Field {
                    fname: f.fname,
                    ty: f.ty.dual(tys),
                    alias: f.alias,
                    final_field: !f.final_field,
                })
                .collect::<Vec<_>>();
            tys.get_struct(self.name(), &fields)
        } else {
            self
        }
    }

    pub fn glb(self, tys: &Types<'t>) -> TyStruct<'t> {
        if let Some(fields) = self.data().fields {
            let new_fields = fields
                .iter()
                .map(|f| Field {
                    ty: f.ty.glb(tys),
                    ..*f
                })
                .collect::<Vec<_>>();
            tys.get_struct(self.name(), &new_fields)
        } else {
            self
        }
    }
}
