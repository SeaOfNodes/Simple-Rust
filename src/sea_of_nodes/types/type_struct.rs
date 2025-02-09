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

    pub fn field(self, fname: &str) -> Option<&Field<'t>> {
        let index = self.find(fname)?;
        Some(&self.fields()[index])
    }

    pub fn find_alias(self, alias: u32) -> Option<usize> {
        self.fields().iter().position(|f| f.alias == alias)
    }

    pub fn is_ary(self) -> bool {
        self.fields().len() == 2 && self.fields()[1].fname == "[]"
    }

    pub fn ary_base(self, tys: &Types<'t>) -> i64 {
        debug_assert!(self.is_ary());
        self.offset(1, tys)
    }

    pub fn ary_scale(self, tys: &Types<'t>) -> i64 {
        debug_assert!(self.is_ary());
        self.fields()[1].ty.log_size(tys) as i64
    }

    pub fn offset(self, index: usize, tys: &Types<'t>) -> i64 {
        tys.struct_offsets
            .borrow_mut()
            .entry(self)
            .or_insert_with(|| tys.get_slice(&self.offsets(tys)))[index] as i64
    }

    /// Field byte offsets
    fn offsets(self, tys: &Types<'t>) -> Vec<usize> {
        // Compute a layout for a collection of fields
        let fields = self.fields(); // No forward refs

        // Compute a layout
        let mut cnts = [0, 0, 0, 0]; // Count of fields at log field size
        for f in fields {
            cnts[f.ty.log_size(tys)] += 1; // Log size is 0(byte), 1(i16/u16), 2(i32/f32), 3(i64/dbl)
        }

        // Base common struct fields go here, e.g. Mark/Klass
        let mut off = 0;

        // Compute offsets to the start of each power-of-2 aligned fields.
        let mut offs = [0, 0, 0, 0];
        for i in (0..4).rev() {
            offs[i] = off;
            off += cnts[i] << i;
        }
        // Assign offsets to all fields.
        // Really a hidden radix sort.
        let mut result = Vec::with_capacity(fields.len() + 1);
        for f in fields {
            let log = f.ty.log_size(tys);
            result.push(offs[log]); // Field offset
            offs[log] += 1 << log; // Next field offset at same alignment
            cnts[log] -= 1; // Count down, should be all zero at end
        }
        result.push((off + 7) & !7); // Round out to max alignment
        result
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

    pub fn lub(self, tys: &Types<'t>) -> TyStruct<'t> {
        if let Some(fields) = self.data().fields {
            let new_fields = fields
                .iter()
                .map(|f| Field {
                    ty: f.ty.lub(tys),
                    ..*f
                })
                .collect::<Vec<_>>();
            tys.get_struct(self.name(), &new_fields)
        } else {
            self
        }
    }
}
