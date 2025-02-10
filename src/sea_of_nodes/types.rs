use crate::datastructures::arena::DroplessArena;
pub use crate::sea_of_nodes::types::field::Field;
pub use crate::sea_of_nodes::types::r#type::*;
pub use crate::sea_of_nodes::types::ty::{Ty, TyFloat, TyInt, TyMem, TyMemPtr, TyStruct, TyTuple};
pub use crate::sea_of_nodes::types::type_float::Float;
pub use crate::sea_of_nodes::types::type_integer::Int;
pub use crate::sea_of_nodes::types::type_mem::Mem;
pub use crate::sea_of_nodes::types::type_mem_ptr::MemPtr;
pub use crate::sea_of_nodes::types::type_struct::Struct;
pub use crate::sea_of_nodes::types::type_tuple::Tuple;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};

mod field;
mod ty;
mod r#type;
mod type_float;
mod type_integer;
mod type_mem;
mod type_mem_ptr;
mod type_struct;
mod type_tuple;

/// Types are interned, so that equality checks and hashing are cheap.
///
/// Interior mutability is used for interning, so there is no need to pass
/// around mutable references.
///
/// For references, `&'t Types<'t>` should be preferred over `&'a Types<'t>`,
/// because we don't want `Ty<'t>` references that outlive their `Types`:
///
/// ```compile_fail
///     # use simple_rust::datastructures::arena ::DroplessArena;
///     # use simple_rust::sea_of_nodes::types::{Ty, Types};
///
///     fn get_t<'t>(types: &'t Types<'t>) -> Ty<'t> {types.ty_bot};
///
///     let arena = DroplessArena::new();
///     let types = Types::new(&arena);
///     let t1 = get_t(&types);
///     drop(types);
///     let types = Types::new(&arena);
///     let t2 = get_t(&types);
///     assert_ne!(t1, t2);
/// ```
/// Without `&'t` in the function parameter (or without `drop(types)`) this would
/// successfully compile and run.
pub struct Types<'a> {
    interner: Interner<'a>,
    struct_offsets: RefCell<HashMap<TyStruct<'a>, &'a [usize]>>,

    pub bot: Ty<'a>,
    pub top: Ty<'a>,
    pub ctrl: Ty<'a>,
    pub xctrl: Ty<'a>,
    pub int_top: TyInt<'a>,
    pub int_bot: TyInt<'a>,
    pub int_zero: TyInt<'a>,
    pub int_one: TyInt<'a>,
    pub int_two: TyInt<'a>,
    pub int_u1: TyInt<'a>,
    pub int_bool: TyInt<'a>,
    pub int_false: TyInt<'a>,
    pub int_true: TyInt<'a>,
    pub int_i8: TyInt<'a>,
    pub int_i16: TyInt<'a>,
    pub int_i32: TyInt<'a>,
    pub int_u8: TyInt<'a>,
    pub int_u16: TyInt<'a>,
    pub int_u32: TyInt<'a>,
    pub float_top: TyFloat<'a>,
    pub float_t32: TyFloat<'a>,
    pub float_zero: TyFloat<'a>,
    pub float_b32: TyFloat<'a>,
    pub float_bot: TyFloat<'a>,
    pub if_both: TyTuple<'a>,
    pub if_neither: TyTuple<'a>,
    pub if_true: TyTuple<'a>,
    pub if_false: TyTuple<'a>,
    pub struct_bot: TyStruct<'a>,
    pub struct_top: TyStruct<'a>,
    pub mem_bot: TyMem<'a>,
    pub mem_top: TyMem<'a>,
    pub ptr_top: TyMemPtr<'a>,
    pub ptr_bot: TyMemPtr<'a>,
    pub ptr_null: TyMemPtr<'a>,
    pub ptr_void: TyMemPtr<'a>,
}

impl<'a> Interner<'a> {
    fn get_float(&self, sz: i8, con: f64) -> TyFloat<'a> {
        self.intern(Type::Float(Float::new(sz, con)))
            .to_float()
            .unwrap()
    }

    fn get_int(&self, min: i64, max: i64) -> TyInt<'a> {
        self.intern(Type::Int(Int { min, max })).to_int().unwrap()
    }

    fn get_mem(&self, alias: u32, t: Ty<'a>) -> TyMem<'a> {
        self.intern(Type::Mem(Mem { alias, t })).to_mem().unwrap()
    }

    fn get_ptr(&self, obj: TyStruct<'a>, nil: bool) -> TyMemPtr<'a> {
        self.intern(Type::MemPtr(MemPtr { to: obj, nil }))
            .to_mem_ptr()
            .unwrap()
    }

    fn get_struct(&self, name: &'a str, fields: Option<&'a [Field<'a>]>) -> TyStruct<'a> {
        self.intern(Type::Struct(Struct { name, fields }))
            .to_struct()
            .unwrap()
    }

    fn get_tuple(&self, tuple: &'a [Ty<'a>]) -> TyTuple<'a> {
        self.intern(Type::Tuple(tuple)).to_tuple().unwrap()
    }
}

impl<'a> Types<'a> {
    pub fn new(arena: &'a DroplessArena) -> Self {
        let interner = Interner::new(arena);
        let intern = |x| interner.intern(x);

        let bot = intern(Type::Bot);
        let top = intern(Type::Top);
        let ctrl = intern(Type::Ctrl);
        let xctrl = intern(Type::XCtrl);

        let int_zero = interner.get_int(0, 0);
        let int_one = interner.get_int(1, 1);
        let int_u1 = interner.get_int(0, 1);

        let struct_top = interner.get_struct("$TOP", Some(&[]));
        let struct_bot = interner.get_struct("$BOT", Some(&[]));

        Self {
            struct_offsets: RefCell::new(HashMap::new()),
            bot,
            top,
            ctrl,
            xctrl,
            int_top: interner.get_int(i64::MAX, i64::MIN),
            int_bot: interner.get_int(i64::MIN, i64::MAX),
            int_zero,
            int_one,
            int_two: interner.get_int(2, 2),
            int_u1,
            int_bool: int_u1,
            int_false: int_zero,
            int_true: int_one,
            int_i8: interner.get_int(-128, 127),
            int_i16: interner.get_int(-32768, 32767),
            int_i32: interner.get_int(-1 << 31, (1 << 31) - 1),
            int_u8: interner.get_int(0, 255),
            int_u16: interner.get_int(0, 65535),
            int_u32: interner.get_int(0, (1 << 32) - 1),
            float_top: interner.get_float(-64, 0.0),
            float_t32: interner.get_float(-32, 0.0),
            float_zero: interner.get_float(0, 0.0),
            float_b32: interner.get_float(32, 0.0),
            float_bot: interner.get_float(64, 0.0),
            if_both: interner.get_tuple(arena.alloc([ctrl, ctrl])),
            if_neither: interner.get_tuple(arena.alloc([xctrl, xctrl])),
            if_true: interner.get_tuple(arena.alloc([ctrl, xctrl])),
            if_false: interner.get_tuple(arena.alloc([xctrl, ctrl])),
            struct_bot,
            struct_top,
            mem_top: interner.get_mem(u32::MAX, top),
            mem_bot: interner.get_mem(u32::MAX, bot),
            ptr_bot: interner.get_ptr(struct_bot, true),
            ptr_top: interner.get_ptr(struct_top, false),
            ptr_null: interner.get_ptr(struct_top, true),
            ptr_void: interner.get_ptr(struct_bot, false),
            interner,
        }
    }

    pub fn get_bool(&self, value: bool) -> TyInt<'a> {
        if value {
            self.int_true
        } else {
            self.int_false
        }
    }

    pub fn get_int(&self, value: i64) -> TyInt<'a> {
        self.interner.get_int(value, value)
    }

    pub fn make_int(&self, min: i64, max: i64) -> TyInt<'a> {
        self.interner.get_int(min, max)
    }

    pub fn get_float(&self, constant: f64) -> TyFloat<'a> {
        self.interner.get_float(0, constant)
    }

    pub fn make_float(&self, sz: i8, constant: f64) -> TyFloat<'a> {
        self.interner.get_float(sz, constant)
    }

    pub fn get_tuple_from_slice(&self, types: &[Ty<'a>]) -> TyTuple<'a> {
        self.interner
            .get_tuple(self.interner.arena.alloc_slice_copy(types))
    }

    pub fn get_tuple_from_array<const N: usize>(&self, types: [Ty<'a>; N]) -> TyTuple<'a> {
        self.interner.get_tuple(self.interner.arena.alloc(types))
    }

    pub fn get_str(&self, name: &str) -> &'a str {
        self.interner.intern_str(name)
    }

    pub fn get_slice<T: Copy>(&self, slice: &[T]) -> &'a [T] {
        self.interner.arena.alloc_slice_copy(slice)
    }

    pub fn get_struct(&self, name: &'a str, fields: &[Field<'a>]) -> TyStruct<'a> {
        let fields = Some(&*self.interner.arena.alloc_slice_copy(fields));
        self.interner
            .intern(Type::Struct(Struct { name, fields }))
            .to_struct()
            .unwrap()
    }

    pub fn make_ary(
        &self,
        len: TyInt<'a>,
        len_alias: u32,
        body: Ty<'a>,
        body_alias: u32,
    ) -> TyStruct<'a> {
        debug_assert!(!body.to_mem_ptr().is_some_and(|t| !t.data().nil));
        let name = self.get_str(&format!("[{}]", body.str()));
        let fields = self.interner.arena.alloc([
            Field {
                fname: "#",
                ty: *len,
                alias: len_alias,
                final_field: true,
            },
            Field {
                fname: "[]",
                ty: body,
                alias: body_alias,
                final_field: false,
            },
        ]);
        self.interner.get_struct(name, Some(fields))
    }
    pub fn make_struct_fref(&self, name: &'a str) -> TyStruct<'a> {
        self.interner.get_struct(name, None)
    }

    pub fn get_mem_ptr(&self, to: TyStruct<'a>, nil: bool) -> TyMemPtr<'a> {
        self.interner.get_ptr(to, nil)
    }

    pub fn get_mem(&self, alias: u32, ty: Ty<'a>) -> TyMem<'a> {
        self.interner.get_mem(alias, ty)
    }
}

struct Interner<'a> {
    arena: &'a DroplessArena,

    // If we ever want multithreading this could be a sharded hashmap like in rustc.
    // See InternedSet in rustc_middle/src/ty/context.rs
    type_to_ty: RefCell<HashMap<&'a Type<'a>, Ty<'a>>>,

    strings: RefCell<HashSet<&'a str>>,
}

impl<'t> Interner<'t> {
    fn new(arena: &'t DroplessArena) -> Self {
        Self {
            arena,
            type_to_ty: Default::default(),
            strings: Default::default(),
        }
    }

    fn intern(&self, t: Type<'t>) -> Ty<'t> {
        *self
            .type_to_ty
            .borrow_mut()
            .raw_entry_mut()
            .from_key(&t)
            .or_insert_with(|| {
                let copy = &*self.arena.alloc(t);
                let ty = Ty::new(copy);
                (copy, ty)
            })
            .1
    }

    fn intern_str(&self, s: &str) -> &'t str {
        self.strings
            .borrow_mut()
            .get_or_insert_with(s, |_| self.arena.alloc_str(s))
    }
}

#[cfg(test)]
mod tests {
    use super::Interner;
    use crate::datastructures::arena::DroplessArena;
    use crate::sea_of_nodes::types::{Int, Type};
    use std::ptr;

    #[test]
    fn test_interner() {
        let arena = DroplessArena::new();
        let interner = Interner::new(&arena);

        let ty_bot_1 = interner.intern(Type::Bot);
        let ty_bot_2 = interner.intern(Type::Bot);
        assert!(ptr::eq(ty_bot_1.data(), ty_bot_2.data()));

        let ty_42 = interner.intern(Type::Int(Int { min: 42, max: 42 }));
        let ty_2 = interner.intern(Type::Int(Int { min: 2, max: 2 }));
        let ty_42_too = interner.intern(Type::Int(Int { min: 42, max: 42 }));

        assert!(!ptr::eq(ty_42.data(), ty_2.data()));
        assert!(ptr::eq(ty_42.data(), ty_42_too.data()));

        let t1 = interner.intern(Type::Tuple(
            arena.alloc([ty_bot_1, ty_bot_2, ty_42, ty_2, ty_42_too]),
        ));
        let t2 = interner.intern(Type::Tuple(
            arena.alloc([ty_bot_2, ty_bot_1, ty_42_too, ty_2, ty_42]),
        ));
        let t3 = interner.intern(Type::Tuple(
            arena.alloc([ty_bot_1, ty_bot_2, ty_42, ty_2, ty_2]),
        ));
        assert!(ptr::eq(t1.data(), t2.data()));
        assert!(!ptr::eq(t1.data(), t3.data()));
    }

    #[test]
    fn test_strings() {
        let arena = DroplessArena::new();
        let interner = Interner::new(&arena);
        let a = interner.intern_str("foo");
        let b = interner.intern_str("bar");
        let aa = interner.intern_str("foo");
        let aaa = interner.intern_str(a);
        let bb = interner.intern_str(b);

        assert!(ptr::eq(a, aa));
        assert!(ptr::eq(a, aaa));
        assert_ne!(a, b);
        assert!(ptr::eq(b, bb));
    }
}
