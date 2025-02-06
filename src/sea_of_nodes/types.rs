use crate::datastructures::arena::DroplessArena;
pub use crate::sea_of_nodes::types::field::Field;
use crate::sea_of_nodes::types::interner::Interner;
pub use crate::sea_of_nodes::types::r#type::*;
pub use crate::sea_of_nodes::types::ty::{Ty, TyMemPtr, TyStruct};
use crate::sea_of_nodes::types::ty::{TyFloat, TyInt, TyMem, TyTuple};

mod field;
mod interner;
mod ty;
mod r#type;

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

    fn get_struct(&self, name: &'a str, fields: &'a [Field<'a>]) -> TyStruct<'a> {
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

        let struct_top = interner.get_struct("$TOP", &[]);
        let struct_bot = interner.get_struct("$BOT", &[]);

        Self {
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

    pub fn get_int(&self, value: i64) -> TyInt<'a> {
        self.interner.get_int(value, value)
    }

    pub fn get_float(&self, constant: f64) -> TyFloat<'a> {
        self.interner.get_float(0, constant)
    }

    pub fn get_tuple_from_slice(&self, types: &[Ty<'a>]) -> Ty<'a> {
        let types: &'a [Ty<'a>] = self.interner.arena.alloc_slice_copy(types);
        self.interner.intern(Type::Tuple(types))
    }

    pub fn get_tuple_from_array<const N: usize>(&self, types: [Ty<'a>; N]) -> Ty<'a> {
        let types: &'a [Ty<'a>] = self.interner.arena.alloc(types);
        self.interner.intern(Type::Tuple(types))
    }

    pub fn get_str(&self, name: &str) -> &'a str {
        self.interner.intern_str(name)
    }

    pub fn get_struct(&self, name: &'a str, fields: &[Field<'a>]) -> TyStruct<'a> {
        let fields = self.interner.arena.alloc_slice_copy(fields);
        self.interner
            .intern(Type::Struct(Struct::Struct { name, fields }))
            .to_struct()
            .unwrap()
    }

    pub fn get_mem_ptr(&self, to: TyStruct<'a>, nil: bool) -> TyMemPtr<'a> {
        self.interner.get_ptr(to, nil)
    }

    pub fn get_mem(&self, alias: u32, ty: Ty<'a>) -> TyMem<'a> {
        self.interner.get_mem(alias, ty)
    }

    pub fn meet(&self, a: Ty<'a>, b: Ty<'a>) -> Ty<'a> {
        match (*a, *b) {
            (_, _) if a == b => a,

            // Bot wins, Top looses
            (Type::Bot, _) | (_, Type::Top) => a,
            (Type::Top, _) | (_, Type::Bot) => b,

            // Ctrl sub-lattice: Ctrl meets ~Ctrl is Ctrl
            (Type::Ctrl, Type::XCtrl) => a,
            (Type::XCtrl, Type::Ctrl) => b,

            // Int sub-lattice
            (Type::Int(ia), Type::Int(ib)) => match (ia, ib) {
                (Int::Bot, _) | (_, Int::Top) => a,
                (_, Int::Bot) | (Int::Top, _) => b,
                (Int::Constant(ca), Int::Constant(cb)) if ca == cb => a,
                _ => self.int_bot,
            },

            // Tuple sub-lattice
            (Type::Tuple(t1), Type::Tuple(t2)) => {
                assert_eq!(t1.len(), t2.len(), "{a} meet {b} not implemented");
                self.get_tuple_from_slice(
                    &t1.iter()
                        .zip(t2.iter())
                        .map(|(x, y)| self.meet(*x, *y))
                        .collect::<Vec<_>>(),
                )
            }

            // Struct sub-lattice
            (Type::Struct(sa), Type::Struct(sb)) => match (sa, sb) {
                (Struct::Bot, _) | (_, Struct::Top) => a,
                (_, Struct::Bot) | (Struct::Top, _) => b,
                (
                    Struct::Struct {
                        name: na,
                        fields: fas,
                    },
                    Struct::Struct {
                        name: nb,
                        fields: fbs,
                    },
                ) if na == nb => {
                    assert_eq!(fas.len(), fbs.len(), "{a} meet {b} can't happen because struct name must be uniuqe in a compilation unit");
                    let fields = fas.iter().zip(fbs).map(|(fa, fb)| {
                        debug_assert_eq!(fa.0, fb.0, "{a} meet {b} can't happen because struct name must be uniuqe in a compilation unit");
                        (fa.0, self.meet(fa.1, fb.1))
                    }).collect::<Vec<_>>();
                    *self.get_struct(na, &fields)
                }
                _ => *self.struct_bot, // It's a struct; that's about all we know
            },

            // Pointer sub-lattice
            (Type::MemPtr(pa), Type::MemPtr(pb)) => *self.get_mem_ptr(
                self.meet(*pa.to, *pb.to).to_struct().unwrap(),
                pa.nil | pb.nil,
            ),

            // Memory sub-lattice
            (Type::Mem(ma), Type::Mem(mb)) => match (ma, mb) {
                (Mem::Bot, _) | (_, Mem::Top) => a,
                (_, Mem::Bot) | (Mem::Top, _) => b,
                _ => self.mem_bot,
            },

            // different sub-lattices meet at bottom
            _ => self.bot,
        }
    }

    /// True if this "isa" t; e.g. 17 isa TypeInteger.BOT
    pub fn isa(&self, this: Ty<'a>, that: Ty<'a>) -> bool {
        self.meet(this, that) == that
    }

    /// Our lattice is defined with a MEET and a DUAL.
    /// JOIN is dual of meet of both duals.
    pub fn join(&self, this: Ty<'a>, that: Ty<'a>) -> Ty<'a> {
        if this == that {
            this
        } else {
            self.dual(self.meet(self.dual(this), self.dual(that)))
        }
    }

    pub fn dual(&self, ty: Ty<'a>) -> Ty<'a> {
        match *ty {
            Type::Bot => self.top,
            Type::Top => self.bot,
            Type::Ctrl => self.xctrl,
            Type::XCtrl => self.ctrl,
            Type::Int(i) => match i {
                Int::Bot => self.int_top,
                Int::Top => self.int_bot,
                Int::Constant(_) => ty, // self dual
            },
            Type::Tuple(types) => {
                self.get_tuple_from_slice(&types.iter().map(|t| self.dual(*t)).collect::<Vec<_>>())
            }
            Type::Struct(s) => match s {
                Struct::Bot => *self.struct_top,
                Struct::Top => *self.struct_bot,
                Struct::Struct { name, fields } => {
                    let fields = fields
                        .iter()
                        .map(|&(name, ty)| (name, self.dual(ty)))
                        .collect::<Vec<_>>();
                    *self.get_struct(name, &fields)
                }
            },
            Type::MemPtr(p) => {
                let to = self.dual(*p.to).to_struct().unwrap();
                *self.get_mem_ptr(to, !p.nil)
            }
            Type::Mem(m) => match m {
                Mem::Bot => self.mem_top,
                Mem::Top => self.mem_bot,
                Mem::Alias(_) => ty, // self dual
            },
        }
    }

    /// compute greatest lower bound in the lattice
    pub fn glb(&self, ty: Ty<'a>) -> Ty<'a> {
        match *ty {
            Type::Bot | Type::Top => self.bot,
            Type::Ctrl => self.xctrl, // why?
            Type::XCtrl => self.bot,  // why?
            Type::Int(_) => self.int_bot,
            Type::Tuple(types) => {
                let types = types.iter().map(|&ty| self.glb(ty)).collect::<Vec<_>>();
                self.get_tuple_from_slice(&types)
            }
            Type::Struct(s) => match s {
                Struct::Bot => ty,
                Struct::Top => ty, // no fields to lower?
                Struct::Struct { name, fields } => {
                    let fields = fields
                        .iter()
                        .map(|&(name, ty)| (name, self.glb(ty)))
                        .collect::<Vec<_>>();
                    *self.get_struct(name, &fields)
                }
            },
            Type::MemPtr(MemPtr { to, .. }) => {
                *self.get_mem_ptr(self.glb(*to).to_struct().unwrap(), true)
            }
            Type::Mem(_) => self.mem_bot,
        }
    }

    pub fn make_init(&self, t: Ty<'a>) -> Option<Ty<'a>> {
        match *t {
            Type::Int(_) => Some(self.int_zero),
            Type::MemPtr(_) => Some(*self.ptr_null),
            _ => None,
        }
    }
}
