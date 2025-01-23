use crate::datastructures::arena::DroplessArena;
use crate::sea_of_nodes::types::interner::Interner;
pub use crate::sea_of_nodes::types::r#type::*;
pub use crate::sea_of_nodes::types::ty::{Ty, TyMemPtr, TyStruct};

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
    pub int_zero: Ty<'a>,
    pub int_one: Ty<'a>,
    pub int_two: Ty<'a>,
    pub int_bot: Ty<'a>,
    pub int_top: Ty<'a>,
    pub if_both: Ty<'a>,
    pub if_neither: Ty<'a>,
    pub if_true: Ty<'a>,
    pub if_false: Ty<'a>,
    pub struct_bot: TyStruct<'a>,
    pub struct_top: TyStruct<'a>,
    pub memory_bot: Ty<'a>,
    pub memory_top: Ty<'a>,
    pub pointer_top: TyMemPtr<'a>,
    pub pointer_bot: TyMemPtr<'a>,
    pub pointer_null: TyMemPtr<'a>,
    pub pointer_void: TyMemPtr<'a>,
}

impl<'a> Types<'a> {
    pub fn new(arena: &'a DroplessArena) -> Self {
        let interner = Interner::new(arena);
        let intern = |x| interner.intern(x);

        let ctrl = intern(Type::Ctrl);
        let xctrl = intern(Type::XCtrl);

        let struct_bot = intern(Type::Struct(Struct::Bot)).to_struct().unwrap();
        let struct_top = intern(Type::Struct(Struct::Top)).to_struct().unwrap();

        Self {
            bot: intern(Type::Bot),
            top: intern(Type::Top),
            ctrl,
            xctrl,
            int_zero: intern(Type::Int(Int::Constant(0))),
            int_one: intern(Type::Int(Int::Constant(1))),
            int_two: intern(Type::Int(Int::Constant(2))),
            int_bot: intern(Type::Int(Int::Bot)),
            int_top: intern(Type::Int(Int::Top)),
            if_both: intern(Type::Tuple {
                types: arena.alloc([ctrl, ctrl]),
            }),
            if_neither: intern(Type::Tuple {
                types: arena.alloc([xctrl, xctrl]),
            }),
            if_true: intern(Type::Tuple {
                types: arena.alloc([ctrl, xctrl]),
            }),
            if_false: intern(Type::Tuple {
                types: arena.alloc([xctrl, ctrl]),
            }),
            struct_bot,
            struct_top,
            memory_bot: intern(Type::Mem(Mem::Bot)),
            memory_top: intern(Type::Mem(Mem::Top)),
            pointer_bot: intern(Type::MemPtr(MemPtr {
                to: struct_bot,
                nil: true,
            }))
            .to_mem_ptr()
            .unwrap(),
            pointer_top: intern(Type::MemPtr(MemPtr {
                to: struct_top,
                nil: false,
            }))
            .to_mem_ptr()
            .unwrap(),
            pointer_null: intern(Type::MemPtr(MemPtr {
                to: struct_top,
                nil: true,
            }))
            .to_mem_ptr()
            .unwrap(),
            pointer_void: intern(Type::MemPtr(MemPtr {
                to: struct_bot,
                nil: false,
            }))
            .to_mem_ptr()
            .unwrap(),
            interner,
        }
    }

    pub fn get_int(&self, value: i64) -> Ty<'a> {
        match value {
            0 => self.int_zero,
            1 => self.int_one,
            2 => self.int_two,
            _ => self.interner.intern(Type::Int(Int::Constant(value))),
        }
    }

    pub fn get_tuple_from_slice(&self, types: &[Ty<'a>]) -> Ty<'a> {
        let types: &'a [Ty<'a>] = self.interner.arena.alloc_slice_copy(types);
        self.interner.intern(Type::Tuple { types })
    }

    pub fn get_tuple_from_array<const N: usize>(&self, types: [Ty<'a>; N]) -> Ty<'a> {
        let types: &'a [Ty<'a>] = self.interner.arena.alloc(types);
        self.interner.intern(Type::Tuple { types })
    }

    pub fn get_str(&self, name: &str) -> &'a str {
        self.interner.intern_str(name)
    }

    pub fn get_struct(&self, name: &'a str, fields: &[(&'a str, Ty<'a>)]) -> TyStruct<'a> {
        let fields = self.interner.arena.alloc_slice_copy(fields);
        self.interner
            .intern(Type::Struct(Struct::Struct { name, fields }))
            .to_struct()
            .unwrap()
    }

    pub fn get_mem_ptr(&self, to: TyStruct<'a>, nil: bool) -> TyMemPtr<'a> {
        self.interner
            .intern(Type::MemPtr(MemPtr { to, nil }))
            .to_mem_ptr()
            .unwrap()
    }

    pub fn get_mem(&self, alias: u32) -> Ty<'a> {
        self.interner.intern(Type::Mem(Mem::Alias(alias)))
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
            (Type::Tuple { types: t1 }, Type::Tuple { types: t2 }) => {
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
                _ => self.memory_bot,
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
            Type::Tuple { types } => {
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
                Mem::Bot => self.memory_top,
                Mem::Top => self.memory_bot,
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
            Type::Tuple { types } => {
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
            Type::Mem(_) => self.memory_bot,
        }
    }

    pub fn make_init(&self, t: Ty<'a>) -> Option<Ty<'a>> {
        match *t {
            Type::Int(_) => Some(self.int_zero),
            Type::MemPtr(_) => Some(*self.pointer_null),
            _ => None,
        }
    }
}
