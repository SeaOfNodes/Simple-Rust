use crate::datastructures::arena::Arena;
use crate::sea_of_nodes::types::interner::Interner;
pub use crate::sea_of_nodes::types::r#type::*;
pub use crate::sea_of_nodes::types::ty::Ty;

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
///     # use simple_rust::datastructures::arena::Arena;
///     # use simple_rust::sea_of_nodes::types::{Ty, Types};
///
///     fn get_t<'t>(types: &'t Types<'t>) -> Ty<'t> {types.ty_bot};
///
///     let arena = Arena::new();
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

    pub ty_bot: Ty<'a>,
    pub ty_top: Ty<'a>,
    pub ty_ctrl: Ty<'a>,
    pub ty_xctrl: Ty<'a>,
    pub ty_zero: Ty<'a>,
    pub ty_one: Ty<'a>,
    pub ty_two: Ty<'a>,
    pub ty_int_bot: Ty<'a>,
    pub ty_int_top: Ty<'a>,
    pub ty_if_both: Ty<'a>,
    pub ty_if_neither: Ty<'a>,
    pub ty_if_true: Ty<'a>,
    pub ty_if_false: Ty<'a>,
}

impl<'a> Types<'a> {
    pub fn new(arena: &'a Arena<Type<'a>>) -> Self {
        let interner = Interner::new(arena);

        let ty_bot = interner.intern(Type::Bot);
        let ty_top = interner.intern(Type::Top);
        let ty_ctrl = interner.intern(Type::Ctrl);
        let ty_xctrl = interner.intern(Type::XCtrl);
        let ty_zero = interner.intern(Type::Int(Int::Constant(0)));
        let ty_one = interner.intern(Type::Int(Int::Constant(1)));
        let ty_two = interner.intern(Type::Int(Int::Constant(2)));
        let ty_int_bot = interner.intern(Type::Int(Int::Bot));
        let ty_int_top = interner.intern(Type::Int(Int::Top));

        let ty_if_both = interner.intern(Type::Tuple {
            types: vec![ty_ctrl, ty_ctrl],
        });
        let ty_if_neither = interner.intern(Type::Tuple {
            types: vec![ty_xctrl, ty_xctrl],
        });
        let ty_if_true = interner.intern(Type::Tuple {
            types: vec![ty_ctrl, ty_xctrl],
        });
        let ty_if_false = interner.intern(Type::Tuple {
            types: vec![ty_xctrl, ty_ctrl],
        });

        Self {
            interner,
            ty_bot,
            ty_top,
            ty_ctrl,
            ty_xctrl,
            ty_zero,
            ty_one,
            ty_two,
            ty_int_bot,
            ty_int_top,
            ty_if_both,
            ty_if_neither,
            ty_if_true,
            ty_if_false,
        }
    }

    pub fn get_int(&self, value: i64) -> Ty<'a> {
        match value {
            0 => self.ty_zero,
            1 => self.ty_one,
            2 => self.ty_two,
            _ => self.interner.intern(Type::Int(Int::Constant(value))),
        }
    }

    pub fn get_tuple(&self, types: Vec<Ty<'a>>) -> Ty<'a> {
        self.interner.intern(Type::Tuple { types })
    }

    pub fn meet(&self, a: Ty<'a>, b: Ty<'a>) -> Ty<'a> {
        match (&*a, &*b) {
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
                _ => self.ty_int_bot,
            },

            // Tuple sub-lattice
            (Type::Tuple { .. }, Type::Tuple { .. }) => {
                todo!("meet on tuples is not implemented yet {a} {b}")
            }

            // different sub-lattices meet at bottom
            _ => self.ty_bot,
        }
    }
}
