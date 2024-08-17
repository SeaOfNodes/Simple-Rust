use crate::datastructures::arena::Arena;
use crate::sea_of_nodes::types::interner::Interner;
pub use crate::sea_of_nodes::types::r#type::*;
pub use crate::sea_of_nodes::types::ty::Ty;

mod interner;
mod ty;
mod r#type;

/// Every compilation unit has its own set of types.
/// They are interned, so that equality checks and hashing are cheap.
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
    pub ty_if_both: Ty<'a>,
    pub ty_if_neither: Ty<'a>,
    pub ty_if_true: Ty<'a>,
    pub ty_if_false: Ty<'a>,
}

impl<'a> Types<'a> {
    pub fn new(arena: &'a Arena<Type<'a>>) -> Self {
        let mut interner = Interner::new(arena);

        let ty_bot = interner.intern(Type::Bot);
        let ty_top = interner.intern(Type::Top);
        let ty_ctrl = interner.intern(Type::Ctrl);
        let ty_xctrl = interner.intern(Type::XCtrl);
        let ty_zero = interner.intern(Type::Int(Int::Constant(0)));
        let ty_one = interner.intern(Type::Int(Int::Constant(1)));
        let ty_two = interner.intern(Type::Int(Int::Constant(2)));
        let ty_int_bot = interner.intern(Type::Int(Int::Bot));

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
            ty_if_both,
            ty_if_neither,
            ty_if_true,
            ty_if_false,
        }
    }

    pub fn get_int(&mut self, value: i64) -> Ty<'a> {
        match value {
            0 => self.ty_zero,
            1 => self.ty_one,
            2 => self.ty_two,
            _ => self.interner.intern(Type::Int(Int::Constant(value))),
        }
    }

    pub fn get_tuple(&mut self, types: Vec<Ty<'a>>) -> Ty<'a> {
        self.interner.intern(Type::Tuple { types })
    }

    pub fn meet(&mut self, a: Ty<'a>, b: Ty<'a>) -> Ty<'a> {
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
