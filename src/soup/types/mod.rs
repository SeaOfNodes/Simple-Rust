use std::collections::HashMap;
use std::sync::Arc;

use crate::arena::Arena;
use crate::modules::ParsedModule;
pub use crate::soup::types::r#type::*;
pub use crate::soup::types::ty::Ty;

mod ty;
mod r#type;

/// Every compilation unit has its own set of types.
/// They are interned, so that equality checks and hashing are cheap.
/// This requires every compilation unit to re-parse all imported types.
pub struct Types<'a> {
    interner: Interner<'a>,

    pub ty_bot: Ty<'a>,
    pub ty_top: Ty<'a>,
    pub ty_ctrl: Ty<'a>,
    pub ty_zero: Ty<'a>,
    pub ty_one: Ty<'a>,
    pub ty_two: Ty<'a>,
    pub ty_int_bot: Ty<'a>,
    pub ty_if: Ty<'a>,
}

struct Interner<'a> {
    arena: &'a Arena<Type<'a>>,
    type_to_ty: HashMap<&'a Type<'a>, Ty<'a>>,
}

impl<'t> Interner<'t> {
    fn intern(&mut self, t: Type<'t>) -> Ty<'t> {
        *self
            .type_to_ty
            .raw_entry_mut()
            .from_key(&t)
            .or_insert_with(|| {
                let copy = &*self.arena.alloc(t);
                let ty = Ty::new(copy);
                (copy, ty)
            })
            .1
    }
}

impl<'a> Types<'a> {
    pub fn new(arena: &'a Arena<Type<'a>>) -> Self {
        let mut interner = Interner {
            arena,
            type_to_ty: Default::default(),
        };

        let ty_bot = interner.intern(Type::Bot);
        let ty_top = interner.intern(Type::Top);
        let ty_ctrl = interner.intern(Type::Ctrl);
        let ty_zero = interner.intern(Type::Int(Int::Constant(0)));
        let ty_one = interner.intern(Type::Int(Int::Constant(1)));
        let ty_two = interner.intern(Type::Int(Int::Constant(2)));
        let ty_int_bot = interner.intern(Type::Int(Int::Bot));
        let ty_if = interner.intern(Type::Tuple {
            types: vec![ty_ctrl, ty_ctrl],
        });

        Self {
            interner,
            ty_bot,
            ty_top,
            ty_ctrl,
            ty_zero,
            ty_one,
            ty_two,
            ty_int_bot,
            ty_if,
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

    pub fn get_module(&mut self, module: Arc<ParsedModule>) -> Ty<'a> {
        self.interner.intern(Type::Module(module))
    }

    pub fn meet(&mut self, a: Ty<'a>, b: Ty<'a>) -> Ty<'a> {
        match (&*a, &*b) {
            (_, _) if a == b => a,
            (Type::Int(ia), Type::Int(ib)) => {
                match (ia, ib) {
                    // Bot wins
                    (Int::Bot, _) => a,
                    (_, Int::Bot) => b,
                    // Top looses
                    (Int::Top, _) => b,
                    (_, Int::Top) => a,
                    (Int::Constant(ca), Int::Constant(cb)) => {
                        if ca == cb {
                            a
                        } else {
                            self.ty_int_bot
                        }
                    }
                }
            }
            _ => todo!("meet {a} {b}"),
        }
    }
}
