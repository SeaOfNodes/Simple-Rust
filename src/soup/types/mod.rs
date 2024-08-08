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
    pub ty_zero: Ty<'a>,
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
        let ty_zero = interner.intern(Type::Int {
            value: 0,
            constant: true,
        });

        Self {
            interner,
            ty_bot,
            ty_top,
            ty_zero,
        }
    }

    pub fn get_int(&mut self, value: i64) -> Ty<'a> {
        match value {
            0 => self.ty_zero,
            _ => self.interner.intern(Type::Int {
                value,
                constant: true,
            }),
        }
    }

    pub fn get_module(&mut self, module: Arc<ParsedModule>) -> Ty<'a> {
        self.interner.intern(Type::Module(module))
    }
}
