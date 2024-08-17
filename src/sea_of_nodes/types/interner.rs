use crate::datastructures::arena::Arena;
use crate::sea_of_nodes::types::{Ty, Type};
use std::collections::HashMap;

pub struct Interner<'a> {
    pub arena: &'a Arena<Type<'a>>,
    pub type_to_ty: HashMap<&'a Type<'a>, Ty<'a>>,
}

impl<'t> Interner<'t> {
    pub fn new(arena: &'t Arena<Type<'t>>) -> Self {
        Self {
            arena,
            type_to_ty: Default::default(),
        }
    }

    pub fn intern(&mut self, t: Type<'t>) -> Ty<'t> {
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

#[cfg(test)]
mod tests {
    use crate::datastructures::arena::Arena;
    use crate::sea_of_nodes::types::interner::Interner;
    use crate::sea_of_nodes::types::{Int, Type};
    use std::ptr;

    #[test]
    fn test_interner() {
        let arena = Arena::new();
        let mut interner = Interner::new(&arena);

        let ty_bot_1 = interner.intern(Type::Bot);
        let ty_bot_2 = interner.intern(Type::Bot);
        assert!(ptr::eq(ty_bot_1.inner(), ty_bot_2.inner()));

        let ty_42 = interner.intern(Type::Int(Int::Constant(42)));
        let ty_2 = interner.intern(Type::Int(Int::Constant(2)));
        let ty_42_too = interner.intern(Type::Int(Int::Constant(42)));

        assert!(!ptr::eq(ty_42.inner(), ty_2.inner()));
        assert!(ptr::eq(ty_42.inner(), ty_42_too.inner()));

        let t1 = interner.intern(Type::Tuple {
            types: vec![ty_bot_1, ty_bot_2, ty_42, ty_2, ty_42_too],
        });
        let t2 = interner.intern(Type::Tuple {
            types: vec![ty_bot_2, ty_bot_1, ty_42_too, ty_2, ty_42],
        });
        let t3 = interner.intern(Type::Tuple {
            types: vec![ty_bot_1, ty_bot_2, ty_42, ty_2, ty_2],
        });
        assert!(ptr::eq(t1.inner(), t2.inner()));
        assert!(!ptr::eq(t1.inner(), t3.inner()));
    }
}
