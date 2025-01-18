use crate::datastructures::arena::DroplessArena;
use crate::sea_of_nodes::types::{MemPtr, Ty, Type, Types};
use std::ptr;

// Test basic properties and GLB
#[test]
fn test_type_ad_hoc() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);

    let s1 = types.get_struct("s1", &[("a", types.int_bot), ("b", types.int_bot)]);
    let s2 = types.get_struct("s2", &[("a", types.int_bot), ("b", types.int_bot)]);
    assert_eq!(s1, types.glb(s1));
    assert_ne!(s1, types.dual(s1));
    assert_eq!(s1, types.glb(types.dual(s1)));

    let m1 = types.get_mem(1);
    let m2 = types.get_mem(2);
    let m3 = types.get_mem(3);
    let m4 = types.get_mem(4);

    assert_ne!(m1, m2);
    assert_ne!(m2, m3);
    assert_ne!(m3, m4);

    assert_eq!(types.struct_bot, types.meet(s1, s2));
    assert_eq!(types.memory_bot, types.meet(m1, m2));
    assert_eq!(types.memory_bot, types.meet(m2, m3));
    assert_eq!(types.memory_bot, types.meet(m3, m4));

    assert_eq!(types.memory_bot, types.glb(m1));
    assert_eq!(m1, types.dual(m1));
    assert_eq!(types.memory_top, types.dual(types.glb(m1)));

    let ptr1 = types.get_pointer(s1, false);
    assert!(matches!(*ptr1, Type::Pointer(MemPtr {to, nil: false}) if to == s1));
    let ptr2 = types.get_pointer(s2, false);
    assert!(matches!(*ptr2, Type::Pointer(MemPtr {to, nil: false}) if to == s2));

    let ptr1nil = types.get_pointer(s1, true);
    assert!(matches!(*ptr1nil, Type::Pointer(MemPtr {to, nil: true}) if to == s1));
    let ptr2nil = types.get_pointer(s2, true);
    assert!(matches!(*ptr2nil, Type::Pointer(MemPtr {to, nil: true}) if to == s2));

    assert_ne!(ptr1, ptr2);
    assert_ne!(ptr1, types.glb(ptr1));
    assert_eq!(ptr1nil, types.glb(ptr1));

    assert_eq!(ptr1, types.dual(types.dual(ptr1)));
    assert_eq!(types.glb(ptr1), types.glb(types.dual(ptr1)));
    assert_eq!(types.pointer_bot, types.meet(ptr1, ptr2nil));
    assert_eq!(types.glb(ptr1), types.meet(ptr1, types.pointer_null));

    let top = types.pointer_top;
    let bot = types.pointer_bot;
    let ptr = types.pointer_void;
    let null = types.pointer_null;

    assert_eq!(bot, types.meet(ptr, null));
    assert_eq!(ptr, types.meet(ptr1, ptr2));
    assert_eq!(top, types.join(null, ptr1));
    assert_eq!(top, types.join(ptr, null));

    let _ptr1_dual = types.dual(ptr1);
    let _nullable_ptr1_dual = types.dual(ptr1nil);
}

// Test theoretical properties.
// This is a symmetric complete bounded (ranked) lattice.
// Also the meet is commutative and associative.
// The lattice has a dual (symmetric), and join is ~(~x meet ~y).
// See https://en.wikipedia.org/wiki/Lattice_(order).
#[test]
fn test_lattice_theory() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);

    let ts = types.gather();

    // Confirm commutative & complete
    for &t0 in &ts {
        for &t1 in &ts {
            types.check_commute(t0, t1);
            types.check_symmetric(t0, t1);
        }
    }

    // Confirm associative
    for &t0 in &ts {
        for &t1 in &ts {
            for &t2 in &ts {
                types.assoc(t0, t1, t2);
            }
        }
    }

    // Confirm symmetry.  If A isa B, then A.join(C) isa B.join(C)
    for &t0 in &ts {
        for &t1 in &ts {
            if types.isa(t0, t1) {
                for &t2 in &ts {
                    let t02 = types.join(t0, t2);
                    let t12 = types.join(t1, t2);
                    let mt = types.meet(t02, t12);
                    assert_same(mt, t12);
                }
            }
        }
    }
}

fn assert_same<'t>(a: Ty<'t>, b: Ty<'t>) {
    assert_eq!(a, b);
    assert!(ptr::eq(a.inner(), b.inner()));
}

impl<'t> Types<'t> {
    // By design in meet, args are already flipped to order _type, which forces
    // symmetry for things with badly ordered _type fields.  The question is
    // still interesting for other orders.
    fn check_commute(&self, t0: Ty<'t>, t1: Ty<'t>) {
        if t0 == t1 {
            return;
        }
        // if( t0.is_simple() && !t1.is_simple() ) return; // By design, flipped the only allowed order
        let mta = self.meet(t0, t1);
        let mtb = self.meet(t1, t0); // Reverse args and try again
        assert_same(mta, mtb);
    }

    // A & B = MT
    // Expect: ~A & ~MT == ~A
    // Expect: ~B & ~MT == ~B
    fn check_symmetric(&self, t0: Ty<'t>, t1: Ty<'t>) {
        if t1 == t0 {
            return;
        };
        let mt = self.meet(t0, t1);
        let ta = self.meet(self.dual(mt), self.dual(t1));
        let tb = self.meet(self.dual(mt), self.dual(t0));
        assert_same(ta, self.dual(t1));
        assert_same(tb, self.dual(t0));
    }

    fn assoc(&self, t0: Ty<'t>, t1: Ty<'t>, t2: Ty<'t>) {
        let t01 = self.meet(t0, t1);
        let t12 = self.meet(t1, t2);
        let t01_2 = self.meet(t01, t2);
        let t0_12 = self.meet(t0, t12);
        assert_same(t01_2, t0_12);
    }

    fn gather(&self) -> Vec<Ty<'t>> {
        let struct_test = self.get_struct("test", &[("test", self.int_zero)]);
        let pointer_test = self.get_pointer(struct_test, false);
        let mut ts = vec![
            self.bot,
            self.ctrl,
            //
            self.int_zero,
            self.bot,
            //
            self.get_mem(1),
            self.memory_bot,
            //
            self.pointer_null,
            self.pointer_bot,
            pointer_test,
            //
            struct_test,
            self.struct_bot,
            //
            self.get_tuple_from_array([self.int_bot, pointer_test]),
        ];
        let t2 = ts.iter().map(|t| self.dual(*t)).collect::<Vec<_>>();
        ts.extend(t2);
        ts
    }
}
