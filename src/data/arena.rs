use std::cell::RefCell;

const ALLOCATION_CAPACITY: usize = 1000;

pub struct Arena<T> {
    inner: RefCell<Inner<T>>,
}

struct Inner<T> {
    current: Vec<T>,
    allocations: Vec<Vec<T>>,
}

impl<T> Arena<T> {
    pub fn new() -> Self {
        Self {
            inner: RefCell::new(Inner {
                current: Vec::with_capacity(ALLOCATION_CAPACITY),
                allocations: vec![],
            }),
        }
    }

    pub fn alloc(&self, t: T) -> &mut T {
        let mut inner = self.inner.borrow_mut();
        if inner.current.len() == inner.current.capacity() {
            let previous =
                std::mem::replace(&mut inner.current, Vec::with_capacity(ALLOCATION_CAPACITY));
            inner.allocations.push(previous);
        }
        let nth = inner.current.len();
        debug_assert!(nth < inner.current.capacity());
        inner.current.push(t);
        unsafe { &mut *inner.current.as_mut_ptr().add(nth) }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pointer_values() {
        struct Test {
            x: u32,
        }

        let arena = Arena::new();

        let a = arena.alloc(Test { x: 0 });
        let b = arena.alloc(Test { x: 1 });
        let c = arena.alloc(Test { x: 2 });

        assert_eq!(a.x, 0);
        assert_eq!(b.x, 1);
        assert_eq!(c.x, 2);

        assert_eq!(
            b as *const Test as usize,
            a as *const Test as usize + std::mem::size_of::<Test>()
        );
        assert_eq!(
            c as *const Test as usize,
            b as *const Test as usize + std::mem::size_of::<Test>()
        );
    }

    #[test]
    fn arena() {
        struct Test {
            x: u32,
        }

        let arena = Arena::new();

        let mut tests: Vec<&mut Test> = Vec::new();
        for i in 0..2_000 {
            let t = arena.alloc(Test { x: i });

            tests.push(t);
        }

        for (expected, t) in tests.iter_mut().enumerate() {
            assert_eq!(expected as u32, t.x);
            t.x = expected as u32 + 100;
        }

        for (i, t) in tests.iter().enumerate() {
            assert_eq!(i as u32 + 100, t.x);
        }
    }
}
