use std::alloc::Layout;
use std::cell::{Cell, RefCell};
use std::mem::MaybeUninit;
use std::{ptr, slice};

const CHUNK_SIZE: usize = 4000;

/// This is an arena allocator.
///
/// Its main purpose is to create references that share the same lifetime
/// and can thus be passed around easily.
///
/// It only works with types that do not need to be dropped
/// which is enforced by requiring them to implement `Copy`.
///
/// This is a combination of rustc_arena::DroplessArena and bumpalo.
/// It does seem to pass all tests with miri, but I wouldn't be surprised if it is unsound.
pub struct DroplessArena {
    start: Cell<*mut u8>,
    end: Cell<*mut u8>,
    allocations: RefCell<Vec<*mut MaybeUninit<u8>>>,
}

impl DroplessArena {
    pub const fn new() -> Self {
        Self {
            start: Cell::new(ptr::null_mut()),
            end: Cell::new(ptr::null_mut()),
            allocations: RefCell::new(vec![]),
        }
    }

    fn alloc_layout(&self, layout: Layout) -> *mut u8 {
        assert!(layout.size() < CHUNK_SIZE, "intended for small values");
        assert_ne!(layout.size(), 0, "would result in identical references");
        debug_assert_ne!(layout.align(), 0, "checked by Layout");
        debug_assert!(layout.align().is_power_of_two(), "checked by Layout");

        loop {
            let end = self.end.get();
            // if end is not null (first time) and no underflow possible, and result greater than null
            if (end as usize) > (layout.size() + layout.align()) {
                let ptr = end.wrapping_sub(layout.size());
                let result = ptr.wrapping_sub(ptr as usize & (layout.align() - 1));
                debug_assert_eq!(result as usize % layout.align(), 0);
                if result >= self.start.get() {
                    self.end.set(result);
                    return result;
                }
            }
            self.grow();
        }
    }

    #[cold]
    fn grow(&self) {
        let chunk = Box::leak(Box::new_uninit_slice(CHUNK_SIZE)).as_mut_ptr();
        self.start.set(chunk.cast());
        self.end.set(chunk.wrapping_add(CHUNK_SIZE).cast()); // could this overflow to 0?
        self.allocations.borrow_mut().push(chunk);
    }

    pub fn alloc<T: Copy>(&self, value: T) -> &mut T {
        let layout = Layout::for_value(&value);
        let result = self.alloc_layout(layout).cast::<T>();
        unsafe {
            // SAFETY: We reserved enough aligned memory, initialize it here, and the
            //         memory won't be freed because self outlives the return value.
            ptr::write(result, value);
            &mut *result
        }
    }

    pub fn alloc_slice_copy<T: Copy>(&self, slice: &[T]) -> &mut [T] {
        let layout = Layout::for_value(slice);
        let result = self.alloc_layout(layout).cast::<T>();
        unsafe {
            // SAFETY: We reserved enough aligned memory, initialize it here, and the
            //         memory won't be freed because self outlives the return value.
            ptr::copy_nonoverlapping(slice.as_ptr(), result, slice.len());
            slice::from_raw_parts_mut(result, slice.len())
        }
    }

    pub fn alloc_str(&self, s: &str) -> &mut str {
        let bytes = self.alloc_slice_copy(s.as_bytes());
        debug_assert_eq!(s.as_bytes(), bytes);
        // SAFETY: the copied bytes are still valid utf8
        unsafe { std::str::from_utf8_unchecked_mut(bytes) }
    }
}

impl Drop for DroplessArena {
    fn drop(&mut self) {
        for &chunk in self.allocations.borrow_mut().iter() {
            // SAFETY: the pointer comes from a boxed slice of that size, and none of the
            //         returned references can outlive self.
            let _ = unsafe { Box::from_raw(slice::from_raw_parts_mut(chunk, CHUNK_SIZE)) };
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::datastructures::arena::{DroplessArena, CHUNK_SIZE};

    #[test]
    fn pointer_values() {
        #[derive(Copy, Clone)]
        struct Test {
            x: u32,
        }

        let arena = DroplessArena::new();

        let a = arena.alloc(Test { x: 0 });
        let b = arena.alloc(Test { x: 1 });
        let c = arena.alloc(Test { x: 2 });

        assert_eq!(a.x, 0);
        assert_eq!(b.x, 1);
        assert_eq!(c.x, 2);

        assert_eq!(
            a as *const Test as usize,
            b as *const Test as usize + size_of::<Test>()
        );
        assert_eq!(
            b as *const Test as usize,
            c as *const Test as usize + size_of::<Test>()
        );
    }

    #[test]
    fn arena() {
        #[derive(Copy, Clone)]
        struct Test {
            x: u32,
        }

        let arena = DroplessArena::new();

        let mut tests: Vec<&mut Test> = Vec::new();
        for i in 0..(2 * CHUNK_SIZE / size_of::<Test>() + 10) {
            let t = arena.alloc(Test { x: i as u32 });

            tests.push(t);
        }

        for (expected, t) in tests.iter_mut().enumerate() {
            assert_eq!(expected as u32, t.x);
            t.x = expected as u32 + 100;
        }

        for (i, t) in tests.iter().enumerate() {
            assert_eq!(i as u32 + 100, t.x);
        }
        assert_eq!(arena.allocations.borrow().len(), 3);
    }

    #[test]
    fn slices() {
        let arena = DroplessArena::new();
        let a = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
        let b = [11, 12, 13, 14, 15, 16, 17];

        let aa = arena.alloc_slice_copy(&a);
        let bb = arena.alloc_slice_copy(&b);

        assert_eq!(aa, a);
        assert_eq!(bb, b);
    }
}
