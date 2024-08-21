use std::num::Wrapping;

/// a simple rng for the worklist that behaves like java
pub struct Random {
    seed: Wrapping<u64>,
}

const MULTIPLIER: Wrapping<u64> = Wrapping(0x5DEECE66D);
const ADDEND: Wrapping<u64> = Wrapping(0xB);
const MASK: Wrapping<u64> = Wrapping((1 << 48) - 1);

impl Random {
    pub fn with_seed(seed: u64) -> Self {
        Self {
            seed: (Wrapping(seed) ^ MULTIPLIER) & MASK,
        }
    }

    fn next(&mut self, bits: u8) -> Wrapping<u64> {
        self.seed = ((self.seed * MULTIPLIER) + ADDEND) & MASK;
        self.seed >> (48 - bits) as usize
    }

    pub fn next_int(&mut self, exclusive_bound: i32) -> usize {
        assert!(exclusive_bound > 0);
        let bound = Wrapping(exclusive_bound as u64);

        let mut r = self.next(31);
        let m = bound - Wrapping(1);

        if (bound & m).0 == 0 {
            r = (bound * r) >> 31 // if power of two
        } else {
            let mut u = r;
            loop {
                r = u % bound;
                if u + m >= r {
                    break;
                }
                u = self.next(31)
            }
        }
        r.0 as usize
    }
}

#[cfg(test)]
mod tests {
    use crate::datastructures::random::Random;

    #[test]
    fn test1() {
        let mut r = Random::with_seed(123);
        assert_eq!(r.next_int(2), 1);
        assert_eq!(r.next_int(2), 0);
        assert_eq!(r.next_int(4), 3);
        assert_eq!(r.next_int(321), 227);
        assert_eq!(r.next_int(64), 16);
        assert_eq!(r.next_int(93), 14);
        assert_eq!(r.next_int(64), 38);
        assert_eq!(r.next_int(64), 16);
        assert_eq!(r.next_int(11), 8);
    }

    #[test]
    fn test2() {
        // var r = new Random(123);
        // IntStream.range(1, 100).map(r::nextInt).toArray()
        let mut r = Random::with_seed(123);
        let array = (1..100).map(|i| r.next_int(i)).collect::<Vec<_>>();
        assert_eq!(
            array,
            &[
                0, 0, 2, 1, 0, 5, 3, 2, 8, 3, 0, 6, 7, 5, 7, 10, 10, 5, 18, 16, 14, 3, 4, 2, 3, 3,
                10, 20, 1, 4, 28, 10, 24, 33, 7, 15, 8, 2, 32, 26, 3, 2, 5, 14, 14, 10, 18, 18, 35,
                40, 48, 38, 27, 44, 38, 13, 10, 52, 47, 37, 35, 55, 14, 25, 32, 8, 61, 39, 47, 50,
                64, 37, 3, 13, 51, 73, 44, 16, 9, 30, 48, 23, 59, 81, 14, 73, 5, 82, 51, 64, 42,
                20, 67, 66, 78, 45, 8, 55, 12
            ]
        );
    }
}
