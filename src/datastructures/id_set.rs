use std::marker::PhantomData;

use crate::datastructures::id::Id;

pub struct IdSet<E> {
    words: Vec<usize>,
    phantom_data: PhantomData<E>,
}

impl<E: Id> IdSet<E> {
    pub fn zeros(capacity: usize) -> Self {
        Self {
            words: vec![
                0;
                if capacity == 0 {
                    0
                } else {
                    capacity / usize::BITS as usize + 1
                }
            ],
            phantom_data: Default::default(),
        }
    }

    fn ensure_size(&mut self, index: usize) {
        let size = index + 1;
        if size > self.words.len() {
            self.words.resize(size, 0)
        }
    }

    fn index(element: &E) -> (usize, u32) {
        let i = element.index();
        let word = i / usize::BITS as usize;
        let bit = (i % usize::BITS as usize) as u32;
        (word, bit)
    }

    pub fn add(&mut self, element: E) {
        let (word, bit) = IdSet::index(&element);
        self.ensure_size(word);
        self.words[word] |= 1 << bit;
        debug_assert!(self.get(element));
    }

    pub fn remove(&mut self, element: E) {
        let (word, bit) = IdSet::index(&element);
        if let Some(word) = self.words.get_mut(word) {
            *word &= !(1 << bit);
        }
        debug_assert!(!self.get(element));
    }

    pub fn get(&self, element: E) -> bool {
        let (word, bit) = IdSet::index(&element);
        self.words.get(word).is_some_and(|w| (w & (1 << bit)) != 0)
    }
}

#[cfg(test)]
mod tests {
    use super::IdSet;

    #[test]
    fn basic() {
        let mut set = IdSet::zeros(0);
        set.add(42);
        assert!(set.get(42));
        set.add(43);
        set.remove(42);
        assert!(!set.get(42));
        assert!(set.get(43));
        set.remove(43);

        for i in 0..100 {
            assert!(!set.get(i));
        }

        assert_eq!(set.words.iter().sum::<usize>(), 0);
    }
}
