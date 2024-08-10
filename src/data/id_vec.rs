use std::marker::PhantomData;
use std::ops::{Index, IndexMut};

use crate::data::id::Id;

pub struct IdVec<I, T>(Vec<T>, PhantomData<I>);

impl<I, T> IdVec<I, T> {
    pub fn new(inner: Vec<T>) -> Self {
        Self(inner, PhantomData)
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn push(&mut self, value: T) {
        self.0.push(value)
    }
}

impl<I: Id, T> Index<I> for IdVec<I, T> {
    type Output = T;

    fn index(&self, index: I) -> &Self::Output {
        &self.0[index.index()]
    }
}
impl<I: Id, T> IndexMut<I> for IdVec<I, T> {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        &mut self.0[index.index()]
    }
}
