pub trait Id {
    fn index(&self) -> usize;
}

impl Id for usize {
    fn index(&self) -> usize {
        *self
    }
}
