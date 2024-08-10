use std::collections::HashMap;

pub struct StringTableBuilder {
    bytes: Vec<u8>,
    // TODO performance
    // TODO this doesn't reuse substrings
    map: HashMap<Vec<u8>, usize>,
}

impl StringTableBuilder {
    pub fn new() -> Self {
        Self {
            bytes: vec![0],
            map: HashMap::new(),
        }
    }

    pub fn index_of<B: AsRef<[u8]>>(&mut self, bytes: B) -> usize {
        let as_bytes = bytes.as_ref();

        if as_bytes.len() == 0 {
            0
        } else if let Some(&index) = self.map.get(as_bytes) {
            index
        } else {
            let index = self.bytes.len();
            self.bytes.extend_from_slice(as_bytes);
            self.bytes.push(0);
            self.map.insert(Vec::from(as_bytes), index);
            index
        }
    }

    pub fn build(self) -> Vec<u8> {
        return self.bytes;
    }
}

#[cfg(test)]
mod tests {
    use super::StringTableBuilder;

    #[test]
    fn string_table_builder() {
        let mut builder = StringTableBuilder::new();
        assert_eq!(1, builder.index_of("abc"));
        assert_eq!(5, builder.index_of("de"));
        assert_eq!(1, builder.index_of("abc"));
        assert_eq!(8, builder.index_of("f"));
        assert_eq!(5, builder.index_of("de"));
        let result = builder.build();
        assert_eq!(&result, "\0abc\0de\0f\0".as_bytes());
    }
}
