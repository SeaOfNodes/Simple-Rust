use crate::datastructures::arena::Arena;
use crate::sea_of_nodes::parser::Parser;
use crate::sea_of_nodes::types::Types;

mod chapter01;
mod chapter02;
mod chapter03;
mod chapter04;
mod chapter05;

fn test_error(source: &str, error: &str) {
    let arena = Arena::new();
    let mut types = Types::new(&arena);

    assert_eq!(
        Parser::new(source, &mut types).parse(),
        Err(error.to_string()),
    );
}
