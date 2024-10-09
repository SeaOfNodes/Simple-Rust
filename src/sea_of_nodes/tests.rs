use crate::datastructures::arena::DroplessArena;
use crate::sea_of_nodes::nodes::index::Stop;
use crate::sea_of_nodes::nodes::{Nodes, Op};
use crate::sea_of_nodes::parser::Parser;
use crate::sea_of_nodes::types::Types;

mod chapter01;
mod chapter02;
mod chapter03;
mod chapter04;
mod chapter05;
mod chapter06;
mod chapter07;
mod chapter08;
mod chapter09;
mod chapter10;
mod fuzzing;
mod type_test;
pub mod evaluator;

fn test_error(source: &str, error: &str) {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);

    assert_eq!(Parser::new(source, &types).parse(), Err(error.to_string()),);
}

fn test_error_iterate(source: &str, error: &str) {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);

    let mut parser = Parser::new(source, &types);
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    assert_eq!(parser.type_check(stop), Err(error.to_string()),);
}

impl<'t> Nodes<'t> {
    pub fn ret_ctrl(&self, stop: Stop) -> &Op<'t> {
        let ret = stop.unique_input(self).unwrap();
        assert!(matches!(&self[ret], Op::Return));
        &self[self.inputs[ret][0].unwrap()]
    }
}
