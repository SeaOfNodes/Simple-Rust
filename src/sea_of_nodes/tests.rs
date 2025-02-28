use crate::datastructures::arena::DroplessArena;
use crate::sea_of_nodes::nodes::node::Stop;
use crate::sea_of_nodes::nodes::{Nodes, Op};
use crate::sea_of_nodes::parser::Parser;
use crate::sea_of_nodes::types::Types;

mod brainfuck;
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
mod chapter11;
mod chapter12;
mod chapter13;
mod chapter14;
mod chapter15;
mod chapter16;
mod chapter17;
pub mod evaluator;
mod rust;
mod scheduler;
mod scheduler_test;
mod type_test;

fn test_error(source: &str, error: &str) {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);

    assert_eq!(Parser::new(source, &types).parse(), Err(error.to_string()),);
}

fn test_error_iterate(source: &str, error: &str) {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);

    let mut parser = Parser::new(source, &types);
    match parser.parse() {
        Err(err) => assert_eq!(err, error),
        Ok(stop) => {
            parser.iterate(stop);
            assert_eq!(parser.type_check(stop), Err(error.to_string()));
        }
    }
}

impl<'t> Nodes<'t> {
    pub fn ret_ctrl(&self, stop: Stop) -> &Op<'t> {
        let ret = stop.unique_input(self).unwrap();
        assert!(matches!(&self[ret], Op::Return));
        &self[self.inputs[ret][0].unwrap()]
    }
}
