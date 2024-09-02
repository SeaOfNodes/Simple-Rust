use crate::datastructures::arena::DroplessArena;
use crate::sea_of_nodes::nodes::{Node, NodeId, Nodes};
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
mod fuzzing;
mod type_test;

fn test_error(source: &str, error: &str) {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);

    assert_eq!(Parser::new(source, &types).parse(), Err(error.to_string()),);
}

fn test_print_stop(source: &str, expected: &str) {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(source, &types);
    let stop = parser.parse().unwrap();

    assert_eq!(parser.print(stop), expected);
}

impl<'t> Nodes<'t> {
    pub fn ret_ctrl(&self, stop: NodeId) -> &Node<'t> {
        assert!(matches!(&self[stop], Node::Stop));

        let ret = self.unique_input(stop).unwrap();
        assert!(matches!(&self[ret], Node::Return));
        &self[self.inputs[ret][0].unwrap()]
    }
}
