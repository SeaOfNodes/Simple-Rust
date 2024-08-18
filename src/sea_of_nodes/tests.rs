use crate::datastructures::arena::Arena;
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

fn test_error(source: &str, error: &str) {
    let arena = Arena::new();
    let mut types = Types::new(&arena);

    assert_eq!(
        Parser::new(source, &mut types).parse(),
        Err(error.to_string()),
    );
}

fn test_print_stop(source: &str, expected: &str) {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new(source, &mut types);
    let stop = parser.parse().unwrap();

    assert_eq!(parser.print(stop), expected);
}

fn test_print_stop_and_show(source: &str, expected: &str) {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new(source, &mut types);
    let stop = parser.parse().unwrap();

    parser.show_graph();

    assert_eq!(parser.print(stop), expected);
}

enum Show {
    No,
    Yes,
}

enum Arg {
    Bot,
    Int(i64),
}

fn test_print_stop_(show: Show, arg: Arg, source: &str, expected: &str) {
    let arena = Arena::new();
    let mut types = Types::new(&arena);

    let arg = match arg {
        Arg::Bot => types.ty_bot,
        Arg::Int(i) => types.get_int(i),
    };

    let mut parser = Parser::new_with_arg(source, &mut types, arg);
    let stop = parser.parse().unwrap();

    if let Show::Yes = show {
        parser.show_graph();
    }

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