use crate::datastructures::arena::Arena;
use crate::sea_of_nodes::nodes::Node;
use crate::sea_of_nodes::parser::Parser;
use crate::sea_of_nodes::tests::{test_error, test_print_stop};
use crate::sea_of_nodes::types::Types;

#[test]
fn test_simple_program() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new("return 1;", &mut types);
    let stop = parser.parse().unwrap();

    assert!(matches!(&parser.nodes[stop], Node::Stop));
    let ret = parser.nodes.unique_input(stop).unwrap();
    assert!(matches!(&parser.nodes[ret], Node::Return));

    let ctrl = parser.nodes.inputs[ret][0].expect("has ctrl");
    assert!(matches!(parser.nodes[ctrl], Node::Proj(_)));

    let expr = parser.nodes.inputs[ret][1].expect("has expr");
    let Node::Constant(constant) = &parser.nodes[expr] else {
        unreachable!("expect constant");
    };

    assert_eq!(Some(parser.nodes.start), parser.nodes.inputs[expr][0]);
    assert_eq!(1, constant.unwrap_int());
}

#[test]
fn test_zero() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new("return 0;", &mut types);
    let _stop = parser.parse().unwrap();

    assert!(matches!(
        &parser.nodes[parser.nodes.start],
        Node::Start { .. }
    ));
    for output in &parser.nodes.outputs[parser.nodes.start] {
        if let Node::Constant(c) = &parser.nodes[*output] {
            assert_eq!(0, c.unwrap_int());
        }
    }
}

#[test]
fn test_bad1() {
    test_error("ret", "Syntax error, expected =: ");
}

#[test]
fn test_bad2() {
    test_error(
        "return 0123;",
        "Syntax error: integer values cannot start with '0'",
    );
}

#[test]
fn test_not_bad3() {
    test_print_stop("return --12;", "return 12;");
}

#[test]
fn test_bad4() {
    test_error("return 100", "Syntax error, expected ;: ")
}

#[test]
fn test_not_bad5() {
    test_print_stop("return -100;", "return -100;");
}

#[test]
fn test_bad6() {
    test_error("return100", "Syntax error, expected =: ");
}

#[test]
fn test_bad7() {
    test_error("return 1;}", "Syntax error, unexpected }");
}
