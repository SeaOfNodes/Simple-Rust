use crate::datastructures::arena::Arena;
use crate::sea_of_nodes::parser::Parser;
use crate::sea_of_nodes::tests::{test_print_stop, test_print_stop_and_show};
use crate::sea_of_nodes::types::Types;

#[test]
fn test_simple_program() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new("return 1+2*3+-5;", &mut types);
    parser.nodes.disable_peephole = true;
    let _stop = parser.parse().unwrap();

    parser.show_graph();

    assert_eq!("return (1+((2*3)+(-5)));", parser.print_stop());
}

#[test]
fn test_add_peephole() {
    test_print_stop("return 1+2;", "return 3;");
}

#[test]
fn test_sub_peephole() {
    test_print_stop("return 1-2;", "return -1;");
}

#[test]
fn test_mul_peephole() {
    test_print_stop("return 2*3;", "return 6;");
}

#[test]
fn test_div_peephole() {
    test_print_stop("return 6/3;", "return 2;");
}

#[test]
fn test_minus_peephole() {
    test_print_stop("return 6/-3;", "return -2;");
}

#[test]
fn test_example() {
    test_print_stop_and_show("return 1+2*3+-5;", "return 2;");
}
