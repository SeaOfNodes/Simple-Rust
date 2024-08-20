use crate::datastructures::arena::Arena;
use crate::sea_of_nodes::parser::Parser;
use crate::sea_of_nodes::tests::test_print_stop;
use crate::sea_of_nodes::types::Types;

#[test]
fn test_parse_grammar() {
    let arena = Arena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new("return 1+2*3+-5;", &types);
    parser.nodes.disable_peephole = true;
    let stop = parser.parse().unwrap();

    parser.show_graph();

    assert_eq!("return ((1+(2*3))+(-5));", parser.print(stop));
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
    let arena = Arena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new("return 1+2*3+-5;", &types);
    let stop = parser.parse().unwrap();

    parser.show_graph();

    assert_eq!(parser.print(stop), "return 2;");
}
