use crate::datastructures::arena::Arena;
use crate::sea_of_nodes::parser::Parser;
use crate::sea_of_nodes::types::Types;

#[test]
fn test_simple_program() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new("return 1+2*3+-5;", &mut types);
    parser.nodes.disable_peephole = true;
    let _stop = parser.parse_and_show().unwrap();

    assert_eq!("return (1+((2*3)+(-5)));", parser.print_stop());
}

#[test]
fn test_add_peephole() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new("return 1+2;", &mut types);
    let _stop = parser.parse().unwrap();

    assert_eq!("return 3;", parser.print_stop());
}

#[test]
fn test_sub_peephole() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new("return 1-2;", &mut types);
    let _stop = parser.parse().unwrap();

    assert_eq!("return -1;", parser.print_stop());
}

#[test]
fn test_mul_peephole() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new("return 2*3;", &mut types);
    let _stop = parser.parse().unwrap();

    assert_eq!("return 6;", parser.print_stop());
}

#[test]
fn test_div_peephole() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new("return 6/3;", &mut types);
    let _stop = parser.parse().unwrap();

    assert_eq!("return 2;", parser.print_stop());
}

#[test]
fn test_minus_peephole() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new("return 6/-3;", &mut types);
    let _stop = parser.parse().unwrap();

    assert_eq!("return -2;", parser.print_stop());
}

#[test]
fn test_example() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new("return 1+2*3+-5;", &mut types);
    let _stop = parser.parse_and_show().unwrap();

    assert_eq!("return 2;", parser.print_stop());
}
