use crate::datastructures::arena::Arena;
use crate::sea_of_nodes::parser::Parser;
use crate::sea_of_nodes::tests::{test_error, test_print_stop};
use crate::sea_of_nodes::types::Types;

#[test]
fn test_var_decl() {
    test_print_stop("int a=1; return a;", "return 1;");
}

#[test]
fn test_var_add() {
    test_print_stop("int a=1; int b=2; return a+b;", "return 3;");
}

#[test]
fn test_var_scope() {
    test_print_stop(
        "int a=1; int b=2; int c=0; { int b=3; c=a+b; } return c;",
        "return 4;",
    );
}

#[test]
fn test_var_scope_no_peephole() {
    let arena = Arena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "int a=1; int b=2; int c=0; { int b=3; c=a+b; #showGraph; } return c; #showGraph;",
        &types,
    );
    parser.nodes.disable_peephole = true;
    let stop = parser.parse().unwrap();

    assert_eq!("return (1+3);", parser.print(stop));
}

#[test]
fn test_var_dist() {
    test_print_stop("int x0=1; int y0=2; int x1=3; int y1=4; return (x0-x1)*(x0-x1) + (y0-y1)*(y0-y1); #showGraph;", "return 8;");
}

#[test]
fn test_self_assign() {
    test_error("int a=a; return a;", "Undefined name 'a'");
}

#[test]
fn test_bad_1() {
    test_error(
        "int a=1; int b=2; int c=0; { int b=3; c=a+b;",
        "Syntax error, expected }: ",
    );
}
