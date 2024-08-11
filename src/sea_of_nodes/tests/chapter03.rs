use crate::datastructures::arena::Arena;
use crate::sea_of_nodes::parser::Parser;
use crate::sea_of_nodes::types::Types;

#[test]
fn test_var_decl() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new("int a=1; return a;", &mut types);
    let _stop = parser.parse().unwrap();

    assert_eq!("return 1;", parser.print_stop());
}

#[test]
fn test_var_add() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new("int a=1; int b=2; return a+b;", &mut types);
    let _stop = parser.parse().unwrap();

    assert_eq!("return 3;", parser.print_stop());
}

#[test]
fn test_var_scope() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new(
        "int a=1; int b=2; int c=0; { int b=3; c=a+b; } return c;",
        &mut types,
    );
    let _stop = parser.parse().unwrap();

    assert_eq!("return 4;", parser.print_stop());
}

#[test]
fn test_var_scope_no_peephole() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new(
        "int a=1; int b=2; int c=0; { int b=3; c=a+b; #showGraph; } return c; #showGraph;",
        &mut types,
    );
    parser.nodes.disable_peephole = true;
    let _stop = parser.parse().unwrap();

    assert_eq!("return (1+3);", parser.print_stop());
}

#[test]
fn test_var_dist() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new(
        "int x0=1; int y0=2; int x1=3; int y1=4; return (x0-x1)*(x0-x1) + (y0-y1)*(y0-y1); #showGraph;",
        &mut types,
    );
    let _stop = parser.parse().unwrap();

    assert_eq!("return 8;", parser.print_stop());
}

#[test]
fn test_self_assign() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new("int a=a; return a;", &mut types);

    assert_eq!(Err("Undefined name 'a'".to_string()), parser.parse());
}

#[test]
fn test_bad_1() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new("int a=1; int b=2; int c=0; { int b=3; c=a+b;", &mut types);
    assert_eq!(
        Err("Syntax error, expected }: ".to_string()),
        parser.parse()
    );
}
