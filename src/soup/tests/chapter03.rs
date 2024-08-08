use std::path::Path;

use crate::arena::Arena;
use crate::soup::soup::Soup;
use crate::soup::types::Types;
use crate::syntax::ast::Item;
use crate::syntax::parser::Parser;

#[test]
fn test_var_decl() {
    let parser = Parser::new(
        "fun main() -> Int { var a: Int = 1; return a; }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let (_start, stop) = soup
        .compile_function(function, &mut types)
        .expect("should compile");

    assert_eq!("return 1;", soup.nodes.print(Some(stop)).to_string());
}

#[test]
fn test_var_add() {
    let parser = Parser::new(
        "fun main() -> Int { var a: Int = 1; var b = 2; return a + b; }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let (_start, stop) = soup
        .compile_function(function, &mut types)
        .expect("should compile");

    assert_eq!("return 3;", soup.nodes.print(Some(stop)).to_string());
}

#[test]
fn test_var_scope() {
    let parser = Parser::new(
        "fun main() -> Int { var a: Int = 1; var b = 2; var c: Int = 0; { var b: Int = 3; c = a + b; } return c; }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let (_start, stop) = soup
        .compile_function(function, &mut types)
        .expect("should compile");

    assert_eq!("return 4;", soup.nodes.print(Some(stop)).to_string());
}

#[test]
fn test_var_scope_no_peephole() {
    let parser = Parser::new(
        "fun main() -> Int { var a: Int = 1; var b: Int = 2; var c: Int = 0; { var b: Int = 3; c = a + b; #show_graph; } return c; #show_graph; }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();
    soup.disable_peephole = true;

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let (_start, stop) = soup
        .compile_function(function, &mut types)
        .expect("should compile");

    assert_eq!("return (1+3);", soup.nodes.print(Some(stop)).to_string());
}

#[test]
fn test_var_dist() {
    let parser = Parser::new(
        "fun main() -> Int {
            var x0: Int = 1;
            var y0      = 2;
            var x1: Int = 3;
            var y1: Int = 4;
            return (x0-x1)*(x0-x1) + (y0-y1)*(y0-y1);
            #show_graph;
        }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let (_start, stop) = soup
        .compile_function(function, &mut types)
        .expect("should compile");

    assert_eq!("return 8;", soup.nodes.print(Some(stop)).to_string());
}

#[test]
fn test_self_assign() {
    let parser = Parser::new(
        "fun main() -> Int { var a: Int = a; return a; }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let () = soup
        .compile_function(function, &mut types)
        .expect_err("should fail");
}

#[test]
fn test_bad_1() {
    let parser = Parser::new(
        "fun main() -> Int { var a: Int = 1; var b: Int = 2; var c: Int = 0; { var b: Int = 3; c = a + b; }",
        Path::new("dummy.ro"),
    );
    let () = parser.parse().expect_err("should not parse");
}
