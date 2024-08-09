use std::path::Path;

use crate::arena::Arena;
use crate::soup::soup::Soup;
use crate::soup::types::Types;
use crate::syntax::ast::Item;
use crate::syntax::parser::Parser;

#[test]
fn test_one_unreachable_message() {
    let parser = Parser::new(
        "fun main() -> Int { return 1; return 2; return 3; }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let errors = soup
        .compile_function(function, &mut types)
        .expect_err("should fail");

    assert_eq!(errors.len(), 1);
    assert!(errors[0].contains("unreachable"));
}

#[test]
fn test_one_unreachable_message_per_block() {
    let parser = Parser::new(
        "fun main() -> Int { { return 1; return 2; } return 3; return 4; }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let errors = soup
        .compile_function(function, &mut types)
        .expect_err("should fail");

    assert_eq!(errors.len(), 2);
    assert!(errors[0].contains("Unreachable"));
    assert!(errors[1].contains("Unreachable"));
}

#[test]
fn test_not_in_scope_read() {
    let parser = Parser::new(
        "fun main() -> Int { { var a = 3; } return a; }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let errors = soup
        .compile_function(function, &mut types)
        .expect_err("should fail");

    assert_eq!(errors.len(), 1);
    assert!(errors[0].contains("not in scope"));
}

#[test]
fn test_not_in_scope_assign() {
    let parser = Parser::new(
        "fun main() -> Int { { var a = 3; } a = 2; return 1; }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let errors = soup
        .compile_function(function, &mut types)
        .expect_err("should fail");

    assert_eq!(errors.len(), 1);
    assert!(errors[0].contains("not in scope"));
}

#[test]
fn test_already_defined() {
    let parser = Parser::new(
        "fun main() -> Int { var a = 2; var a = 1; return 1; }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let errors = soup
        .compile_function(function, &mut types)
        .expect_err("should fail");

    assert_eq!(errors.len(), 1);
    assert!(errors[0].contains("already defined"));
}
