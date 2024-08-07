use std::path::Path;

use crate::arena::Arena;
use crate::hir::types::Types;
use crate::soup::soup::Soup;
use crate::syntax::ast::Item;
use crate::syntax::parser::Parser;

#[test]
fn test_simple_program() {
    let parser = Parser::new("fun main() -> Int { return 1+2*3+-5; }", Path::new("dummy.ro"));
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    soup.disable_peephole = true;

    let Item::Function(function) = &ast.items[0] else { unreachable!("expect function") };
    let (_start, stop) = soup.compile_function(function, &mut types).expect("should compile");

    // TODO does it matter that it isn't parsed like this?
    // assert_eq!("return (1+((2*3)+(-5)));", soup.nodes.print(Some(stop)).to_string());
    assert_eq!("return ((1+(2*3))+(-5));", soup.nodes.print(Some(stop)).to_string());
}

#[test]
fn test_add_peephole() {
    let parser = Parser::new("fun main() -> Int { return 1+2; }", Path::new("dummy.ro"));
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else { unreachable!("expect function") };
    let (_start, stop) = soup.compile_function(function, &mut types).expect("should compile");

    assert_eq!("return 3;", soup.nodes.print(Some(stop)).to_string());
}

#[test]
fn test_sub_peephole() {
    let parser = Parser::new("fun main() -> Int { return 1-2; }", Path::new("dummy.ro"));
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else { unreachable!("expect function") };
    let (_start, stop) = soup.compile_function(function, &mut types).expect("should compile");

    assert_eq!("return -1;", soup.nodes.print(Some(stop)).to_string());
}

#[test]
fn test_mul_peephole() {
    let parser = Parser::new("fun main() -> Int { return 2*3; }", Path::new("dummy.ro"));
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else { unreachable!("expect function") };
    let (_start, stop) = soup.compile_function(function, &mut types).expect("should compile");

    assert_eq!("return 6;", soup.nodes.print(Some(stop)).to_string());
}

#[test]
fn test_div_peephole() {
    let parser = Parser::new("fun main() -> Int { return 6/3; }", Path::new("dummy.ro"));
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else { unreachable!("expect function") };
    let (_start, stop) = soup.compile_function(function, &mut types).expect("should compile");

    assert_eq!("return 2;", soup.nodes.print(Some(stop)).to_string());
}

#[test]
fn test_minus_peephole() {
    let parser = Parser::new("fun main() -> Int { return 6/-3; }", Path::new("dummy.ro"));
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else { unreachable!("expect function") };
    let (_start, stop) = soup.compile_function(function, &mut types).expect("should compile");

    assert_eq!("return -2;", soup.nodes.print(Some(stop)).to_string());
}

#[test]
fn test_example() {
    let parser = Parser::new("fun main() -> Int { return 1+2*3+-5; }", Path::new("dummy.ro"));
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else { unreachable!("expect function") };
    let (_start, stop) = soup.compile_function(function, &mut types).expect("should compile");

    assert_eq!("return 2;", soup.nodes.print(Some(stop)).to_string());
}