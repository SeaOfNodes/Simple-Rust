use std::path::Path;

use crate::arena::Arena;
use crate::soup::nodes::Node;
use crate::soup::soup::Soup;
use crate::soup::types::Types;
use crate::syntax::ast::Item;
use crate::syntax::parser::Parser;

#[test]
fn test_simple_program() {
    let parser = Parser::new("fun main() -> Int { return 1; }", Path::new("dummy.ro"));
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();
    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let (start, stop) = soup
        .compile_function(function, &mut types)
        .expect("should compile");

    let Node::ReturnNode(ret) = &soup.nodes[stop] else {
        unreachable!("expect return");
    };
    assert_eq!(ret.ctrl(), Some(start));

    let expr = ret.expr().expect("has expr");
    let Node::ConstantNode(constant) = &soup.nodes[expr] else {
        unreachable!("expect constant");
    };

    assert_eq!(Some(start), constant.start());
    assert_eq!(1, constant.ty().unwrap_int());
}

#[test]
fn test_zero() {
    let parser = Parser::new("fun main() -> Int { return 0; }", Path::new("dummy.ro"));
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let (start, _stop) = soup
        .compile_function(function, &mut types)
        .expect("should compile");

    let Node::StartNode(start) = &soup.nodes[start] else {
        unreachable!("expect type start");
    };
    for output in &start.base.outputs {
        if let Node::ConstantNode(c) = &soup.nodes[*output] {
            assert_eq!(0, c.ty().unwrap_int());
        }
    }
}
