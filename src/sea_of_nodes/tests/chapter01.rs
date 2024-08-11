use std::path::Path;

use crate::datastructures::arena::Arena;
use crate::sea_of_nodes::nodes::Node;
use crate::sea_of_nodes::soup::Soup;
use crate::sea_of_nodes::types::Types;
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
    let stop = soup
        .compile_function(function, &mut types)
        .expect("should compile");

    assert!(matches!(&soup.nodes[stop], Node::Stop));
    let ret = soup.nodes.unique_input(stop).unwrap();

    assert!(matches!(&soup.nodes[ret], Node::Return));
    assert!(matches!(
        soup.nodes[soup.nodes.inputs[ret][0].expect("has ctrl")],
        Node::Proj(_)
    ));

    let expr = soup.nodes.inputs[ret][1].expect("has expr");
    let Node::Constant(constant) = &soup.nodes[expr] else {
        unreachable!("expect constant");
    };

    assert_eq!(Some(soup.nodes.start), soup.nodes.inputs[expr][0]);
    assert_eq!(1, constant.unwrap_int());
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
    let _stop = soup
        .compile_function(function, &mut types)
        .expect("should compile");

    let Node::Start { .. } = &soup.nodes[soup.nodes.start] else {
        unreachable!("expect type start");
    };
    for output in &soup.nodes.outputs[soup.nodes.start] {
        if let Node::Constant(c) = &soup.nodes[*output] {
            assert_eq!(0, c.unwrap_int());
        }
    }
}
