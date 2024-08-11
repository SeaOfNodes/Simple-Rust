use crate::datastructures::arena::Arena;
use crate::sea_of_nodes::nodes::Node;
use crate::sea_of_nodes::parser::Parser;
use crate::sea_of_nodes::types::Types;

#[test]
fn test_simple_program() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new("return 1;", &mut types);
    let stop = parser.parse().unwrap();

    assert!(matches!(&parser.nodes[stop], Node::Stop));
    let ret = parser.nodes.unique_input(stop).unwrap();
    assert!(matches!(&parser.nodes[ret], Node::Return));

    let ctrl = parser.nodes.inputs[ret][0].expect("has ctrl");
    assert!(matches!(parser.nodes[ctrl], Node::Proj(_)));

    let expr = parser.nodes.inputs[ret][1].expect("has expr");
    let Node::Constant(constant) = &parser.nodes[expr] else {
        unreachable!("expect constant");
    };

    assert_eq!(Some(parser.nodes.start), parser.nodes.inputs[expr][0]);
    assert_eq!(1, constant.unwrap_int());
}

#[test]
fn test_zero() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new("return 0;", &mut types);
    let _stop = parser.parse().unwrap();

    assert!(matches!(
        &parser.nodes[parser.nodes.start],
        Node::Start { .. }
    ));
    for output in &parser.nodes.outputs[parser.nodes.start] {
        if let Node::Constant(c) = &parser.nodes[*output] {
            assert_eq!(0, c.unwrap_int());
        }
    }
}

#[test]
fn test_bad1() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);

    assert_eq!(
        Err("Syntax error, expected =: ".to_string()),
        Parser::new("ret", &mut types).parse()
    );
}

#[test]
fn test_bad2() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);

    assert_eq!(
        Err("Syntax error: integer values cannot start with '0'".to_string()),
        Parser::new("return 0123;", &mut types).parse()
    );
}

#[test]
fn test_not_bad3() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new("return 0123;", &mut types);
    let _stop = parser.parse();

    // this test used to fail in chapter 1
    assert_eq!("return 12;", parser.print_stop());
}

#[test]
fn test_bad4() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);

    assert_eq!(
        Err("Syntax error, expected ;: ".to_string()),
        Parser::new("return 100", &mut types).parse()
    );
}

#[test]
fn test_not_bad5() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new("return -100;", &mut types);
    let _stop = parser.parse();

    // this test used to fail in chapter 1
    assert_eq!("return -100;", parser.print_stop());
}

#[test]
fn test_bad6() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);

    assert_eq!(
        Err("Syntax error, expected =: ".to_string()),
        Parser::new("return100", &mut types).parse()
    );
}

#[test]
fn test_bad7() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);

    assert_eq!(
        Err("Syntax error, unexpected }".to_string()),
        Parser::new("return 1;}", &mut types).parse()
    );
}
