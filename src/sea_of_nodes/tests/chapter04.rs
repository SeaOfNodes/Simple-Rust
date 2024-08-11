use crate::datastructures::arena::Arena;
use crate::sea_of_nodes::nodes::Node;
use crate::sea_of_nodes::parser::Parser;
use crate::sea_of_nodes::types::Types;

#[test]
fn test_peephole() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new("return 1+arg+2; #showGraph;", &mut types);
    let _stop = parser.parse().unwrap();

    assert_eq!("return (arg+3);", parser.print_stop());
}

#[test]
fn test_peephole_2() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new("return (1+arg)+2;", &mut types);
    let _stop = parser.parse().unwrap();

    assert_eq!("return (arg+3);", parser.print_stop());
}

#[test]
fn test_add_0() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new("return 0+arg;", &mut types);
    let _stop = parser.parse().unwrap();

    assert_eq!("return arg;", parser.print_stop());
}

#[test]
fn test_add_add_mul() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new("return arg+0+arg;", &mut types);
    let _stop = parser.parse().unwrap();

    assert_eq!("return (arg*2);", parser.print_stop());
}

#[test]
fn test_peephole_3() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new("return 1+arg+2+arg+3; #showGraph;", &mut types);
    let _stop = parser.parse().unwrap();

    assert_eq!("return ((arg*2)+6);", parser.print_stop());
}

#[test]
fn test_mul_1() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new("return 1*arg;", &mut types);
    let _stop = parser.parse().unwrap();

    assert_eq!("return arg;", parser.print_stop());
}

#[test]
fn test_var_arg() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let arg = types.ty_bot;
    let mut parser = Parser::new_with_arg("return arg; #showGraph;", &mut types, arg);
    let stop = parser.parse().unwrap();

    assert!(matches!(&parser.nodes[stop], Node::Stop));
    let ret = parser.nodes.unique_input(stop).expect("has one ret");
    assert!(matches!(&parser.nodes[ret], Node::Return));

    assert!(matches!(
        parser.nodes[parser.nodes.inputs[ret][0].unwrap()],
        Node::Proj(_)
    ));
    assert!(matches!(
        parser.nodes[parser.nodes.inputs[ret][1].unwrap()],
        Node::Proj(_)
    ));
}

#[test]
fn test_constant_arg() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let arg = types.get_int(2);
    let mut parser = Parser::new_with_arg("return arg; #showGraph;", &mut types, arg);
    let _stop = parser.parse().unwrap();

    assert_eq!("return 2;", parser.print_stop());
}

#[test]
fn test_comp_eq() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new("return 3==3; #showGraph;", &mut types);
    let _stop = parser.parse().unwrap();

    assert_eq!("return 1;", parser.print_stop());
}

#[test]
fn test_comp_eq_2() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new("return 3==4; #showGraph;", &mut types);
    let _stop = parser.parse().unwrap();

    assert_eq!("return 0;", parser.print_stop());
}
#[test]
fn test_comp_neq() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new("return 3!=3; #showGraph;", &mut types);
    let _stop = parser.parse().unwrap();

    assert_eq!("return 0;", parser.print_stop());
}

#[test]
fn test_comp_neq_2() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new("return 3!=4; #showGraph;", &mut types);
    let _stop = parser.parse().unwrap();

    assert_eq!("return 1;", parser.print_stop());
}

#[test]
fn test_bug_1() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new(
        "int a=arg+1; int b=a; b=1; return a+2; #showGraph;",
        &mut types,
    );
    let _stop = parser.parse().unwrap();

    assert_eq!("return (arg+3);", parser.print_stop());
}

#[test]
fn test_bug_2() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new("int a=arg+1; a=a; return a; #showGraph;", &mut types);
    let _stop = parser.parse().unwrap();

    assert_eq!("return (arg+1);", parser.print_stop());
}

#[test]
fn test_bug_3() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new("inta=1; return a;", &mut types);
    assert_eq!(Err("Undefined name 'inta'".to_string()), parser.parse());
}

#[test]
fn test_bug_4() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new("return -arg;", &mut types);
    let _stop = parser.parse().unwrap();

    assert_eq!("return (-arg);", parser.print_stop());
}
