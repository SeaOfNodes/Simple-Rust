use crate::datastructures::arena::Arena;
use crate::sea_of_nodes::nodes::Node;
use crate::sea_of_nodes::parser::Parser;
use crate::sea_of_nodes::tests::{test_error, test_print_stop};
use crate::sea_of_nodes::types::Types;

#[test]
fn test_peephole() {
    test_print_stop("return 1+arg+2; #showGraph;", "return (arg+3);");
}

#[test]
fn test_peephole_2() {
    test_print_stop("return (1+arg)+2;", "return (arg+3);");
}

#[test]
fn test_add_0() {
    test_print_stop("return 0+arg;", "return arg;");
}

#[test]
fn test_add_add_mul() {
    test_print_stop("return arg+0+arg;", "return (arg*2);");
}

#[test]
fn test_peephole_3() {
    test_print_stop("return 1+arg+2+arg+3; #showGraph;", "return ((arg*2)+6);");
}

#[test]
fn test_mul_1() {
    test_print_stop("return 1*arg;", "return arg;");
}

#[test]
fn test_var_arg() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
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
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let arg = types.get_int(2);
    let mut parser = Parser::new_with_arg("return arg; #showGraph;", &mut types, arg);
    let _stop = parser.parse().unwrap();

    assert_eq!("return 2;", parser.print_stop());
}

#[test]
fn test_comp_eq() {
    test_print_stop("return 3==3; #showGraph;", "return 1;");
}

#[test]
fn test_comp_eq_2() {
    test_print_stop("return 3==4; #showGraph;", "return 0;");
}
#[test]
fn test_comp_neq() {
    test_print_stop("return 3!=3; #showGraph;", "return 0;");
}

#[test]
fn test_comp_neq_2() {
    test_print_stop("return 3!=4; #showGraph;", "return 1;");
}

#[test]
fn test_bug_1() {
    test_print_stop(
        "int a=arg+1; int b=a; b=1; return a+2; #showGraph;",
        "return (arg+3);",
    );
}

#[test]
fn test_bug_2() {
    test_print_stop("int a=arg+1; a=a; return a; #showGraph;", "return (arg+1);");
}

#[test]
fn test_bug_3() {
    test_error("inta=1; return a;", "Undefined name 'inta'");
}

#[test]
fn test_bug_4() {
    test_print_stop("return -arg;", "return (-arg);");
}
