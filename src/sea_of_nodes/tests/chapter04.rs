use crate::datastructures::arena::DroplessArena;
use crate::sea_of_nodes::nodes::Op;
use crate::sea_of_nodes::parser::Parser;
use crate::sea_of_nodes::tests::test_error;
use crate::sea_of_nodes::types::Types;

#[test]
fn test_peephole() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new("return 1+arg+2; #showGraph;", &types);
    let stop = parser.parse().unwrap();
    assert_eq!(parser.print(stop), "return (arg+3);");
}

#[test]
fn test_peephole_2() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new("return (1+arg)+2;", &types);
    let stop = parser.parse().unwrap();
    assert_eq!(parser.print(stop), "return (arg+3);");
}

#[test]
fn test_add_0() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new("return 0+arg;", &types);
    let stop = parser.parse().unwrap();
    assert_eq!(parser.print(stop), "return arg;");
}

#[test]
fn test_add_add_mul() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new("return arg+0+arg;", &types);
    let stop = parser.parse().unwrap();
    assert_eq!(parser.print(stop), "return (arg*2);");
}

#[test]
fn test_peephole_3() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new("return 1+arg+2+arg+3; #showGraph;", &types);
    let stop = parser.parse().unwrap();
    assert_eq!(parser.print(stop), "return ((arg*2)+6);");
}

#[test]
fn test_mul_1() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new("return 1*arg;", &types);
    let stop = parser.parse().unwrap();
    assert_eq!(parser.print(stop), "return arg;");
}

#[test]
fn test_var_arg() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let arg = types.ty_bot;
    let mut parser = Parser::new_with_arg("return arg; #showGraph;", &types, arg);
    let stop = parser.parse().unwrap();

    assert!(matches!(&parser.nodes[stop], Op::Stop));
    let ret = stop.unique_input(&mut parser.nodes).expect("has one ret");
    assert!(matches!(&parser.nodes[ret], Op::Return));

    assert!(matches!(
        parser.nodes[parser.nodes.inputs[ret][0].unwrap()],
        Op::Proj(_)
    ));
    assert!(matches!(
        parser.nodes[parser.nodes.inputs[ret][1].unwrap()],
        Op::Proj(_)
    ));
}

#[test]
fn test_constant_arg() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let arg = types.get_int(2);
    let mut parser = Parser::new_with_arg("return arg; #showGraph;", &types, arg);
    let stop = parser.parse().unwrap();

    assert_eq!("return 2;", parser.print(stop));
}

#[test]
fn test_comp_eq() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new("return 3==3; #showGraph;", &types);
    let stop = parser.parse().unwrap();
    assert_eq!(parser.print(stop), "return 1;");
}

#[test]
fn test_comp_eq_2() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new("return 3==4; #showGraph;", &types);
    let stop = parser.parse().unwrap();
    assert_eq!(parser.print(stop), "return 0;");
}
#[test]
fn test_comp_neq() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new("return 3!=3; #showGraph;", &types);
    let stop = parser.parse().unwrap();
    assert_eq!(parser.print(stop), "return 0;");
}

#[test]
fn test_comp_neq_2() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new("return 3!=4; #showGraph;", &types);
    let stop = parser.parse().unwrap();
    assert_eq!(parser.print(stop), "return 1;");
}

#[test]
fn test_bug_1() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new("int a=arg+1; int b=a; b=1; return a+2; #showGraph;", &types);
    let stop = parser.parse().unwrap();
    assert_eq!(parser.print(stop), "return (arg+3);");
}

#[test]
fn test_bug_2() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new("int a=arg+1; a=a; return a; #showGraph;", &types);
    let stop = parser.parse().unwrap();
    assert_eq!(parser.print(stop), "return (arg+1);");
}

#[test]
fn test_bug_3() {
    test_error("inta=1; return a;", "Undefined name 'inta'");
}

#[test]
fn test_bug_4() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new("return -arg;", &types);
    let stop = parser.parse().unwrap();
    assert_eq!(parser.print(stop), "return (-arg);");
}
