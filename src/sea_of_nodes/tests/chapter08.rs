use crate::datastructures::arena::Arena;
use crate::sea_of_nodes::nodes::Node;
use crate::sea_of_nodes::parser::Parser;
use crate::sea_of_nodes::tests::{test_error, test_print_stop_and_show};
use crate::sea_of_nodes::types::Types;
use crate::sea_of_nodes::{graph_evaluator, ir_printer};

#[test]
fn test_ex6() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new(
        "
while(arg < 10) {
    arg = arg + 1;
    if (arg == 5)
        break;
    if (arg == 6)
        break;
}
return arg;
                ", &mut types);
    let stop = parser.parse().unwrap();
    parser.show_graph();

    assert_eq!("return Phi(Region36,Phi(Region25,Phi(Loop6,arg,(Phi_arg+1)),Add),Add);", parser.print(stop));
    assert!(matches!(parser.nodes.ret_ctrl(stop), Node::Region {.. }));

    let nodes = parser.nodes;
    assert_eq!(5, graph_evaluator::evaluate(&nodes, &mut types, stop, Some(1), None));
    assert_eq!(10, graph_evaluator::evaluate(&nodes, &mut types, stop, Some(6), None));
}


#[test]
fn test_ex5() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new(
        "
int a = 1;
while(arg < 10) {
    arg = arg + 1;
    if (arg == 5)
        continue;
    if (arg == 7)
        continue;
    a = a + 1;
}
return a;
                ", &mut types);
    let stop = parser.parse().unwrap();
    parser.show_graph();

    assert_eq!("return Phi(Loop7,1,Phi(Region42,Phi_a,(Phi_a+1)));", parser.print(stop));
    assert!(matches!(parser.nodes.ret_ctrl(stop), Node::Proj(_)));
    println!("{}", ir_printer::pretty_print(&parser.nodes, stop, 99));
}


#[test]
fn test_ex4() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new(
        "
while(arg < 10) {
    arg = arg + 1;
    if (arg == 5)
        continue;
    if (arg == 6)
        break;
}
return arg;
                ", &mut types);
    let stop = parser.parse().unwrap();
    parser.show_graph();

    assert_eq!("return Phi(Region34,Phi(Loop6,arg,(Phi_arg+1)),Add);", parser.print(stop));
    assert!(matches!(parser.nodes.ret_ctrl(stop), Node::Region{..}));
}

#[test]
fn test_ex3() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new(
        "
while(arg < 10) {
    arg = arg + 1;
    if (arg == 6)
        break;
}
return arg;
                ", &mut types);
    let stop = parser.parse().unwrap();
    parser.show_graph();

    assert_eq!("return Phi(Region25,Phi(Loop6,arg,(Phi_arg+1)),Add);", parser.print(stop));
    assert!(matches!(parser.nodes.ret_ctrl(stop), Node::Region{..}));
}


#[test]
fn test_ex2() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new(
        "
while(arg < 10) {
    arg = arg + 1;
    if (arg == 5)
        continue;
    if (arg == 6)
        continue;
}
return arg;
                ", &mut types);
    let stop = parser.parse().unwrap();
    parser.show_graph();

    assert_eq!("return Phi(Loop6,arg,(Phi_arg+1));", parser.print(stop));
    assert!(matches!(parser.nodes.ret_ctrl(stop), Node::Proj(_)));
}


#[test]
fn test_ex1() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new(
        "
while(arg < 10) {
    arg = arg + 1;
    if (arg == 5)
        continue;
}
return arg;
                ", &mut types);
    let stop = parser.parse().unwrap();
    parser.show_graph();

    assert_eq!("return Phi(Loop6,arg,(Phi_arg+1));", parser.print(stop));
    assert!(matches!(parser.nodes.ret_ctrl(stop), Node::Proj(_)));
}

#[test]
fn test_regress1() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new(
        "
while( arg < 10 ) {
    int a = arg+2;
    if( a > 4 )
        break;
}
return arg;
                ", &mut types);
    let stop = parser.parse().unwrap();
    parser.show_graph();

    assert_eq!("return arg;", parser.print(stop));
    println!("{}", ir_printer::pretty_print(&parser.nodes, stop, 99));
}

#[test]
fn test_regress2() {
    test_print_stop_and_show(
        "if(1) return 0;  else while(arg>--arg) arg=arg+1; return 0;",
        "Stop[ return 0; return 0; ]",
    );
}

#[test]
fn test_break_outside_loop() {
    test_error("
if(arg <= 10) {
    break;
    arg = arg + 1;
}
return arg;
", "No active loop for a break or continue");
}

#[test]
fn test_regress3() {
    test_print_stop_and_show("
while(arg < 10) {
    break;
}
return arg;
", "return arg;");
}

#[test]
fn test_regress4() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new("
int a = 1;
while(arg < 10) {
    a = a + 1;
    if (arg > 2) {
        int a = 17;
        break;
    }
}
return a;
", &mut types);
    let stop = parser.parse().unwrap();
    parser.show_graph();

    assert_eq!("return Phi(Region28,Phi(Loop7,1,(Phi_a+1)),Add);", parser.print(stop));
    assert!(matches!(parser.nodes.ret_ctrl(stop), Node::Region {..}));
}

#[test]
fn test_regress5() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new("
int a = 1;
while(1) {
    a = a + 1;
    if (a<10) continue;
    break;
}
return a;
", &mut types);
    let stop = parser.parse().unwrap();
    parser.show_graph();

    assert_eq!("return (Phi(Loop7,1,Add)+1);", parser.print(stop));
    assert!(matches!(parser.nodes.ret_ctrl(stop), Node::Region {..}));
}