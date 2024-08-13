use crate::datastructures::arena::Arena;
use crate::sea_of_nodes::ir_printer;
use crate::sea_of_nodes::nodes::{Node, NodeId, Nodes};
use crate::sea_of_nodes::parser::Parser;
use crate::sea_of_nodes::tests::test_print_stop_and_show;
use crate::sea_of_nodes::types::Types;

/// assertTrue(stop.ret().ctrl() instanceof ProjNode);
fn assert_ret_ctrl_is_proj(nodes: &Nodes, stop: NodeId) {
    assert!(matches!(&nodes[stop], Node::Stop));

    let ret = nodes.unique_input(stop).unwrap();
    assert!(matches!(&nodes[ret], Node::Return));
    assert!(matches!(
        &nodes[nodes.inputs[ret][0].unwrap()],
        Node::Proj(_)
    ));
}

#[test]
fn test_example() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new(
        "
                while(arg < 10) {
                    arg = arg + 1;
                    #showGraph;
                }
                return arg;
                ",
        &mut types,
    );
    parser.nodes.disable_peephole = true;
    let stop = parser.parse().unwrap();
    parser.show_graph();
    assert_eq!(parser.print(stop), "return Phi(Loop6,arg,(Phi_arg+1));");
    assert_ret_ctrl_is_proj(&parser.nodes, stop);
}

#[test]
fn test_regression() {
    test_print_stop_and_show(
        "
int a = 1;
if(arg){}else{
    while(a < 10) {
        a = a + 1;
    }
}
return a;
",
        "return Phi(Region23,1,Phi(Loop11,1,(Phi_a+1)));",
    )
}

#[test]
fn test_while_nested() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new(
        "
int sum = 0;
int i = 0;
while(i < arg) {
    i = i + 1;
    int j = 0;
    while( j < arg ) {
        sum = sum + j;
        j = j + 1;
    }
}
return sum;
",
        &mut types,
    );
    let stop = parser.parse().unwrap();
    parser.show_graph();
    assert_eq!(
        parser.print(stop),
        "return Phi(Loop8,0,Phi(Loop21,Phi_sum,(Phi(Loop,0,(Phi_j+1))+Phi_sum)));"
    );
    println!("{}", ir_printer::print(&parser, stop, 99));
}

#[test]
fn test_while_scope() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new(
        "
int a = 1;
int b = 2;
while(a < 10) {
    if (a == 2) a = 3;
    else b = 4;
}
return b;
",
        &mut types,
    );
    parser.nodes.disable_peephole = true;
    let stop = parser.parse().unwrap();
    parser.show_graph();
    assert_eq!(
        parser.print(stop),
        "return Phi(Loop8,2,Phi(Region27,Phi_b,4));"
    );
    assert_ret_ctrl_is_proj(&parser.nodes, stop);

    println!("{}", ir_printer::print(&parser, stop, 99));
}

#[test]
fn test_while_nested_if_and_inc() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new(
        "
int a = 1;
int b = 2;
while(a < 10) {
    if (a == 2) a = 3;
    else b = 4;
    b = b + 1;
    a = a + 1;
}
return b;
",
        &mut types,
    );
    let stop = parser.parse().unwrap();
    parser.show_graph();
    assert_eq!(
        parser.print(stop),
        "return Phi(Loop8,2,(Phi(Region27,Phi_b,4)+1));"
    );
    assert_ret_ctrl_is_proj(&parser.nodes, stop);
    println!("{}", ir_printer::print(&parser, stop, 99));
}

#[test]
fn test_while() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new(
        "
int a = 1;
while(a < 10) {
    a = a + 1;
    a = a + 2;
}
return a;
",
        &mut types,
    );
    parser.nodes.disable_peephole = true;
    let stop = parser.parse().unwrap();
    parser.show_graph();
    assert_eq!(parser.print(stop), "return Phi(Loop7,1,((Phi_a+1)+2));");
    assert_ret_ctrl_is_proj(&parser.nodes, stop);
}

#[test]
fn test_while_peep() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new(
        "
int a = 1;
while(a < 10) {
    a = a + 1;
    a = a + 2;
}
return a;
",
        &mut types,
    );
    let stop = parser.parse().unwrap();
    parser.show_graph();
    assert_eq!(parser.print(stop), "return Phi(Loop7,1,(Phi_a+3));");
    assert_ret_ctrl_is_proj(&parser.nodes, stop);
}

#[test]
fn test_while2() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new(
        "
int a = 1;
while(arg) a = 2;
return a;
",
        &mut types,
    );
    parser.nodes.disable_peephole = true;
    let stop = parser.parse().unwrap();
    parser.show_graph();
    assert_eq!(parser.print(stop), "return Phi(Loop7,1,2);");
    assert_ret_ctrl_is_proj(&parser.nodes, stop);

    println!("{}", ir_printer::print(&parser, stop, 99));
}

#[test]
fn test_while2_peep() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new(
        "
int a = 1;
while(arg) a = 2;
return a;
",
        &mut types,
    );
    let stop = parser.parse().unwrap();
    assert_eq!(parser.print(stop), "return Phi(Loop7,1,2);");
    assert_ret_ctrl_is_proj(&parser.nodes, stop);
    println!("{}", ir_printer::print(&parser, stop, 99));
}

#[test]
fn test_while3() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new(
        "
int a = 1;
while(a < 10) {
    int b = a + 1;
    a = b + 2;
}
return a;
",
        &mut types,
    );
    parser.nodes.disable_peephole = true;
    let stop = parser.parse().unwrap();
    parser.show_graph();
    assert_eq!(parser.print(stop), "return Phi(Loop7,1,((Phi_a+1)+2));");
    assert_ret_ctrl_is_proj(&parser.nodes, stop);
}

#[test]
fn test_while3_peep() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new(
        "
int a = 1;
while(a < 10) {
    int b = a + 1;
    a = b + 2;
}
return a;
",
        &mut types,
    );
    let stop = parser.parse().unwrap();
    parser.show_graph();
    assert_eq!(parser.print(stop), "return Phi(Loop7,1,(Phi_a+3));");
    assert_ret_ctrl_is_proj(&parser.nodes, stop);
}

#[test]
fn test_while4() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new(
        "
int a = 1;
int b = 2;
while(a < 10) {
    int b = a + 1;
    a = b + 2;
}
return a;
",
        &mut types,
    );
    parser.nodes.disable_peephole = true;
    let stop = parser.parse().unwrap();
    parser.show_graph();
    assert_eq!(parser.print(stop), "return Phi(Loop8,1,((Phi_a+1)+2));");
    assert_ret_ctrl_is_proj(&parser.nodes, stop);
}

#[test]
fn test_while4_peep() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new(
        "
int a = 1;
int b = 2;
while(a < 10) {
    int b = a + 1;
    a = b + 2;
}
return a;
",
        &mut types,
    );
    let stop = parser.parse().unwrap();
    parser.show_graph();
    assert_eq!(parser.print(stop), "return Phi(Loop8,1,(Phi_a+3));");
    assert_ret_ctrl_is_proj(&parser.nodes, stop);
}
