use crate::datastructures::arena::DroplessArena;
use crate::sea_of_nodes::ir_printer;
use crate::sea_of_nodes::nodes::index::StopId;
use crate::sea_of_nodes::nodes::{Node, Nodes};
use crate::sea_of_nodes::parser::Parser;
use crate::sea_of_nodes::types::Types;

/// assertTrue(stop.ret().ctrl() instanceof ProjNode);
fn assert_ret_ctrl_is_proj(nodes: &Nodes, stop: StopId) {
    assert!(matches!(nodes.ret_ctrl(stop), Node::Proj(_)));
}

#[test]
fn test_example() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "
                while(arg < 10) {
                    arg = arg + 1;
                    #showGraph;
                }
                return arg;
                ",
        &types,
    );
    parser.nodes.disable_peephole = true;
    let stop = parser.parse().unwrap();
    parser.show_graph();
    assert_eq!(parser.print(stop), "return Phi(Loop6,arg,(Phi_arg+1));");
    assert_ret_ctrl_is_proj(&parser.nodes, stop);
}

#[test]
fn test_regression() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "
int a = 1;
if(arg){}else{
    while(a < 10) {
        a = a + 1;
    }
}
return a;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    assert_eq!(
        parser.print(stop),
        "return Phi(Region24,1,Phi(Loop13,1,(Phi_a+1)));"
    );
}

#[test]
fn test_while_nested() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
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
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    assert_eq!(
        parser.print(stop),
        "return Phi(Loop8,0,Phi(Loop20,Phi_sum,(Phi_sum+Phi(Loop,0,(Phi_j+1)))));"
    );
    println!("{}", ir_printer::pretty_print(&parser.nodes, stop, 99));
}

#[test]
fn test_while_scope() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
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
        &types,
    );
    parser.nodes.disable_peephole = true;
    let stop = parser.parse().unwrap();
    parser.show_graph();
    assert_eq!(
        parser.print(stop),
        "return Phi(Loop8,2,Phi(Region27,Phi_b,4));"
    );
    assert_ret_ctrl_is_proj(&parser.nodes, stop);

    println!("{}", ir_printer::pretty_print(&parser.nodes, stop, 99));
}

#[test]
fn test_while_nested_if_and_inc() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
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
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    assert_eq!(
        parser.print(stop),
        "return Phi(Loop8,2,(Phi(Region27,Phi_b,4)+1));"
    );
    assert_ret_ctrl_is_proj(&parser.nodes, stop);
    println!("{}", ir_printer::pretty_print(&parser.nodes, stop, 99));
}

#[test]
fn test_while() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "
int a = 1;
while(a < 10) {
    a = a + 1;
    a = a + 2;
}
return a;
",
        &types,
    );
    parser.nodes.disable_peephole = true;
    let stop = parser.parse().unwrap();
    parser.show_graph();
    assert_eq!(parser.print(stop), "return Phi(Loop7,1,((Phi_a+1)+2));");
    assert_ret_ctrl_is_proj(&parser.nodes, stop);
}

#[test]
fn test_while_peep() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "
int a = 1;
while(a < 10) {
    a = a + 1;
    a = a + 2;
}
return a;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    assert_eq!(parser.print(stop), "return Phi(Loop7,1,(Phi_a+3));");
    assert_ret_ctrl_is_proj(&parser.nodes, stop);
}

#[test]
fn test_while2() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "
int a = 1;
while(arg) a = 2;
return a;
",
        &types,
    );
    parser.nodes.disable_peephole = true;
    let stop = parser.parse().unwrap();
    parser.show_graph();
    assert_eq!(parser.print(stop), "return Phi(Loop7,1,2);");
    assert_ret_ctrl_is_proj(&parser.nodes, stop);

    println!("{}", ir_printer::pretty_print(&parser.nodes, stop, 99));
}

#[test]
fn test_while2_peep() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "
int a = 1;
while(arg) a = 2;
return a;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    assert_eq!(parser.print(stop), "return Phi(Loop7,1,2);");
    assert_ret_ctrl_is_proj(&parser.nodes, stop);
    println!("{}", ir_printer::pretty_print(&parser.nodes, stop, 99));
}

#[test]
fn test_while3() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "
int a = 1;
while(a < 10) {
    int b = a + 1;
    a = b + 2;
}
return a;
",
        &types,
    );
    parser.nodes.disable_peephole = true;
    let stop = parser.parse().unwrap();
    parser.show_graph();
    assert_eq!(parser.print(stop), "return Phi(Loop7,1,((Phi_a+1)+2));");
    assert_ret_ctrl_is_proj(&parser.nodes, stop);
}

#[test]
fn test_while3_peep() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "
int a = 1;
while(a < 10) {
    int b = a + 1;
    a = b + 2;
}
return a;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    assert_eq!(parser.print(stop), "return Phi(Loop7,1,(Phi_a+3));");
    assert_ret_ctrl_is_proj(&parser.nodes, stop);
}

#[test]
fn test_while4() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
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
        &types,
    );
    parser.nodes.disable_peephole = true;
    let stop = parser.parse().unwrap();
    parser.show_graph();
    assert_eq!(parser.print(stop), "return Phi(Loop8,1,((Phi_a+1)+2));");
    assert_ret_ctrl_is_proj(&parser.nodes, stop);
}

#[test]
fn test_while4_peep() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
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
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    assert_eq!(parser.print(stop), "return Phi(Loop8,1,(Phi_a+3));");
    assert_ret_ctrl_is_proj(&parser.nodes, stop);
}
