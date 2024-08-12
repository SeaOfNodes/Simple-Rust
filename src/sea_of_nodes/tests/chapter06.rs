use crate::datastructures::arena::Arena;
use crate::sea_of_nodes::nodes::Node;
use crate::sea_of_nodes::parser::Parser;
use crate::sea_of_nodes::tests::{
    Arg, Show, test_print_stop, test_print_stop_, test_print_stop_and_show,
};
use crate::sea_of_nodes::types::Types;

#[test]
fn test_peephole_return() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
if( true ) return 2;
return 1;
",
        &mut types,
    );
    let stop = parser.parse().unwrap();
    parser.show_graph();
    assert_eq!("return 2;", parser.print_stop());

    let ret = parser.nodes.unique_input(stop).unwrap();
    assert!(matches!(parser.nodes[ret], Node::Return));
    let ret_ctrl = parser.nodes.inputs[ret][0].unwrap();
    assert!(matches!(parser.nodes[ret_ctrl], Node::Proj(_)));
}

#[test]
fn test_peephole_rotate() {
    test_print_stop(
        "\
int a = 1;
if (arg)
    a = 2;
return (arg < a) < 3;
",
        "return ((arg<Phi(Region12,2,1))<3);",
    )
}

#[test]
fn test_peephole_cfg() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int a=1;
if( true )
  a=2;
else
  a=3;
return a;
",
        &mut types,
    );
    let stop = parser.parse().unwrap();
    parser.show_graph();
    assert_eq!("return 2;", parser.print_stop());

    let ret = parser.nodes.unique_input(stop).unwrap();
    assert!(matches!(parser.nodes[ret], Node::Return));
    let ret_ctrl = parser.nodes.inputs[ret][0].unwrap();
    assert!(matches!(parser.nodes[ret_ctrl], Node::Proj(_)));
}

#[test]
fn test_if_if() {
    test_print_stop_and_show(
        "
int a=1;
if( arg!=1 )
    a=2;
else
    a=3;
int b=4;
if( a==2 )
    b=42;
else
    b=5;
return b;",
        "return Phi(Region32,42,5);",
    )
}

#[test]
fn test_if_arg_if() {
    test_print_stop_and_show(
        "\
int a=1;
if( 1==1 )
    a=2;
else
    a=3;
int b=4;
if( arg==2 )
    b=a;
else
    b=5;
return b;",
        "return Phi(Region28,2,5);",
    );
}

#[test]
fn test_merge3_with2() {
    test_print_stop_(
        Show::No,
        Arg::Int(2),
        "
int a=1;
if( arg==1 )
    if( arg==2 )
        a=2;
    else
        a=3;
else if( arg==3 )
    a=4;
else
    a=5;
return a;
#showGraph;",
        "return 5;",
    );
}

#[test]
fn test_merge3_with1() {
    test_print_stop_(
        Show::Yes,
        Arg::Int(1),
        "
int a=1;
if( arg==1 )
    if( arg==2 )
        a=2;
    else
        a=3;
else if( arg==3 )
    a=4;
else
    a=5;
return a;
",
        "return 3;",
    );
}

const MERGE3_PEEPHOLE: &str = "
int a=1;
if( arg==1 )
    if( 1==2 )
        a=2;
    else
        a=3;
else if( arg==3 )
    a=4;
else
    a=5;
return a;
";

#[test]
fn test_merge3_peephole() {
    test_print_stop_(
        Show::Yes,
        Arg::Bot,
        MERGE3_PEEPHOLE,
        "return Phi(Region36,3,Phi(Region34,4,5));",
    );
}

#[test]
fn test_merge3_peephole1() {
    test_print_stop_(Show::Yes, Arg::Int(1), MERGE3_PEEPHOLE, "return 3;");
}

#[test]
fn test_merge3_peephole3() {
    test_print_stop_(Show::Yes, Arg::Int(3), MERGE3_PEEPHOLE, "return 4;");
}

const DEMO1: &str = "
int a = 0;
int b = 1;
if( arg ) {
    a = 2;
    if( arg ) { b = 2; }
    else b = 3;
}
return a+b;
";

#[test]
fn test_demo1_non_const() {
    test_print_stop_(Show::Yes, Arg::Bot, DEMO1, "return Phi(Region22,4,1);");
}

#[test]
fn test_demo1_true() {
    test_print_stop_(Show::Yes, Arg::Int(1), DEMO1, "return 4;");
}

#[test]
fn test_demo1_false() {
    test_print_stop_(Show::Yes, Arg::Int(0), DEMO1, "return 1;");
}

const DEMO2: &str = "
int a = 0;
int b = 1;
int c = 0;
if( arg ) {
    a = 1;
    if( arg==2 ) { c=2; } else { c=3; }
    if( arg ) { b = 2; }
    else b = 3;
}
return a+b+c;
";

#[test]
fn test_demo2_non_const() {
    test_print_stop_(
        Show::Yes,
        Arg::Bot,
        DEMO2,
        "return (Phi(Region33,Phi(Region22,2,3),0)+Phi(Region33,3,1));",
    );
}

#[test]
fn test_demo2_true() {
    test_print_stop_(Show::Yes, Arg::Int(1), DEMO2, "return 6;");
}

#[test]
fn test_demo2_arg2() {
    test_print_stop_(Show::Yes, Arg::Int(2), DEMO2, "return 5;");
}
