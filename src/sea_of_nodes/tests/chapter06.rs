use crate::datastructures::arena::DroplessArena;
use crate::sea_of_nodes::nodes::Node;
use crate::sea_of_nodes::parser::Parser;
use crate::sea_of_nodes::types::Types;

#[test]
fn test_peephole_return() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
if( true ) return 2;
return 1;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    assert_eq!("return 2;", parser.print(stop));

    let ret = parser.nodes.unique_input(stop).unwrap();
    assert!(matches!(parser.nodes[ret], Node::Return));
    let ret_ctrl = parser.nodes.inputs[ret][0].unwrap();
    assert!(matches!(parser.nodes[ret_ctrl], Node::Proj(_)));
}

#[test]
fn test_peephole_rotate() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int a = 1;
if (arg)
    a = 2;
return (arg < a) < 3;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    assert_eq!(parser.print(stop), "return ((arg<Phi(Region12,2,1))<3);");
}

#[test]
fn test_peephole_cfg() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int a=1;
if( true )
  a=2;
else
  a=3;
return a;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    assert_eq!("return 2;", parser.print(stop));

    let ret = parser.nodes.unique_input(stop).unwrap();
    assert!(matches!(parser.nodes[ret], Node::Return));
    let ret_ctrl = parser.nodes.inputs[ret][0].unwrap();
    assert!(matches!(parser.nodes[ret_ctrl], Node::Proj(_)));
}

#[test]
fn test_if_if() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
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
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    assert_eq!(parser.print(stop), "return Phi(Region37,42,5);");
}

#[test]
fn test_if_arg_if() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
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
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    assert_eq!(parser.print(stop), "return Phi(Region30,2,5);");
}

#[test]
fn test_merge3_with2() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new_with_arg(
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
        &types,
        types.get_int(2),
    );
    let stop = parser.parse().unwrap();
    assert_eq!(parser.print(stop), "return 5;");
}

#[test]
fn test_merge3_with1() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);

    let mut parser = Parser::new_with_arg(
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
        &types,
        types.get_int(1),
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    assert_eq!(parser.print(stop), "return 3;");
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
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new_with_arg(MERGE3_PEEPHOLE, &types, types.ty_bot);
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    assert_eq!(
        parser.print(stop),
        "return Phi(Region41,3,Phi(Region39,4,5));"
    );
}

#[test]
fn test_merge3_peephole1() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new_with_arg(MERGE3_PEEPHOLE, &types, types.get_int(1));
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    assert_eq!(parser.print(stop), "return 3;");
}

#[test]
fn test_merge3_peephole3() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new_with_arg(MERGE3_PEEPHOLE, &types, types.get_int(3));
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    assert_eq!(parser.print(stop), "return 4;");
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
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new_with_arg(DEMO1, &types, types.ty_bot);
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    assert_eq!(parser.print(stop), "return Phi(Region24,4,1);");
}

#[test]
fn test_demo1_true() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new_with_arg(DEMO1, &types, types.get_int(1));
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    assert_eq!(parser.print(stop), "return 4;");
}

#[test]
fn test_demo1_false() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new_with_arg(DEMO1, &types, types.get_int(0));
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    assert_eq!(parser.print(stop), "return 1;");
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
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new_with_arg(DEMO2, &types, types.ty_bot);
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    assert_eq!(
        parser.print(stop),
        "return (Phi(Region36,Phi(Region23,2,3),0)+Phi(Region,3,1));"
    );
}

#[test]
fn test_demo2_true() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new_with_arg(DEMO2, &types, types.get_int(1));
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    assert_eq!(parser.print(stop), "return 6;");
}

#[test]
fn test_demo2_arg2() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new_with_arg(DEMO2, &types, types.get_int(2));
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    assert_eq!(parser.print(stop), "return 5;");
}
