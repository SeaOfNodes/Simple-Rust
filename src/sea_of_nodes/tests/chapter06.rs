use crate::datastructures::arena::DroplessArena;
use crate::sea_of_nodes::nodes::Op;
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
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return 2;");
    assert!(matches!(parser.nodes.ret_ctrl(stop), Op::CProj(_)));
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
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return ((arg<Phi(Region14,2,1))<3);");
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
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return 2;");
    assert!(matches!(parser.nodes.ret_ctrl(stop), Op::CProj(_)));
}

#[test]
fn test_if_if() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new_with_arg(
        "\
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
        types.int_bot,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return Phi(Region39,42,5);");
}

#[test]
fn test_if_arg_if() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new_with_arg(
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
        types.int_bot,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return Phi(Region32,2,5);");
}

#[test]
fn test_merge_3_with_2() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new_with_arg(
        "\
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
        types.get_int(2),
    );
    let stop = parser.parse().unwrap();
    assert_eq!(parser.print(stop), "return 5;");
}

#[test]
fn test_merge_3_with_1() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new_with_arg(
        "\
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
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return 3;");
}

#[test]
fn test_merge_3_peephole() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new_with_arg(
        "\
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
",
        &types,
        types.int_bot,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(
        parser.print(stop),
        "return Phi(Region43,3,Phi(Region41,4,5));"
    );
}

#[test]
fn test_merge_3_peephole_1() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new_with_arg(
        "\
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
",
        &types,
        types.get_int(1),
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return 3;");
}

#[test]
fn test_merge_3_peephole_3() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new_with_arg(
        "\
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
",
        &types,
        types.get_int(3),
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return 4;");
}

#[test]
fn test_demo_1_non_const() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int a = 0;
int b = 1;
if( arg ) {
    a = 2;
    if( arg ) { b = 2; }
    else b = 3;
}
return a+b;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return Phi(Region26,4,1);");
}

#[test]
fn test_demo_1_true() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new_with_arg(
        "\
int a = 0;
int b = 1;
if( arg ) {
    a = 2;
    if( arg ) { b = 2; }
    else b = 3;
}
return a+b;
",
        &types,
        types.get_int(1),
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return 4;");
}

#[test]
fn test_demo_1_false() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new_with_arg(
        "\
int a = 0;
int b = 1;
if( arg ) {
    a = 2;
    if( arg ) { b = 2; }
    else b = 3;
}
return a+b;
",
        &types,
        types.get_int(0),
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return 1;");
}

#[test]
fn test_demo_2_non_const() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
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
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(
        parser.print(stop),
        "return (Phi(Region38,Phi(Region25,2,3),0)+Phi(Region,3,1));"
    );
}

#[test]
fn test_demo_2_true() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new_with_arg(
        "\
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
",
        &types,
        types.get_int(1),
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return 6;");
}

#[test]
fn test_demo_2arg_2() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new_with_arg(
        "\
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
",
        &types,
        types.get_int(2),
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return 5;");
}
