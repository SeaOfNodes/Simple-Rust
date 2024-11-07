use crate::datastructures::arena::DroplessArena;
use crate::sea_of_nodes::nodes::{Op, ProjOp};
use crate::sea_of_nodes::parser::Parser;
use crate::sea_of_nodes::tests::evaluator::{evaluate, Object};
use crate::sea_of_nodes::tests::test_error;
use crate::sea_of_nodes::types::Types;

#[test]
fn test_ex_6() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
while(arg < 10) {
    arg = arg + 1;
    if (arg == 5)
        break;
    if (arg == 6)
        break;
}
return arg;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(
        parser.print(stop),
        "return Phi(Region36,Phi(Region26,Phi(Loop8,arg,(Phi_arg+1)),Add),Add);"
    );
    assert!(matches!(parser.nodes.ret_ctrl(stop), Op::Region { .. }));
    assert_eq!(
        Object::Long(5),
        evaluate(&parser.nodes, stop, Some(1), None).1
    );
    assert_eq!(
        Object::Long(10),
        evaluate(&parser.nodes, stop, Some(6), None).1
    );
}

#[test]
fn test_ex_5() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
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
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(
        parser.print(stop),
        "return Phi(Loop9,1,Phi(Region41,Phi_a,(Phi_a+1)));"
    );
    assert!(matches!(parser.nodes.ret_ctrl(stop), Op::CProj(_)));
}

#[test]
fn test_ex_4() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
while(arg < 10) {
    arg = arg + 1;
    if (arg == 5)
        continue;
    if (arg == 6)
        break;
}
return arg;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(
        parser.print(stop),
        "return Phi(Region34,Phi(Loop8,arg,(Phi_arg+1)),Add);"
    );
    assert!(matches!(parser.nodes.ret_ctrl(stop), Op::Region { .. }));
}

#[test]
fn test_ex_3() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
while(arg < 10) {
    arg = arg + 1;
    if (arg == 6)
        break;
}
return arg;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(
        parser.print(stop),
        "return Phi(Region26,Phi(Loop8,arg,(Phi_arg+1)),Add);"
    );
    assert!(matches!(parser.nodes.ret_ctrl(stop), Op::Region { .. }));
}

#[test]
fn test_ex_2() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
while(arg < 10) {
    arg = arg + 1;
    if (arg == 5)
        continue;
    if (arg == 6)
        continue;
}
return arg;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return Phi(Loop8,arg,(Phi_arg+1));");
    assert!(matches!(parser.nodes.ret_ctrl(stop), Op::CProj(_)));
}

#[test]
fn test_ex_1() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
while(arg < 10) {
    arg = arg + 1;
    if (arg == 5)
        continue;
}
return arg;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return Phi(Loop8,arg,(Phi_arg+1));");
    assert!(matches!(parser.nodes.ret_ctrl(stop), Op::CProj(_)));
}

#[test]
fn test_regress_1() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
while( arg < 10 ) {
    int a = arg+2;
    if( a > 4 )
        break;
}
return arg;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return arg;");
}

#[test]
fn test_regress_2() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "if(1) return 0;  else while(arg>--arg) arg=arg+1; return 0;",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return 0;");
}

#[test]
fn test_break_outside_loop() {
    test_error(
        "\
if(arg <= 10) {
    break;
    arg = arg + 1;
}
return arg;
",
        "No active loop for a break or continue",
    );
}

#[test]
fn test_regress_3() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
while(arg < 10) {
    break;
}
return arg;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return arg;");
}

#[test]
fn test_regress_4() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int a = 1;
while(arg < 10) {
    a = a + 1;
    if (arg > 2) {
        int a = 17;
        break;
    }
}
return a;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(
        parser.print(stop),
        "return Phi(Region29,Phi(Loop9,1,(Phi_a+1)),Add);"
    );
    assert!(matches!(parser.nodes.ret_ctrl(stop), Op::Region { .. }));
}

#[test]
fn test_regress_5() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int a = 1;
while(1) {
    a = a + 1;
    if (a<10) continue;
    break;
}
return a;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return (Phi(Loop9,1,Add)+1);");
    assert!(matches!(
        parser.nodes.ret_ctrl(stop),
        Op::CProj(ProjOp { index: 1, .. })
    ));
}
