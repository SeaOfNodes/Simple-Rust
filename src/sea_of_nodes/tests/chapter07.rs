use crate::datastructures::arena::DroplessArena;
use crate::sea_of_nodes::nodes::Op;
use crate::sea_of_nodes::parser::Parser;
use crate::sea_of_nodes::types::Types;

#[test]
fn test_example() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
while(arg < 10) {
    arg = arg + 1;
}
return arg;
",
        &types,
    );
    parser.nodes.disable_peephole = true;
    let stop = parser.parse().unwrap();
    assert_eq!(parser.print(stop), "return Phi(Loop8,arg,(Phi_arg+1));");
    assert!(matches!(parser.nodes.ret_ctrl(stop), Op::CProj(_)));
}

#[test]
fn test_regression() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
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
    parser.type_check(stop).unwrap();

    assert_eq!(
        parser.print(stop),
        "return Phi(Region26,1,Phi(Loop15,1,(Phi_a+1)));"
    );
}

#[test]
fn test_while_nested() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
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
    parser.type_check(stop).unwrap();

    assert_eq!(
        parser.print(stop),
        "return Phi(Loop10,0,Phi(Loop22,Phi_sum,(Phi_sum+Phi(Loop,0,(Phi_j+1)))));"
    );
}

#[test]
fn test_while_scope() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
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
    assert_eq!(
        parser.print(stop),
        "return Phi(Loop10,2,Phi(Region29,Phi_b,4));"
    );
    assert!(matches!(parser.nodes.ret_ctrl(stop), Op::CProj(_)));
}

#[test]
fn test_while_nested_if_and_inc() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
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
    parser.type_check(stop).unwrap();

    assert_eq!(
        parser.print(stop),
        "return Phi(Loop10,2,(Phi(Region29,Phi_b,4)+1));"
    );
    assert!(matches!(parser.nodes.ret_ctrl(stop), Op::CProj(_)));
}

#[test]
fn test_while() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
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
    assert_eq!(parser.print(stop), "return Phi(Loop9,1,((Phi_a+1)+2));");
    assert!(matches!(parser.nodes.ret_ctrl(stop), Op::CProj(_)));
}

#[test]
fn test_while_peep() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
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
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return Phi(Loop9,1,(Phi_a+3));");
    assert!(matches!(parser.nodes.ret_ctrl(stop), Op::CProj(_)));
}

#[test]
fn test_while_2() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int a = 1;
while(arg) a = 2;
return a;
",
        &types,
    );
    parser.nodes.disable_peephole = true;
    let stop = parser.parse().unwrap();
    assert_eq!(parser.print(stop), "return Phi(Loop9,1,2);");
    assert!(matches!(parser.nodes.ret_ctrl(stop), Op::CProj(_)));
}

#[test]
fn test_while_2_peep() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int a = 1;
while(arg) a = 2;
return a;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return Phi(Loop9,1,2);");
    assert!(matches!(parser.nodes.ret_ctrl(stop), Op::CProj(_)));
}

#[test]
fn test_while_3() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
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
    assert_eq!(parser.print(stop), "return Phi(Loop9,1,((Phi_a+1)+2));");
    assert!(matches!(parser.nodes.ret_ctrl(stop), Op::CProj(_)));
}

#[test]
fn test_while_3_peep() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
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
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return Phi(Loop9,1,(Phi_a+3));");
    assert!(matches!(parser.nodes.ret_ctrl(stop), Op::CProj(_)));
}

#[test]
fn test_while_4() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
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
    assert_eq!(parser.print(stop), "return Phi(Loop10,1,((Phi_a+1)+2));");
    assert!(matches!(parser.nodes.ret_ctrl(stop), Op::CProj(_)));
}

#[test]
fn test_while_4_peep() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
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
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return Phi(Loop10,1,(Phi_a+3));");
    assert!(matches!(parser.nodes.ret_ctrl(stop), Op::CProj(_)));
}
