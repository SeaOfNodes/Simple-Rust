use crate::datastructures::arena::DroplessArena;
use crate::sea_of_nodes::parser::Parser;
use crate::sea_of_nodes::tests::test_error;
use crate::sea_of_nodes::types::Types;

#[test]
fn test_if_stmt() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int a = 1;
if (arg == 1)
    a = arg+2;
else {
    a = arg-3;
}
return a;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return Phi(Region,(arg+2),(arg-3));");
}

#[test]
fn test_test() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new_with_arg(
        "\
int c = 3;
int b = 2;
if (arg == 1) {
    b = 3;
    c = 4;
}
return c;",
        &types,
        types.int_bot,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return Phi(Region,4,3);");
}

#[test]
fn test_return_2() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new_with_arg(
        "\
if( arg==1 )
    return 3;
else
    return 4;
",
        &types,
        types.int_bot,
    );
    let stop = parser.parse().unwrap();
    assert_eq!(parser.print(stop), "Stop[ return 3; return 4; ]");
}

#[test]
fn test_if_merge_b() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int a=arg+1;
int b=0;
if( arg==1 )
    b=a;
else
    b=a+1;
return a+b;",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return ((arg*2)+Phi(Region,2,3));");
}

#[test]
fn test_if_merge_2() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int a=arg+1;
int b=arg+2;
if( arg==1 )
    b=b+a;
else
    a=b+1;
return a+b;",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(
        parser.print(stop),
        "return ((Phi(Region,(arg*2),arg)+arg)+Phi(Region,4,5));"
    );
}

#[test]
fn test_merge_3() {
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
        types.int_bot,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(
        parser.print(stop),
        "return Phi(Region,Phi(Region,2,3),Phi(Region,4,5));"
    );
}

#[test]
fn test_merge_4() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new_with_arg(
        "\
int a=0;
int b=0;
if( arg )
    a=1;
if( arg==0 )
    b=2;
return arg+a+b;
",
        &types,
        types.int_bot,
    );
    let stop = parser.parse().unwrap();
    assert_eq!(
        parser.print(stop),
        "return ((arg+Phi(Region,1,0))+Phi(Region,2,0));"
    );
}

#[test]
fn test_merge_5() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int a=arg==2;
if( arg==1 )
{
    a=arg==3;
}
return a;",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return (arg==Phi(Region,3,2));");
}

#[test]
fn test_true() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new("return true;", &types);
    let stop = parser.parse().unwrap();
    assert_eq!(parser.print(stop), "return 1;");
}

#[test]
fn test_half_def() {
    test_error(
        "if( arg==1 ) int b=2; return b;",
        "Cannot define a new name on one arm of an if",
    );
}

#[test]
fn test_half_def_2() {
    test_error(
        "if( arg==1 ) { int b=2; } else { int b=3; } return b;",
        "Undefined name 'b'",
    );
}

#[test]
fn test_regress_1() {
    test_error(
        "if(arg==2) int a=1; else int b=2; return a;",
        "Cannot define a new name on one arm of an if",
    );
}

#[test]
fn test_bad_num() {
    test_error(
        "return 1-;",
        "Syntax error, expected an identifier or expression: ;",
    );
}

#[test]
fn test_keyword_1() {
    test_error(
        "int true=0; return true;",
        "Expected an identifier, found 'true'",
    );
}

#[test]
fn test_keyword_2() {
    test_error(
        "int else=arg; if(else) else=2; else else=1; return else;",
        "Expected an identifier, found 'else'",
    );
}

#[test]
fn test_keyword_3() {
    test_error(
        "int a=1; ififif(arg)inta=2;return a;",
        "Undefined name 'ififif'",
    );
}
