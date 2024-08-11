use crate::datastructures::arena::Arena;
use crate::sea_of_nodes::parser::Parser;
use crate::sea_of_nodes::tests::test_error;
use crate::sea_of_nodes::types::Types;

#[test]
fn test_if_stmt() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int a = 1;
if (arg == 1)
    a = arg+2;
else {
    a = arg-3;
    #showGraph;
}
#showGraph;
return a;",
        &mut types,
    );
    let _stop = parser.parse_and_show().unwrap();

    assert_eq!("return Phi(Region17,(arg+2),(arg-3));", parser.print_stop());
}

#[test]
fn test_test() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int c = 3;
int b = 2;
if (arg == 1) {
    b = 3;
    c = 4;
}
return c;",
        &mut types,
    );
    let _stop = parser.parse_and_show().unwrap();

    assert_eq!("return Phi(Region16,4,3);", parser.print_stop());
}
#[test]
fn test_return2() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
if( arg==1 )
    return 3;
else
    return 4;
#showGraph;",
        &mut types,
    );
    let _stop = parser.parse().unwrap();

    assert_eq!("Stop[ return 3; return 4; ]", parser.print_stop());
}
#[test]
fn test_if_merge_b() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int a=arg+1;
int b=0;
if( arg==1 )
    b=a;
else
    b=a+1;
return a+b;",
        &mut types,
    );
    let _stop = parser.parse_and_show().unwrap();

    assert_eq!("return ((arg*2)+Phi(Region20,2,3));", parser.print_stop());
}
#[test]
fn test_if_merge2() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int a=arg+1;
int b=arg+2;
if( arg==1 )
    b=b+a;
else
    a=b+1;
return a+b;",
        &mut types,
    );
    let _stop = parser.parse_and_show().unwrap();

    assert_eq!(
        "return ((Phi(Region31,(arg*2),arg)+arg)+Phi(Region31,4,5));",
        parser.print_stop()
    );
}
#[test]
fn test_merge3() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new(
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
#showGraph;",
        &mut types,
    );
    let _stop = parser.parse().unwrap();

    assert_eq!(
        "return Phi(Region33,Phi(Region21,2,3),Phi(Region31,4,5));",
        parser.print_stop()
    );
}
#[test]
fn test_merge4() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int a=0;
int b=0;
if( arg )
    a=1;
if( arg==0 )
    b=2;
return arg+a+b;
#showGraph;",
        &mut types,
    );
    let _stop = parser.parse().unwrap();

    assert_eq!(
        "return ((arg+Phi(Region13,1,0))+Phi(Region22,2,0));",
        parser.print_stop()
    );
}
#[test]
fn test_merge5() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int a=arg==2;
if( arg==1 )
{
    a=arg==3;
}
return a;",
        &mut types,
    );
    let _stop = parser.parse_and_show().unwrap();

    assert_eq!("return (arg==Phi(Region16,3,2));", parser.print_stop());
}
#[test]
fn test_true() {
    let arena = Arena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new("return true;", &mut types);
    let _stop = parser.parse().unwrap();

    assert_eq!("return 1;", parser.print_stop());
}
#[test]
fn test_half_def() {
    test_error(
        "if( arg==1 ) int b=2; return b;",
        "Cannot define a new name on one arm of an if",
    );
}
#[test]
fn test_half_def2() {
    test_error(
        "if( arg==1 ) { int b=2; } else { int b=3; } return b;",
        "Undefined name 'b'",
    );
}
#[test]
fn test_regress1() {
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
fn test_keyword1() {
    test_error(
        "int true=0; return true;",
        "Expected an identifier, found 'true'",
    );
}

#[test]
fn test_keyword2() {
    test_error(
        "int else=arg; if(else) else=2; else else=1; return else;",
        "Expected an identifier, found 'else'",
    );
}
#[test]
fn test_keyword3() {
    test_error(
        "int a=1; ififif(arg)inta=2;return a;",
        "Syntax error, expected =: (",
    );
}
