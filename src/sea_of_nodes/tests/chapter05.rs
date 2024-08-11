use crate::datastructures::arena::Arena;
use crate::sea_of_nodes::parser::Parser;
use crate::sea_of_nodes::types::Types;

#[test]
fn test_if_stmt() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
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
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
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
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
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
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
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
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
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
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
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
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
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
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
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
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new("return true;", &mut types);
    let _stop = parser.parse().unwrap();

    assert_eq!("return 1;", parser.print_stop());
}
#[test]
fn test_half_def() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new("if( arg==1 ) int b=2; return b;", &mut types);
    assert_eq!(
        Err("Cannot define a new name on one arm of an if".to_string()),
        parser.parse()
    );
}
#[test]
fn test_half_def2() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new(
        "if( arg==1 ) { int b=2; } else { int b=3; } return b;",
        &mut types,
    );
    assert_eq!(Err("Undefined name 'b'".to_string()), parser.parse());
}
#[test]
fn test_regress1() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new("if(arg==2) int a=1; else int b=2; return a;", &mut types);
    assert_eq!(
        Err("Cannot define a new name on one arm of an if".to_string()),
        parser.parse()
    );
}
#[test]
fn test_bad_num() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new("return 1-;", &mut types);
    assert_eq!(
        Err("Syntax error, expected an identifier or expression: ;".to_string()),
        parser.parse()
    );
}
#[test]
fn test_keyword1() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new("int true=0; return true;", &mut types);
    assert_eq!(
        Err("Expected an identifier, found 'true'".to_string()),
        parser.parse()
    );
}

#[test]
fn test_keyword2() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new(
        "int else=arg; if(else) else=2; else else=1; return else;",
        &mut types,
    );
    assert_eq!(
        Err("Expected an identifier, found 'else'".to_string()),
        parser.parse()
    );
}
#[test]
fn test_keyword3() {
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut parser = Parser::new("int a=1; ififif(arg)inta=2;return a;", &mut types);
    assert_eq!(
        Err("Syntax error, expected =: (".to_string()),
        parser.parse()
    );
}
