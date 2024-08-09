use std::path::Path;

use crate::arena::Arena;
use crate::soup::nodes::Node;
use crate::soup::soup::Soup;
use crate::soup::types::Types;
use crate::syntax::ast::Item;
use crate::syntax::parser::Parser;

#[test]
fn test_peephole() {
    let parser = Parser::new(
        "fun main(arg: Int) -> Int { return 1+(arg+2); #show_graph; }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let (_start, stop) = soup
        .compile_function(function, &mut types)
        .expect("should compile");

    assert_eq!("return (arg+3);", soup.nodes.print(Some(stop)).to_string());
}

#[test]
fn test_peephole_2() {
    let parser = Parser::new(
        "fun main(arg: Int) -> Int { return (1+arg)+2; }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let (_start, stop) = soup
        .compile_function(function, &mut types)
        .expect("should compile");

    assert_eq!("return (arg+3);", soup.nodes.print(Some(stop)).to_string());
}

#[test]
fn test_add_0() {
    let parser = Parser::new(
        "fun main(arg: Int) -> Int { return 0+arg; }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let (_start, stop) = soup
        .compile_function(function, &mut types)
        .expect("should compile");

    assert_eq!("return arg;", soup.nodes.print(Some(stop)).to_string());
}

#[test]
fn test_add_add_mul() {
    let parser = Parser::new(
        "fun main(arg: Int) -> Int { return arg+0+arg; }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let (_start, stop) = soup
        .compile_function(function, &mut types)
        .expect("should compile");

    assert_eq!("return (arg*2);", soup.nodes.print(Some(stop)).to_string());
}

#[test]
fn test_peephole_3() {
    let parser = Parser::new(
        "fun main(arg: Int) -> Int { return 1+arg+2+arg+3; #show_graph }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let (_start, stop) = soup
        .compile_function(function, &mut types)
        .expect("should compile");

    assert_eq!(
        "return ((arg*2)+6);",
        soup.nodes.print(Some(stop)).to_string()
    );
}

#[test]
fn test_mul_1() {
    let parser = Parser::new(
        "fun main(arg: Int) -> Int { return 1*arg; }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let (_start, stop) = soup
        .compile_function(function, &mut types)
        .expect("should compile");

    assert_eq!("return arg;", soup.nodes.print(Some(stop)).to_string());
}

#[test]
fn test_var_arg() {
    let parser = Parser::new(
        "fun main(arg: Int) -> Int { return arg; #show_graph; }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    soup.set_arg(types.ty_bot);

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let (_start, stop) = soup
        .compile_function(function, &mut types)
        .expect("should compile");

    assert!(matches!(
        soup.nodes[soup.nodes[stop].base().inputs[0].unwrap()],
        Node::ProjNode(_)
    ));
    assert!(matches!(
        soup.nodes[soup.nodes[stop].base().inputs[1].unwrap()],
        Node::ProjNode(_)
    ));
}

#[test]
fn test_constant_arg() {
    let parser = Parser::new(
        "fun main(arg: Int) -> Int { return arg; #show_graph; }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    soup.set_arg(types.get_int(2));

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let (_start, stop) = soup
        .compile_function(function, &mut types)
        .expect("should compile");

    assert_eq!("return 2;", soup.nodes.print(Some(stop)).to_string());
}

#[test]
fn test_comp_eq() {
    let parser = Parser::new(
        "fun main() -> Int { return 3 == 3; #show_graph; }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let (_start, stop) = soup
        .compile_function(function, &mut types)
        .expect("should compile");

    assert_eq!("return 1;", soup.nodes.print(Some(stop)).to_string());
}

#[test]
fn test_comp_eq_2() {
    let parser = Parser::new(
        "fun main() -> Int { return 3 == 4; #show_graph; }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let (_start, stop) = soup
        .compile_function(function, &mut types)
        .expect("should compile");

    assert_eq!("return 0;", soup.nodes.print(Some(stop)).to_string());
}
#[test]
fn test_comp_neq() {
    let parser = Parser::new(
        "fun main() -> Int { return 3 != 3; #show_graph; }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let (_start, stop) = soup
        .compile_function(function, &mut types)
        .expect("should compile");

    assert_eq!("return 0;", soup.nodes.print(Some(stop)).to_string());
}

#[test]
fn test_comp_neq_2() {
    let parser = Parser::new(
        "fun main() -> Int { return 3 != 4; #show_graph; }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let (_start, stop) = soup
        .compile_function(function, &mut types)
        .expect("should compile");

    assert_eq!("return 1;", soup.nodes.print(Some(stop)).to_string());
}

#[test]
fn test_bug_1() {
    let parser = Parser::new(
        "fun main(arg: Int) -> Int { var a: Int = arg + 1; var b = a; b=1; return a+2; #show_graph; }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let (_start, stop) = soup
        .compile_function(function, &mut types)
        .expect("should compile");

    assert_eq!("return (arg+3);", soup.nodes.print(Some(stop)).to_string());
}

#[test]
fn test_bug_2() {
    let parser = Parser::new(
        "fun main(arg: Int) -> Int { var a: Int = arg + 1; a = a; return a; #show_graph; }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let (_start, stop) = soup
        .compile_function(function, &mut types)
        .expect("should compile");

    assert_eq!("return (arg+1);", soup.nodes.print(Some(stop)).to_string());
}

#[test]
fn test_bug_3() {
    let parser = Parser::new(
        "fun main() -> Int { vara = 1; return a; }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let errors = soup
        .compile_function(function, &mut types)
        .expect_err("should not compile");

    assert_eq!(errors.len(), 1);
    assert!(errors[0].contains("not in scope"));
}

#[test]
fn test_bug_4() {
    let parser = Parser::new(
        "fun main(arg: Int) -> Int { return -arg; }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let (_start, stop) = soup
        .compile_function(function, &mut types)
        .expect("should compile");

    assert_eq!("return (-arg);", soup.nodes.print(Some(stop)).to_string());
}
