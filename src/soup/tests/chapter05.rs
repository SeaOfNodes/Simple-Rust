use std::path::Path;

use crate::data::arena::Arena;
use crate::soup::soup::Soup;
use crate::soup::types::Types;
use crate::syntax::ast::Item;
use crate::syntax::parser::Parser;

#[test]
fn test_if_stmt() {
    let parser = Parser::new(
        "fun main(arg: Int) -> Int {
            var a = 1;
            if arg == 1 { 
                a = arg+2;
            } else {
                a = arg-3;
                #show_graph;
            }
            #show_graph;
            return a;   
            #show_graph;
        }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let stop = soup
        .compile_function(function, &mut types)
        .expect("should compile");

    assert_eq!(
        "return Phi(Region17,(arg+2),(arg-3));",
        soup.nodes.print(Some(stop)).to_string()
    );
}

#[test]
fn test_test() {
    let parser = Parser::new(
        "fun main(arg: Int) -> Int {
            var c = 3;
            var b = 2;
            if arg == 1 {
                b = 3;
                c = 4;
            }
            return c;
        }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let stop = soup
        .compile_function(function, &mut types)
        .expect("should compile");

    assert_eq!(
        "return Phi(Region16,4,3);",
        soup.nodes.print(Some(stop)).to_string()
    );
}
#[test]
fn test_return2() {
    let parser = Parser::new(
        "fun main(arg: Int) -> Int {
            if arg==1 {
                return 3;
            } else {
                return 4;
            }
            #show_graph;            
        }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let stop = soup
        .compile_function(function, &mut types)
        .expect("should compile");

    assert_eq!(
        "Stop[ return 3; return 4; ]",
        soup.nodes.print(Some(stop)).to_string()
    );
}
#[test]
fn test_if_merge_b() {
    let parser = Parser::new(
        "fun main(arg: Int) -> Int {
            var a=arg+1;
            var b=0;
            if arg==1 {
                b=a;
            } else {
                b=a+1;
            }
            return a+b;
        }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let stop = soup
        .compile_function(function, &mut types)
        .expect("should compile");

    assert_eq!(
        "return ((arg*2)+Phi(Region20,2,3));",
        soup.nodes.print(Some(stop)).to_string()
    );
}
#[test]
fn test_if_merge2() {
    let parser = Parser::new(
        "fun main(arg: Int) -> Int {
            var a=arg+1;
            var b=arg+2;
            if arg==1 {
                b=b+a;
            } else {
                a=b+1;
            }
            return a+b;
        }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let stop = soup
        .compile_function(function, &mut types)
        .expect("should compile");

    assert_eq!(
        "return ((Phi(Region31,(arg*2),arg)+arg)+Phi(Region31,4,5));",
        soup.nodes.print(Some(stop)).to_string()
    );
}
#[test]
fn test_merge3() {
    let parser = Parser::new(
        "fun main(arg: Int) -> Int {
            var a=1;
            if( arg==1 ) {
                if( arg==2 ) {
                    a=2;
                } else {
                    a=3;
                }
            } else if( arg==3 ) {
                a=4;
            } else {
                a=5;
            }
            return a;
            #show_graph;
        }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let stop = soup
        .compile_function(function, &mut types)
        .expect("should compile");

    assert_eq!(
        "return Phi(Region33,Phi(Region21,2,3),Phi(Region31,4,5));",
        soup.nodes.print(Some(stop)).to_string()
    );
}
#[test]
fn test_merge4() {
    let parser = Parser::new(
        "fun main(arg: Int) -> Int {
            var a=0;
            var b=0;
            if( arg ) {
                a=1;
            }
            if( arg==0 ) {
                b=2;
            }
            return arg+a+b;
            #show_graph;
        }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let stop = soup
        .compile_function(function, &mut types)
        .expect("should compile");

    assert_eq!(
        "return ((arg+Phi(Region13,1,0))+Phi(Region22,2,0));",
        soup.nodes.print(Some(stop)).to_string()
    );
}
#[test]
fn test_merge5() {
    //         Parser parser = new Parser(
    // """
    // var a=arg==2;
    // if( arg==1 )
    // {
    //     a=arg==3;
    // }
    // return a;""");
    //         StopNode ret = parser.parse(true);
    //         assertEquals("", ret.toString());
    //     }
    //
    let parser = Parser::new(
        "fun main(arg: Int) -> Int {
            var a=arg==2;
            if arg==1 {
                a=arg==3;
            }
            return a;
            #show_graph;
        }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let stop = soup
        .compile_function(function, &mut types)
        .expect("should compile");

    assert_eq!(
        "return (arg==Phi(Region16,3,2));",
        soup.nodes.print(Some(stop)).to_string()
    );
}
#[test]
fn test_true() {
    let parser = Parser::new(
        "fun main() -> Int {
            return true;
        }",
        Path::new("dummy.ro"),
    );
    let ast = parser.parse().expect("should parse");
    let mut arena = Arena::new();
    let mut types = Types::new(&mut arena);
    let mut soup = Soup::new();

    let Item::Function(function) = &ast.items[0] else {
        unreachable!("expect function")
    };
    let stop = soup
        .compile_function(function, &mut types)
        .expect("should compile");

    assert_eq!("return 1;", soup.nodes.print(Some(stop)).to_string());
}
#[test]
fn test_half_def() {
    let parser = Parser::new(
        "fun main(arg: Int) -> Int {
            if arg==1 { var b=2; } return b;
        }",
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
fn test_half_def2() {
    let parser = Parser::new(
        "fun main(arg: Int) -> Int {
            if arg==1 { var b=2; } else { var b=3; } return b;
        }",
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
fn test_regress1() {
    let parser = Parser::new(
        "fun main(arg: Int) -> Int {
            if arg==2 { var a=1; } else { var b=2; } return a;
        }",
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
fn test_bad_num() {
    let parser = Parser::new(
        "fun main() -> Int {
            return 1-;
        }",
        Path::new("dummy.ro"),
    );
    parser.parse().expect_err("should not parse");
}
#[test]
fn test_keyword1() {
    let parser = Parser::new(
        "fun main() -> Int {
            var true=0; return true;
        }",
        Path::new("dummy.ro"),
    );
    parser.parse().expect_err("should not parse");
}

#[test]
fn test_keyword2() {
    let parser = Parser::new(
        "fun main() -> Int {
            var else=arg;
            if else { else=2; } else { else=1; } 
            return else;
        }",
        Path::new("dummy.ro"),
    );
    parser.parse().expect_err("should not parse");
}
#[test]
fn test_keyword3() {
    let parser = Parser::new(
        "fun main() -> Int {
            var a=1; ififif(arg)inta=2;return a;
        }",
        Path::new("dummy.ro"),
    );
    parser.parse().expect_err("should not parse");
}
