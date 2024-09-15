use crate::datastructures::arena::DroplessArena;
use crate::sea_of_nodes::parser::Parser;
use crate::sea_of_nodes::types::Types;

use crate::sea_of_nodes::ir_printer::pretty_print_llvm;
use crate::sea_of_nodes::tests::test_error;
use crate::sea_of_nodes::tests::test_error_iterate;

#[test]
fn test_fuzzer() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int a = arg/3;
int b = arg*5;
int x = arg*7;
int y = arg/11;
int p; int g; int h;
if( (arg/13)==0 ) {
    p = x + y;
    g = x;
    h = y;
} else {
    p = a + b;
    g = a;
    h = b;
}
int r = g+h;
return p-r;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();
    assert_eq!(parser.print(stop), "return 0;");
}

#[test]
fn test_struct() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct Bar {
    int a;
    int b;
}
struct Foo {
    int x;
}
Foo? foo = null;
Bar bar = new Bar;
bar.a = 1;
bar.a = 2;
return bar.a;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    parser.type_check(stop).unwrap();
    println!("{}", pretty_print_llvm(&parser.nodes, stop, 99));
    assert_eq!(parser.print(stop), "return 2;");
}

#[test]
fn test_example() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct Vector2D { int x; int y; }
Vector2D v = new Vector2D;
v.x = 1;
if (arg)
    v.y = 2;
else
    v.y = 3;
return v;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.show_graph();
    parser.iterate(stop);
    parser.show_graph();
    parser.type_check(stop).unwrap();
    println!("{}", pretty_print_llvm(&parser.nodes, stop, 99));
    assert_eq!(parser.print(stop), "return new Vector2D;");
}

#[test]
fn test_bug() {
    test_error(
        "\
struct s0 {
    int v0;
}
s0? v1=null;
int v3=v1.zAicm;
",
        "Accessing unknown field 'zAicm' from 'null'",
    );
}

#[test]
fn test_bug_2() {
    test_error(
        "\
struct s0 { int v0; }
arg=0+new s0.0;
",
        "Expected an identifier, found 'null'",
    );
}

#[test]
fn test_loop() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct Bar { int a; }
Bar bar = new Bar;
while (arg) {
    bar.a = bar.a + 2;
    arg = arg + 1;
}
return bar.a;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    parser.type_check(stop).unwrap();
    assert_eq!(parser.print(stop), "return Phi(Loop10,0,(Phi_a+2));");
}

#[test]
fn test_if() {
    // NOTE we don't call iterate because this is a parse error
    test_error(
        "\
struct Bar { int a; }
Bar bar = new Bar;
if (arg) bar = null;
bar.a = 1;
return bar.a;
",
        "Type null is not of declared type *Bar",
    );
}

#[test]
fn test_if_2() {
    test_error_iterate(
        "\
struct Bar { int a; }
Bar? bar = null;
if (arg) bar = new Bar;
bar.a = 1;
return bar.a;
",
        "Might be null accessing 'a'",
    );
}

#[test]
fn test_if_3() {
    test_error(
        "\
struct Bar { int a; }
Bar bar = null;
if (arg) bar = null;
bar.a = 1;
return bar.a;
",
        "Type null is not of declared type *Bar",
    );
}

#[test]
fn test_if_or_null() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct Bar { int a; }
Bar? bar = new Bar;
if (arg) bar = null;
if( bar ) bar.a = 1;
return bar;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    parser.type_check(stop).unwrap();
    assert_eq!(parser.print(stop), "return Phi(Region15,null,new Bar);");
}

#[test]
fn test_if_or_null_2() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct Bar { int a; }
Bar? bar = new Bar;
if (arg) bar = null;
int rez = 3;
if( !bar ) rez=4;
else bar.a = 1;
return rez;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    parser.type_check(stop).unwrap();
    assert_eq!(parser.print(stop), "return Phi(Region32,4,3);");
}

#[test]
fn test_while_with_null_inside() {
    test_error_iterate(
        "\
struct s0 {int v0;}
s0? v0 = new s0;
int ret = 0;
while(arg) {
    ret = v0.v0;
    v0 = null;
    arg = arg - 1;
}
return ret;
",
        "Might be null accessing 'v0'",
    );
}

#[test]
fn test_redeclare_struct() {
    test_error(
        "\
struct s0 {
    int v0;
}
s0? v1=new s0;
s0? v1;
v1=new s0;
",
        "Redefining name 'v1'",
    );
}

#[test]
fn test_iter() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct Iter {
    int x;
    int len;
}
Iter i = new Iter;
i.len = arg;
int sum=0;
while( i.x < i.len ) {
    sum = sum + i.x;
    i.x = i.x + 1;
}
return sum;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    parser.type_check(stop).unwrap();
    assert_eq!(
        parser.print(stop),
        "return Phi(Loop14,0,(Phi(Loop,0,(Phi_x+1))+Phi_sum));"
    );
}

#[test]
fn test_1() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct s0 {int v0;}
s0 ret = new s0;
while(arg) {
    s0 v0 = new s0;
    v0.v0 = arg;
    arg = arg-1;
    if (arg==5) ret=v0;
    #showGraph;
}
return ret;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.show_graph();
    parser.iterate(stop);
    parser.show_graph();
    parser.type_check(stop).unwrap();
    println!("{}", pretty_print_llvm(&parser.nodes, stop, 99));
    assert_eq!(
        parser.print(stop),
        "return Phi(Loop10,new s0,Phi(Region31,new s0,Phi_ret));"
    );
}

#[test]
fn test_2() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct s0 {int v0;}
s0 ret = new s0;
s0 v0 = new s0;
while(arg) {
    v0.v0 = arg;
    arg = arg-1;
    if (arg==5) ret=v0;
        #showGraph;
}
return ret;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.show_graph();
    parser.iterate(stop);
    parser.show_graph();
    parser.type_check(stop).unwrap();
    println!("{}", pretty_print_llvm(&parser.nodes, stop, 99));
    assert_eq!(
        parser.print(stop),
        "return Phi(Loop13,new s0,Phi(Region32,new s0,Phi_ret));"
    );
}

#[test]
fn test_3() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct s0 {int v0;}
s0 ret = new s0;
while(arg < 10) {
    s0 v0 = new s0;
    if (arg == 5) ret=v0;
    arg = arg + 1;
}
return ret;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.show_graph();
    parser.iterate(stop);
    parser.show_graph();
    parser.type_check(stop).unwrap();
    println!("{}", pretty_print_llvm(&parser.nodes, stop, 99));
    assert_eq!(
        parser.print(stop),
        "return Phi(Loop10,new s0,Phi(Region30,new s0,Phi_ret));"
    );
}

#[test]
fn test_bug_3() {
    test_error(
        "\
struct s0 {
    int f0;
}
if(0>=0) return new s0;
return new s0;
int v0=null.f0;
",
        "Accessing unknown field 'f0' from 'null'",
    );
}

#[test]
fn test_bug_4() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
if(0) {
    while(0) if(arg) continue;
    int v0=0;
    while(1) {
        int arg=-arg;
        v0=arg;
    }
}
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.show_graph();
    parser.iterate(stop);
    parser.show_graph();
    parser.type_check(stop).unwrap();
    assert_eq!(parser.print(stop), "Stop[ ]");
}

#[test]
fn test_bug_5() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct s0 {
    int f0;
}
if(0) return 0;
else return new s0;
if(new s0.f0) return 0;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.show_graph();
    parser.iterate(stop);
    parser.show_graph();
    parser.type_check(stop).unwrap();
    assert_eq!(parser.print(stop), "return new s0;");
}

#[test]
fn test_bug_6_missed_worklist() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
while(0) {}
int v4=0;
while(0<arg) {
    v4=v4+1;
    while(1) v4=-v4;
    while(0) arg=-1;
}
return 0;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    parser.type_check(stop).unwrap();
}

#[test]
fn test_bug_7() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct s0 {  int f0; }
s0 v0 = new s0;
while(v0.f0) {}
s0 v1 = v0;
return v1;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    parser.type_check(stop).unwrap();
    assert_eq!(parser.print(stop), "return new s0;");
}

#[test]
fn test_bug_8() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int v2=0;
while(0)
while(0) {}
{
    {
        {
            int v36=0;
            {
                while(0) {
                    {
                        while(-v2) {
                            {
                                while(v36) {
                                                while(v2) return 0;
                                                break;
                                }                            }
                            if(-v2) break;
                        }
                    }
                }
            }
        }    }
}
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    parser.type_check(stop).unwrap();
    assert_eq!(parser.print(stop), "Stop[ ]");
}

#[test]
fn test_bug_9() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int v0=arg==0;
while(v0) continue;
return 0;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    parser.type_check(stop).unwrap();
    assert_eq!(parser.print(stop), "return 0;");
}
