use crate::datastructures::arena::DroplessArena;
use crate::sea_of_nodes::parser::Parser;
use crate::sea_of_nodes::tests::evaluator::{evaluate, Object};
use crate::sea_of_nodes::types::Types;

#[test]
fn test_jig() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "
int v0=0;
arg=0;
while(v0) {
        while(1) if(arg*arg*0==0) {}
                while(0) {}
    arg=1;
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
fn test_gvn1() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "
int x = arg + arg;
if(arg < 10) {
    return arg + arg;
}
else {
    x = x + 1;
}
return x;
                ",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.show_graph();
    assert_eq!(
        "Stop[ return (arg*2); return (Mul+1); ]",
        parser.print(stop)
    );

    assert_eq!(
        Object::Long(2),
        evaluate(&parser.nodes, stop, Some(1), None).1
    );
    assert_eq!(
        Object::Long(23),
        evaluate(&parser.nodes, stop, Some(11), None).1
    );
}

#[test]
fn test_gvn2() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "
return arg*arg-arg*arg;
                ",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.show_graph();
    assert_eq!("return 0;", parser.print(stop));
    assert_eq!(
        Object::Long(0),
        evaluate(&parser.nodes, stop, Some(1), None).1
    );
}

#[test]
fn test_worklist1() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "
int step = 1;
while (arg < 10) {
    arg = arg + step + 1;
}
return arg;
                ",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.show_graph();
    parser.iterate(stop);
    parser.show_graph();
    parser.type_check(stop).unwrap();
    assert_eq!("return Phi(Loop7,arg,(Phi_arg+2));", parser.print(stop));
    assert_eq!(
        Object::Long(11),
        evaluate(&parser.nodes, stop, Some(1), None).1
    );
}

#[test]
fn test_worklist2() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "
int cond = 0;
int one = 1;
while (arg < 10) {
    if (cond) one = 2;
    arg = arg + one*3 + 1;
}
return arg;
                ",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.show_graph();
    parser.iterate(stop);
    parser.show_graph();
    parser.type_check(stop).unwrap();
    assert_eq!("return Phi(Loop8,arg,(Phi_arg+4));", parser.print(stop));
    assert_eq!(
        Object::Long(13),
        evaluate(&parser.nodes, stop, Some(1), None).1
    );
}

#[test]
fn test_worklist3() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "
int v1 = 0;
int v2 = 0;
int v3 = 0;
int v4 = 0;
int v5 = 0;
int v6 = 0;
int v7 = 0;
int v8 = 0;
while (arg) {
    if (v1) v2 = 1;
    if (v2) v3 = 1;
    if (v3) v4 = 1;
    if (v4) v5 = 1;
    if (v5) v6 = 1;
    if (v6) v7 = 1;
    if (v7) v8 = 1;
    arg = arg + v8 + 1;
}
return arg;
                ",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    parser.type_check(stop).unwrap();
    assert_eq!("return Phi(Loop14,arg,(Phi_arg+1));", parser.print(stop));
}

#[test]
fn test_region_peep_bug() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "
int v0=0;
int v1=0;
while(v1+arg) {
    arg=0;
    int v2=v0;
    while(arg+1) {}
    v0=1;
    v1=v2;
}
                ",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    parser.type_check(stop).unwrap();
    assert_eq!("Stop[ ]", parser.print(stop));
}

#[test]
fn test_while0() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new("while(0) continue; if(0) arg=0;", &types);
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    parser.type_check(stop).unwrap();
    assert_eq!("Stop[ ]", parser.print(stop));
}

#[test]
fn test_while1() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "
if(0) while(0) {
    int arg=arg;
    while(0) {}
}
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    parser.type_check(stop).unwrap();
    assert_eq!("Stop[ ]", parser.print(stop));
}

#[test]
fn test_precedence() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new("return 3-1+2;", &types);
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    parser.type_check(stop).unwrap();
    assert_eq!("return 4;", parser.print(stop));
}

#[test]
fn test_swap2() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new("return 1+(1+1);", &types);
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    parser.type_check(stop).unwrap();
    assert_eq!("return 3;", parser.print(stop));
}

#[test]
fn test_fuzz0() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "
int one = 1;
int a = 0;
int zero = 0;
while(arg) {
    a = -(one + a + 2);
    arg = arg + 1;
    one = one + zero;
}
return a;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    parser.type_check(stop).unwrap();
    assert_eq!("return Phi(Loop9,0,(-(Phi_a+3)));", parser.print(stop));
}

#[test]
fn test_fuzz1() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "
while(1) {}
while(arg) break;
while(arg) arg=0;
arg=0;
int v0=0!=0<-0;
return -0+0+0;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    parser.type_check(stop).unwrap();
    assert_eq!("Stop[ ]", parser.print(stop));
}

#[test]
fn test_fuzz2() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new("return 0+-0;", &types);
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    parser.type_check(stop).unwrap();
    assert_eq!("return 0;", parser.print(stop));
}

#[test]
fn test_fuzz3() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new("int v0=0; while(0==69) while(v0) return 0;", &types);
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    parser.type_check(stop).unwrap();
    assert_eq!("Stop[ ]", parser.print(stop));
}

#[test]
fn test_fuzz4() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "
while(1) {
    arg=0<=0;
    if(1<0) while(arg==-0) arg=arg-arg;
}
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    parser.type_check(stop).unwrap();
    assert_eq!("Stop[ ]", parser.print(stop));
}

#[test]
fn test_fuzz5() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "
{
    int v0=0;
    while(1)
            int v1=0--0;
    while(v0)
        break;
    while(-v0) {
        while(0+0+v0) continue;
        break;
    }
    if(-0!=-0+0+v0) while(0+0+0+0)
                break;
}
return 0!=0;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    parser.type_check(stop).unwrap();
    assert_eq!("Stop[ ]", parser.print(stop));
}

#[test]
fn test_fuzz6() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "
int v0=0;
while(0==1) while(v0)
        v0=1+v0;
                                   ",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    parser.type_check(stop).unwrap();
    assert_eq!("Stop[ ]", parser.print(stop));
}

#[test]
fn test_fuzz7() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "
while(1) {}
int v0=0;
while(v0)
    {}
int v1=0;
while(1)
        v1=1;
return v1+v0;
                                   ",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    parser.type_check(stop).unwrap();
    assert_eq!("Stop[ ]", parser.print(stop));
}

#[test]
fn test_fuzz8() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new("while(arg) arg = arg - 1; #showGraph; return arg;", &types);
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.show_graph();
    parser.type_check(stop).unwrap();
    assert_eq!("return Phi(Loop6,arg,(Phi_arg-1));", parser.print(stop));
}

#[test]
fn test_meet() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut t1 = types.ty_top;
    let mut t2 = types.ty_int_top;

    assert_eq!(types.ty_int_top.clone(), types.meet(t1, t2));
    assert_eq!(types.ty_int_top.clone(), types.meet(t2, t1));
    t1 = types.ty_bot;
    t2 = types.ty_int_bot;
    assert_eq!(types.ty_bot.clone(), types.meet(t1, t2));
    assert_eq!(types.ty_bot.clone(), types.meet(t2, t1));
}
