use crate::datastructures::arena::DroplessArena;
use crate::sea_of_nodes::parser::Parser;
use crate::sea_of_nodes::tests::evaluator::evaluate;
use crate::sea_of_nodes::tests::evaluator::Object;
use crate::sea_of_nodes::tests::test_error_iterate;
use crate::sea_of_nodes::types::Types;

#[test]
fn test_jig() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
return 3.14;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return 3.14;");
    assert_eq!(
        Object::Double(3.14),
        evaluate(&parser.nodes, stop, Some(0), None).object
    );
}

#[test]
fn test_linked_list_0() {
    test_error_iterate(
        "\
struct LLI { LLI? next; int i; };
LLI? !head = null;
while( arg ) {
    LLI !x = new LLI;
    x.next = head;
    x.i = arg;
    head = x;
    arg = arg-1;
}
return head.next.i;
",
        "Might be null accessing 'i'",
    );
}

#[test]
fn test_linked_list_1() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct LLI { LLI? next; int i; };
LLI? !head = null;
while( arg ) {
    LLI !x = new LLI;
    x.next = head;
    x.i = arg;
    head = x;
    arg = arg-1;
}
if( !head ) return 0;
LLI? next = head.next;
if( next==null ) return 1;
return next.i;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "Stop[ return 0; return 1; return .i; ]");
    assert_eq!(
        Object::Long(2),
        evaluate(&parser.nodes, stop, Some(3), None).object
    );
}

#[test]
fn test_co_recur() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct int0 { int i; flt0? f; };
struct flt0 { flt f; int0? i; };
int0 !i0 = new int0;
i0.i = 17;
flt0 !f0 = new flt0;
f0.f = 3.14;
i0.f = f0;
f0.i = i0;
return f0.i.f.i.i;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return 17;");
}

#[test]
fn test_null_ref_0() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct N { N? next; int i; };
N n = new N;
return n.next;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return null;");
}

#[test]
fn test_null_ref_1() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct M { int m; };
struct N { M next; int i; };
N n = new N { next = new M; };
return n.next;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return (const)M;");
}

#[test]
fn test_null_ref_2() {
    test_error_iterate(
        "\
struct M { int m; };
struct N { M next; int i; };
N n = new N { next = null; }
return n.next;
",
        "Type null is not of declared type *M",
    );
}

#[test]
fn test_null_ref_3() {
    test_error_iterate(
        "\
struct N { N? next; int i; };
N !n = new N;
n.i = 3.14;
return n.i;
",
        "Type 3.14 is not of declared type int",
    );
}

#[test]
fn test_empty() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct S{};
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return 0;");
    assert_eq!(
        Object::Long(0),
        evaluate(&parser.nodes, stop, Some(0), None).object
    );
}

#[test]
fn test_forward_ref_0() {
    test_error_iterate(
        "\
struct S1 { S2? s; };
return new S2;
",
        "Unknown struct type 'S2'",
    );
}

#[test]
fn test_forward_ref_1() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct S1 { S2? s; };
struct S2 { int x; };
return new S1.s=new S2;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return S2;");
}

#[test]
fn testcheck_null() {
    test_error_iterate(
        "\
struct I {int i;};
struct P { I? pi; };
P !p1 = new P;
P p2 = new P;
p2.pi = new I;
p2.pi.i = 2;
if (arg) p1 = new P;
return p1.pi.i + 1;
",
        "Might be null accessing 'i'",
    );
}
