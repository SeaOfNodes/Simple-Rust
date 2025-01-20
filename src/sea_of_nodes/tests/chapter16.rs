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
fn test_multi_0() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int x, y;
return x+y;
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
fn test_multi_1() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int x=2, y=x+1;
return x+y;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return 5;");
    assert_eq!(
        Object::Long(5),
        evaluate(&parser.nodes, stop, Some(0), None).object
    );
}

#[test]
fn test_final_1() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int x=2, y=3;
if( arg ) { int x = y; x = x*x; y=x; } // Shadow final x
return y;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return Phi(Region,9,3);");
    assert_eq!(
        Object::Long(3),
        evaluate(&parser.nodes, stop, Some(0), None).object
    );
    assert_eq!(
        Object::Long(9),
        evaluate(&parser.nodes, stop, Some(1), None).object
    );
}

#[test]
fn test_construct_0() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct X { int x=3; };
X z = new X;
return z.x;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return 3;");
    assert_eq!(
        Object::Long(3),
        evaluate(&parser.nodes, stop, Some(0), None).object
    );
}

#[test]
fn test_construct_1() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct X { int !x; };
X z = new X { x=3; };
return z.x;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return 3;");
    assert_eq!(
        Object::Long(3),
        evaluate(&parser.nodes, stop, Some(0), None).object
    );
}

#[test]
fn test_construct_2() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct X { int x=3; };
X z = new X { x = 4; };
return z.x;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return 4;");
    assert_eq!(
        Object::Long(4),
        evaluate(&parser.nodes, stop, Some(0), None).object
    );
}

#[test]
fn test_struct_final_0() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct Point { int !x, !y; };
Point p = new Point { x=3; y=4; };
return p;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return (const)Point;");
    assert_eq!(
        "Obj<Point>{x=3,y=4}",
        evaluate(&parser.nodes, stop, Some(0), None).to_string()
    );
}

#[test]
fn test_struct_final_1() {
    test_error_iterate(
        "\
struct Point { int x=3, y=4; };
Point p = new Point { x=5; y=6; };
p.x++;
return p;
",
        "Cannot modify final field 'x'",
    );
}

#[test]
fn test_struct_final_2() {
    test_error_iterate(
        "\
struct Point { int x=3, y=4; };
Point p = new Point;
p.x++;
return p;
",
        "Cannot modify final field 'x'",
    );
}

#[test]
fn test_struct_final_3() {
    test_error_iterate(
        "\
struct Point { var x; var y; };
Point p = new Point;
p.x++;
return p;
",
        "'Point' is not fully initialized, field 'x' needs to be set in a constructor",
    );
}

#[test]
fn test_struct_final_4() {
    test_error_iterate(
        "\
struct Point { val x=3; val y=4; };
Point p = new Point;
p.x++;
return p;
",
        "Cannot reassign final 'x'",
    );
}

#[test]
fn test_struct_final_5() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct Point { var x=3; var y=4; };
Point !p = new Point;
p.x++;
return p;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return Point;");
    assert_eq!(
        "Obj<Point>{x=4,y=4}",
        evaluate(&parser.nodes, stop, Some(0), None).to_string()
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
    head = new LLI { next=head; i=arg; };
    arg = arg-1;
}
if( !head ) return 0;
LLI? next = head.next;
if( !next ) return 1;
return next.i;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "Stop[ return 0; return 1; return .i; ]");
    assert_eq!(
        Object::Long(0),
        evaluate(&parser.nodes, stop, Some(0), None).object
    );
    assert_eq!(
        Object::Long(1),
        evaluate(&parser.nodes, stop, Some(1), None).object
    );
    assert_eq!(
        Object::Long(2),
        evaluate(&parser.nodes, stop, Some(3), None).object
    );
}

#[test]
fn test_linked_list_2() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct LLI { LLI? next; int i; };
LLI? !head = null;
while( arg ) {
    head = new LLI {
        next=head;
        // Any old code in the constructor
        int !tmp=arg;
        while( arg > 10 ) {
            tmp = tmp + arg;
            arg = arg - 1;
        }
        i=tmp;
    };
    arg = arg-1;
}
if( !head ) return 0;
LLI? next = head.next;
if( !next ) return 1;
return next.i;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "Stop[ return 0; return 1; return .i; ]");
    assert_eq!(
        Object::Long(0),
        evaluate(&parser.nodes, stop, Some(0), None).object
    );
    assert_eq!(
        Object::Long(1),
        evaluate(&parser.nodes, stop, Some(1), None).object
    );
    assert_eq!(
        Object::Long(2),
        evaluate(&parser.nodes, stop, Some(11), None).object
    );
}

#[test]
fn test_square() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct Square {
    flt !side = arg;
    // Newtons approximation to the square root, computed in a constructor.
    // The actual allocation will copy in this result as the initial
    // value for 'diag'.
    flt !diag = arg*arg/2;
    while( 1 ) {
        flt next = (side/diag + diag)/2;
        if( next == diag ) break;
        diag = next;
    }
};
return new Square;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return Square;");
    assert_eq!(
        "Obj<Square>{side=3.0,diag=1.7320508075688772}",
        evaluate(&parser.nodes, stop, Some(3), None).to_string()
    );
    assert_eq!(
        "Obj<Square>{side=4.0,diag=2.0}",
        evaluate(&parser.nodes, stop, Some(4), None).to_string()
    );
}
