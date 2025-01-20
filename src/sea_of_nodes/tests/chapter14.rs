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
fn test_range() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int b;
if( arg ) b=1; else b=0;
int c = 99;
if( b < 0 ) c = -1;
if( b > 2 ) c =  1;
return c;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return 99;");
    assert_eq!(
        Object::Long(99),
        evaluate(&parser.nodes, stop, Some(0), None).object
    );
}

#[test]
fn test_u_8() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
u8 b = 123;
b = b + 456;// Truncate
return b;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return 67;");
    assert_eq!(
        Object::Long(67),
        evaluate(&parser.nodes, stop, Some(0), None).object
    );
}

#[test]
fn test_u_8_while() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
u8 b = 123;
while( b ) b = b + 456;// Truncate
return b;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return 0;");
}

#[test]
fn test_u_1() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
bool b = 123;
b = b + 456;// Truncate
u1 c = b;   // No more truncate needed
return c;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return 1;");
    assert_eq!(
        Object::Long(1),
        evaluate(&parser.nodes, stop, Some(0), None).object
    );
}

#[test]
fn test_and() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int b = 123;
b = b+456 & 31;                 // Precedence
return b;
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
fn test_ref_load() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct Foo { u1 b; };
Foo !f = new Foo;
f.b = 123;
return f.b;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return 1;");
    assert_eq!(
        Object::Long(1),
        evaluate(&parser.nodes, stop, Some(0), None).object
    );
}

#[test]
fn test_signed() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
i8 b = 255;                     // Chopped
return b;                       // Sign extend
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return -1;");
    assert_eq!(
        Object::Long(-1),
        evaluate(&parser.nodes, stop, Some(0), None).object
    );
}

#[test]
fn test_i_8() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
i8 b = arg;
b = b + 1;// Truncate
return b;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(
        parser.print(stop),
        "return (((((arg<<56)>>56)+1)<<56)>>56);"
    );
    assert_eq!(
        Object::Long(1),
        evaluate(&parser.nodes, stop, Some(0), None).object
    );
    assert_eq!(
        Object::Long(-128),
        evaluate(&parser.nodes, stop, Some(127), None).object
    );
}

#[test]
fn test_mask() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
u16 mask = (1<<16)-1;           // AND mask
int c = 123456789 & mask;
return c;                       //
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return 52501;");
    assert_eq!(
        Object::Long(52501),
        evaluate(&parser.nodes, stop, Some(0), None).object
    );
}

#[test]
fn test_or() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
return (arg | 123 ^ 456) >>> 1;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return (((arg|123)^456)>>>1);");
    assert_eq!(
        Object::Long(217),
        evaluate(&parser.nodes, stop, Some(0), None).object
    );
}

#[test]
fn test_mask_float() {
    test_error_iterate(
        "\
flt f = arg;
arg = f & 0;
return arg;
",
        "Cannot '&' FltBot",
    );
}

#[test]
fn test_clone_and() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int v0=0;
u32 v1 = 1&(1<<arg)&(1<<arg);
while(arg) v1=-v0;
while(v1) break;
return v1;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(
        parser.print(stop),
        "return (Phi(Loop,((1<<arg)&1),0)&Phi(Loop,Shl,4294967295));"
    );
    assert_eq!(
        Object::Long(1),
        evaluate(&parser.nodes, stop, Some(0), None).object
    );
}

#[test]
fn test_and_high() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int v0=0;
if(0&0>>>0) {
    while(0) {
        u8 v1=0;
        v0=0>>>0;
        v1=arg;
        while(v1+0) {}
    }
}
return v0;
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
fn test_types() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
i8  xi8  = 123456789;  if( xi8  !=        21 ) return -8;
i16 xi16 = 123456789;  if( xi16 !=    -13035 ) return -16;
i32 xi32 = 123456789;  if( xi32 != 123456789 ) return -32;
i64 xi64 = 123456789;  if( xi64 != 123456789 ) return -64;
int xint = 123456789;  if( xint != 123456789 ) return -64;

u1  ui1  = 123456789;  if( ui1  !=         1 ) return 1;
u8  ui8  = 123456789;  if( ui8  !=        21 ) return 8;
u16 ui16 = 123456789;  if( ui16 !=     52501 ) return 16;
u32 ui32 = 123456789;  if( ui32 != 123456789 ) return 32;

flt fflt = 3.141592653589793;  if( fflt != 3.141592653589793 ) return 3;
f64 ff64 = 3.141592653589793;  if( ff64 != 3.141592653589793 ) return 3;
f32 ff32 = 3.141592653589793;  if( ff32 != 3.1415927410125732) return 5;

return 0;
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
