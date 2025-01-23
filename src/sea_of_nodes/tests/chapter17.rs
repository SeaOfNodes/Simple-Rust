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
int i = 0;
i=i=1;
return i;
                                   //return 3.14;
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
fn test_inc_0() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
return arg++;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return arg;");
    assert_eq!(
        Object::Long(0),
        evaluate(&parser.nodes, stop, Some(0), None).object
    );
}

#[test]
fn test_inc_1() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
return arg+++arg++;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return ((arg*2)+1);");
    assert_eq!(
        Object::Long(1),
        evaluate(&parser.nodes, stop, Some(0), None).object
    );
}

#[test]
fn test_inc_2() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
//   -(arg--)-(arg--)
return -arg---arg--;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return (-((arg*2)+-1));");
    assert_eq!(
        Object::Long(1),
        evaluate(&parser.nodes, stop, Some(0), None).object
    );
}

#[test]
fn test_inc_3() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int[] !xs = new int[arg];
xs[0]++;
xs[1]++;
return xs[0];
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return 1;");
    assert_eq!(
        Object::Long(1),
        evaluate(&parser.nodes, stop, Some(2), None).object
    );
}

#[test]
fn test_inc_4() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
u8[] !xs = new u8[1];
xs[0]--;
return xs[0];
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return 255;");
    assert_eq!(
        Object::Long(255),
        evaluate(&parser.nodes, stop, Some(2), None).object
    );
}

#[test]
fn test_inc_5() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct S { u16 x; };
S !s = new S;
s.x--;
return s.x;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return 65535;");
    assert_eq!(
        Object::Long(65535),
        evaluate(&parser.nodes, stop, Some(2), None).object
    );
}

#[test]
fn test_inc_6() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new("return --arg;", &types);
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return (arg+-1);");
    assert_eq!(
        Object::Long(-1),
        evaluate(&parser.nodes, stop, Some(0), None).object
    );
}

#[test]
fn test_inc_7() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new("u8 x=0; return --x;", &types);
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return 255;");
    assert_eq!(
        Object::Long(255),
        evaluate(&parser.nodes, stop, Some(0), None).object
    );
}

#[test]
fn test_inc_8() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new("int x; x+=2; return x+=3;", &types);
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
fn test_var_0() {
    test_error_iterate("var d; return d;", "Syntax error, expected =expression: ;");
}

#[test]
fn test_var_1() {
    test_error_iterate("val d; return d;", "Syntax error, expected =expression: ;");
}

#[test]
fn test_var_2() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "int x; x=3; x++; return x; // Ok, no initializer so x is mutable ",
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
fn test_var_3() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "int x=3; x++; return x; // Ok, primitive so x is mutable despite initializer ",
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
fn test_var_4() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "struct S{int x;}; S s; s=new S; s.x++; return s.x; // Ok, no initializer so x is mutable ",
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
fn test_var_5() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new("struct S{int x;}; S s; s=new S{x=3;}; s.x++; return s.x; // Ok, no initializer so x is mutable ", &types);
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
fn test_var_6() {
    test_error_iterate(
        "struct S{int x;}; S s=new S; s.x++; return s.x; // Error initializer so x is immutable ",
        "Cannot modify final field 'x'",
    );
}

#[test]
fn test_var_7() {
    test_error_iterate("struct S{int x;}; S s=new S{x=3;}; s.x++; return s.x; // Error initializer so x is immutable ", "Cannot modify final field 'x'");
}

#[test]
fn test_var_8() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "struct S{int x;}; S !s=new S; s.x++; return s.x; // Ok, has '!' so s.x is mutable ",
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
fn test_var_9() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "struct S{int x;}; var s=new S; s.x++; return s.x; // Ok, has var so s.x is mutable ",
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
fn test_var_10() {
    test_error_iterate(
        "struct S{int x;}; val s=new S; s.x++; return s.x; // Error, has val so x is immutable ",
        "Cannot modify final field 'x'",
    );
}

#[test]
fn test_var_11() {
    test_error_iterate(
        "\
struct Bar { int x; };
Bar !bar = new Bar;
bar.x = 3; // Ok, bar is mutable

struct Foo { Bar? !bar; int y; };
Foo !foo = new Foo { bar = bar; };
foo.bar = bar; // Ok foo is mutable
foo.bar.x++;   // Ok foo and foo.bar and foo.bar.x are all mutable

val xfoo = foo; // Throw away mutability
xfoo.bar.x++;   // Error, cannot mutate through xfoo
",
        "Cannot modify final field 'x'",
    );
}

#[test]
fn test_var_12() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct Bar { int x; };
Bar !bar = new Bar;
bar.x = 3; // Ok, bar is mutable

struct Foo { Bar? !bar; int y; };
Foo !foo = new Foo;
foo.bar = bar; // Ok bar is mutable
foo.bar.x++;   // Ok foo and foo.bar and foo.bar.x are all mutable

val xfoo = foo;        // Throw away mutability
int x4 = xfoo.bar.x;   // Ok to read through xfoo, gets 4
foo.bar.x++;           // Bumps to 5
int x5 = xfoo.bar.x;   // Ok to read through xfoo, gets 5
return x4*10+x5;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return 45;");
    assert_eq!(
        Object::Long(45),
        evaluate(&parser.nodes, stop, Some(0), None).object
    );
}

#[test]
fn test_var_13() {
    test_error_iterate(
        "\
int i,i++;
",
        "Redefining name 'i'",
    );
}

#[test]
fn test_var_14() {
    test_error_iterate(
        "\
struct B {};
struct A { B b; };
A x = new A {
    return b; // read before init
    b = new B;
};
",
        "Cannot read uninitialized field 'b'",
    );
}

#[test]
fn test_var_15() {
    test_error_iterate(
        "\
struct B {};
struct A { B b; };
return new A {
    if (arg) b = new B; // Constructor ends with partial init of b
}.b;
",
        "'A' is not fully initialized, field 'b' needs to be set in a constructor",
    );
}

#[test]
fn test_trinary_0() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
return arg ? 1 : 2;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return Phi(Region,1,2);");
    assert_eq!(
        Object::Long(2),
        evaluate(&parser.nodes, stop, Some(0), None).object
    );
    assert_eq!(
        Object::Long(1),
        evaluate(&parser.nodes, stop, Some(1), None).object
    );
}

#[test]
fn test_trinary_1() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
return arg ? 0 : arg;
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
fn test_trinary_2() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct Bar { int x; };
var b = arg ? new Bar : null;
return b ? b.x++ + b.x++ : -1;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return Phi(Region,1,-1);");
    assert_eq!(
        Object::Long(-1),
        evaluate(&parser.nodes, stop, Some(0), None).object
    );
    assert_eq!(
        Object::Long(1),
        evaluate(&parser.nodes, stop, Some(1), None).object
    );
}

#[test]
fn test_trinary_3() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct Bar { int x; };
var b = arg ? new Bar;
return b ? b.x++ + b.x++ : -1;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return Phi(Region,1,-1);");
    assert_eq!(
        Object::Long(-1),
        evaluate(&parser.nodes, stop, Some(0), None).object
    );
    assert_eq!(
        Object::Long(1),
        evaluate(&parser.nodes, stop, Some(1), None).object
    );
}

#[test]
fn test_trinary_4() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct Bar { Bar? next; int x; };
var b = arg ? new Bar { next = (arg==2) ? new Bar{x=2;}; x=1; };
return b ? b.next ? b.next.x : b.x; // parses \"b ? (b.next ? b.next.x : b.x) : 0\"
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(
        parser.print(stop),
        "return Phi(Region,Phi(Region,.x,.x),0);"
    );
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
        evaluate(&parser.nodes, stop, Some(2), None).object
    );
}

#[test]
fn test_trinary_5() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
flt f=arg?1:1.2;
return f;   // missing widening
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return Phi(Region,1.0,1.2);");
    assert_eq!(
        Object::Double(1.2),
        evaluate(&parser.nodes, stop, Some(0), None).object
    );
}

#[test]
fn test_for_0() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int sum=0;
for( int i=0; i<arg; i++ )
    sum += i;
return sum;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(
        parser.print(stop),
        "return Phi(Loop,0,(Phi_sum+Phi(Loop,0,(Phi_i+1))));"
    );
    assert_eq!(
        Object::Long(3),
        evaluate(&parser.nodes, stop, Some(3), None).object
    );
    assert_eq!(
        Object::Long(45),
        evaluate(&parser.nodes, stop, Some(10), None).object
    );
}

#[test]
fn test_for_1() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int sum=0, i=0;
for( ; i<arg; i++ )
    sum += i;
return sum;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(
        parser.print(stop),
        "return Phi(Loop,0,(Phi_sum+Phi(Loop,0,(Phi_i+1))));"
    );
    assert_eq!(
        Object::Long(3),
        evaluate(&parser.nodes, stop, Some(3), None).object
    );
    assert_eq!(
        Object::Long(45),
        evaluate(&parser.nodes, stop, Some(10), None).object
    );
}

#[test]
fn test_for_2() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int sum=0;
for( int i=0; ; i++ ) {
    if( i>=arg ) break;
    sum += i;
}
return sum;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(
        parser.print(stop),
        "return Phi(Loop,0,(Phi_sum+Phi(Loop,0,(Phi_i+1))));"
    );
    assert_eq!(
        Object::Long(3),
        evaluate(&parser.nodes, stop, Some(3), None).object
    );
    assert_eq!(
        Object::Long(45),
        evaluate(&parser.nodes, stop, Some(10), None).object
    );
}

#[test]
fn test_for_3() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int sum=0;
for( int i=0; i<arg; )
    sum += i++;
return sum;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(
        parser.print(stop),
        "return Phi(Loop,0,(Phi_sum+Phi(Loop,0,(Phi_i+1))));"
    );
    assert_eq!(
        Object::Long(3),
        evaluate(&parser.nodes, stop, Some(3), None).object
    );
    assert_eq!(
        Object::Long(45),
        evaluate(&parser.nodes, stop, Some(10), None).object
    );
}

#[test]
fn test_for_4() {
    test_error_iterate(
        "\
int sum=0;
for( int i=0; i<arg; i++ )
    sum += i;
return i;
",
        "Undefined name 'i'",
    );
}

#[test]
fn test_for_5() {
    test_error_iterate(
        "\
for(;;arg++;) {}
",
        "Syntax error, expected Unexpected code after expression: ;",
    );
}

#[test]
fn test_forward_0() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct A{
    B? f1;
    B? f2;
};
return new A;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return A;");
    assert_eq!(
        "Obj<A>{f1=null,f2=null}",
        evaluate(&parser.nodes, stop, None, None).to_string()
    );
}

#[test]
fn test_forward_1() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct A{
    B?[]? nil_array_of_b;
    B?[]  not_array_of_b = new B?[0];
};
return new A.not_array_of_b;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return (const)[*B?];");
    assert_eq!(
        "Obj<[*B?]>{#=0,[]=[]}",
        evaluate(&parser.nodes, stop, None, None).to_string()
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
while( arg-- )
    head = new LLI { next=head; i=arg; };
int sum=0;
var ptr = head; // A read-only ptr, to be assigned from read-only next fields
for( ; ptr; ptr = ptr.next )
    sum += ptr.i;
return sum;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return Phi(Loop,0,(Phi_sum+.i));");
    assert_eq!(
        Object::Long(45),
        evaluate(&parser.nodes, stop, Some(10), None).object
    );
}

#[test]
fn sieve_of_eratosthenes() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
var ary = new bool[arg], primes = new int[arg];
var nprimes=0, p=0;
// Find primes while p^2 < arg
for( p=2; p*p < arg; p++ ) {
    // skip marked non-primes
    while( ary[p] ) p++;
    // p is now a prime
    primes[nprimes++] = p;
    // Mark out the rest non-primes
    for( int i = p + p; i < ary#; i += p )
        ary[i] = true;
}
// Now just collect the remaining primes, no more marking
for( ; p < arg; p++ )
    if( !ary[p] )
        primes[nprimes++] = p;
// Copy/shrink the result array
var !rez = new int[nprimes];
for( int j=0; j<nprimes; j++ )
    rez[j] = primes[j];
return rez;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return [int];");

    let result = evaluate(&parser.nodes, stop, Some(20), None);
    let Object::Obj(obj) = result.object else {
        panic!()
    };
    assert_eq!(
        "[int] {int #; int ![]; }",
        result.heap.objs[obj].ty.to_string()
    );

    let Object::Long(nprimes) = result.heap.objs[obj].fields[0] else {
        panic!()
    };
    let primes = [2, 3, 5, 7, 11, 13, 17, 19];
    assert_eq!(nprimes as usize, primes.len());
    assert_eq!(
        primes.map(|p| Object::Long(p)),
        result.heap.objs[obj].fields[1..]
    );
}
