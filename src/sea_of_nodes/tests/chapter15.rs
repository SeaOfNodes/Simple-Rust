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
fn test_cyclic() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct C { C? l; };
C !c = new C;
c.l = c;
return c;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return C;");
    assert_eq!(
        "Obj<C>@1{l=obj@1}",
        evaluate(&parser.nodes, stop, Some(0), None).to_string()
    );
}

#[test]
fn test_safety_check() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
u8[] old = new u8[0];
u8[] !output = new u8[1];
int i = 0;
while (i < old#) {
    output[i] = old[i];
    i = i + 1;
}
output[i] = 1;
return output;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return [u8];");
    assert_eq!(
        "",
        evaluate(&parser.nodes, stop, Some(0), None).to_string()
    );
}

#[test]
fn test_basic_1() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int[] is = new int[2];
return is[1];
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
fn test_basic_2() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int[] is = new int[2];
int[] is2 = new int[2];
return is[1];
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
fn test_basic_3() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int[] !a = new int[2];
a[0] = 1;
a[1] = 2;
return a[0];
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
fn test_basic_4() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct A { int i; };
A?[] !a = new A?[2];
return a;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return [*A?];");
    assert_eq!(
        "Obj<[*A?]>{#=2,[]=[null,null]}",
        evaluate(&parser.nodes, stop, Some(0), None).to_string()
    );
}

#[test]
fn test_basic_5() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct S { int x; flt y; };
// A new S
S !s = new S; s.x=99; s.y = 3.14;

// Double-d array of Ss.  Fill in one row.
S?[]?[] !iss = new S?[]?[2];
iss[0] = new S?[7];
iss[0][2] = s;

// Now pull out the filled-in value, with null checks
flt rez;
S?[]? is = iss[arg];
if( !is ) rez = 1.2;
else {
    S? i = is[2];
    if( !i ) rez = 2.3;
    else rez = i.y;
}
return rez;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(
        parser.print(stop),
        "return Phi(Region,1.2,Phi(Region,2.3,3.14));"
    );
    assert_eq!(
        Object::Double(3.14),
        evaluate(&parser.nodes, stop, Some(0), None).object
    );
    assert_eq!(
        Object::Double(1.2),
        evaluate(&parser.nodes, stop, Some(1), None).object
    );
}

#[test]
fn test_basic_6() {
    test_error_iterate(
        "\
struct S { int x; flt y; };
// A new S
S !s = new S; s.x=99; s.y = 3.14;

// Double-d array of Ss.  Fill in one row.
S?[]?[] iss = new S?[]?[2];
iss[0] = new S?[7];
iss[0][2] = s;

// Now pull out the filled-in value, with null checks
flt rez = 1.2;
if( iss[arg] )
    if( iss[arg][2] )
        rez = iss[arg][2].y;
return rez;
",
        "Might be null accessing 'y'",
    );
}

#[test]
fn test_tree() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
// Can we define a forward-reference array?
struct Tree { Tree?[]? _kids; };
Tree !root = new Tree;
root._kids = new Tree?[2]; // NO BANG SO ARRAY IS OF IMMUTABLE TREES????
root._kids[0] = new Tree;
return root;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return Tree;");
    assert_eq!(
        "Obj<Tree>{_kids=Obj<[*Tree?]>{#=2,[]=[Obj<Tree>{_kids=null},null]}}",
        evaluate(&parser.nodes, stop, Some(0), None).to_string()
    );
}

#[test]
fn test_nested_struct_add_mem_proj() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct S { int a; int[] b; };
return 0;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    assert_eq!(parser.print(stop), "return 0;");
}

#[test]
fn test_rolling_sum() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int[] !ary = new int[arg];
// Fill [0,1,2,3,4,...]
int i=0;
while( i < ary# ) {
    ary[i] = i;
    i = i+1;
}
// Fill [0,1,3,6,10,...]
i=0;
while( i < ary# - 1 ) {
    ary[i+1] = ary[i+1] + ary[i];
    i = i+1;
}
return ary[1] * 1000 + ary[3]; // 1 * 1000 + 6
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return (.[]+(.[]*1000));");
    assert_eq!(
        Object::Long(1006),
        evaluate(&parser.nodes, stop, Some(4), None).object
    );
}

#[test]
fn sieve_oeratosthenes() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int[] !ary = new int[arg], !primes = new int[arg];
int nprimes = 0, p=2;
// Find primes while p^2 < arg
while( p*p < arg ) {
    // skip marked non-primes
    while( ary[p]==1 ) p = p + 1;
    // p is now a prime
    primes[nprimes] = p;  nprimes = nprimes + 1;
    // Mark out the rest non-primes
    int i = p + p;
    while( i < ary# ) {
        ary[i] = 1;
        i = i + p;
    }
    p = p + 1;
}
// Now just collect the remaining primes, no more marking
while( p < arg ) {
    if( ary[p] == 0 ) {
        primes[nprimes] = p;  nprimes = nprimes + 1;
    }
    p = p + 1;
}
// Copy/shrink the result array
int[] !rez = new int[nprimes];
int j = 0;
while( j < nprimes ) {
    rez[j] = primes[j];
    j = j + 1;
}
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

#[test]
fn test_new_node_init() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct S {int i; flt f;};
S !s1 = new S;
S !s2 = new S;
s2.i = 3;
s2.f = 2.0;
if (arg) s1 = new S;
return s1.i + s1.f;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return ((flt).i+.f);");
    assert_eq!(
        Object::Double(0.0),
        evaluate(&parser.nodes, stop, Some(0), None).object
    );
}

#[test]
fn test_bad_0() {
    test_error_iterate(
        "\
return new flt;
",
        "Cannot allocate a flt",
    );
}

#[test]
fn test_bad_1() {
    test_error_iterate(
        "\
int is = new int[2];
",
        "Type *[int] is not of declared type int",
    );
}

#[test]
fn test_bad_2() {
    test_error_iterate(
        "\
int[] is = new int[3.14];
return is[1];
",
        "Cannot allocate an array with length 3.14",
    );
}

#[test]
fn test_bad_3() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
int[] is = new int[arg];
return is[1];
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return 0;");
    assert_eq!(
        Object::Long(0),
        evaluate(&parser.nodes, stop, Some(4), None).object
    );

    // TODO find a better solution that is actually safe
    use std::panic;
    let sea = &parser.nodes;
    // TODO java only passes this test, becasue it doens't actually throw
    // assert_eq!(
    //     "ArrayIndexOutOfBoundsException(Array index 1 out of bounds for array length 0)",
    //     panic::catch_unwind(panic::AssertUnwindSafe(move || evaluate(sea, stop, Some(0), None)))
    //         .unwrap_err()
    //         .downcast::<String>()
    //         .unwrap()
    //         .as_str()
    // );
    assert_eq!(
        "NegativeArraySizeException(-1)",
        panic::catch_unwind(panic::AssertUnwindSafe(move || evaluate(
            sea,
            stop,
            Some(-1),
            None
        )))
        .unwrap_err()
        .downcast::<String>()
        .unwrap()
        .as_str()
    );
}

#[test]
fn test_progress() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
i8 v1=0&0;
u8 v2=0;
byte v4=0;
if(0) {}
while(v2<0) {
    v4=0-v1;
    break;
}
int v5=0&0;
while(v5+(0&0)) {
    int v7=0&0;
    while(v7)
        v4=0>>>v5;
    while(v1)
        return 0;
}
return v1;
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
fn test_sharp_not() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
if(0>>0) {}
while(0) {}
u32 v7=0;
int v8=0;
while(0<--1>>>---(v7*0==v8)) {}
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return 0;");
}

#[test]
fn test_progress_2() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
if(1) {}
else {
        while(arg>>>0&0>>>0) {}
    byte v3=0>>>0;
                while(0) {}
        int v7=0>>>0;
        while(v7<0>>>0) {
                    while(0+v7<=0) if(1) arg=-12;
            if(arg) {
                v3=arg+v3+0;
                arg=0;
            }
        }
}
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return 0;");
}
