use crate::datastructures::arena::DroplessArena;
use crate::sea_of_nodes::parser::Parser;
use crate::sea_of_nodes::tests::evaluator::evaluate;
use crate::sea_of_nodes::tests::evaluator::Object;
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
fn test_float() {
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
fn test_square_root() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
flt guess = arg;
while( 1 ) {
    flt next = (arg/guess + guess)/2;
    if( next == guess ) break;
    guess = next;
}
return guess;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(
        parser.print(stop),
        "return Phi(Loop,(flt)arg,(((ToFloat/Phi_guess)+Phi_guess)/2.0));"
    );
    assert_eq!(
        Object::Double(3.0),
        evaluate(&parser.nodes, stop, Some(9), None).object
    );
    assert_eq!(
        Object::Double(1.414213562373095),
        evaluate(&parser.nodes, stop, Some(2), None).object
    );
}

#[test]
fn test_fpops() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
flt x = arg;
return x+1==x;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(parser.print(stop), "return ((flt)arg==(ToFloat+1.0));");
    assert_eq!(
        Object::Long(0),
        evaluate(&parser.nodes, stop, Some(1), None).object
    );
}
