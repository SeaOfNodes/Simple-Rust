use crate::datastructures::arena::DroplessArena;
use crate::sea_of_nodes::parser::Parser;
use crate::sea_of_nodes::tests::evaluator::{EResult, Evaluator, Heap, Object};
use crate::sea_of_nodes::types::Types;

fn assert_obj(heap: &Heap, obj: EResult, name: &str, fields: &[Object]) {
    let EResult::Value(Object::Obj(o)) = obj else {
        unreachable!()
    };
    assert_eq!(heap.objs[o].ty.name(), name);
    assert_eq!(heap.objs[o].fields, fields);
}

#[test]
fn test_store_in_if() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct S {
    int f;
}
S v0=new S;
if(arg) v0.f=1;
return v0;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    let mut eval = Evaluator::new(*stop, &parser.nodes);

    let result = eval.evaluate(0, 10);
    assert_obj(&eval.heap, result, "S", &[Object::Long(0)]);

    let result = eval.evaluate(1, 10);
    assert_obj(&eval.heap, result, "S", &[Object::Long(1)]);
}

#[test]
fn test_store_in_if_2() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(
        "\
struct S {
    int f;
}
S v=new S;
v.f = 2;
int i=new S.f;
i=v.f;
if (arg) v.f=1;
return i;
",
        &types,
    );
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    let mut eval = Evaluator::new(*stop, &parser.nodes);
    assert_eq!(eval.evaluate(0, 10), EResult::Value(Object::Long(2)));
    assert_eq!(eval.evaluate(0, 10), EResult::Value(Object::Long(2)));
}
