use crate::datastructures::arena::DroplessArena;
use crate::sea_of_nodes::parser::Parser;
use crate::sea_of_nodes::tests::evaluator::{evaluate, Object};
use crate::sea_of_nodes::types::Types;

/// This used to crash with index out of bounds in the graph visualizer,
/// because the parse_while changed the current scope but not the x_scope.
#[test]
fn test_xscope_after_while() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new("while(0)break;#showGraph;", &types);
    let stop = parser.parse().unwrap();
    assert_eq!(parser.print(stop), "Stop[ ]");
}

#[test]
fn test_eval_negate_overflow() {
    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new("return -arg;", &types);
    let stop = parser.parse().unwrap();

    assert_eq!("return (-arg);", parser.print(stop));

    let nodes = parser.nodes;
    assert_eq!(Object::Long(-1), evaluate(&nodes, stop, Some(1), None).1);
    assert_eq!(
        Object::Long(i64::MIN),
        evaluate(&nodes, stop, Some(i64::MIN), None).1
    );
}
