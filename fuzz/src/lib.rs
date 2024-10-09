pub mod script_generator;

use simple_rust::datastructures::arena ::DroplessArena;
use simple_rust::sea_of_nodes::tests::evaluator;
use simple_rust::sea_of_nodes::tests::evaluator::EResult;
use simple_rust::sea_of_nodes::parser::Parser;
use simple_rust::sea_of_nodes::types::Types;

pub fn run_and_compare_eval(source: &str, definitely_valid: bool) {
    let arena1 = DroplessArena::new();
    let mut types1 = Types::new(&arena1);
    let mut parser1 = Parser::new(source, &mut types1);
    parser1.disable_show_graph_println = true;
    parser1.nodes.disable_peephole = true;
    let result1 = parser1.parse();

    let arena2 = DroplessArena::new();
    let mut types2 = Types::new(&arena2);
    let mut parser2 = Parser::new(source, &mut types2);
    parser2.disable_show_graph_println = true;
    parser2.nodes.disable_peephole = false;
    let result2 = parser2.parse();

    let (stop1, stop2) = match (result1, result2) {
        (Ok(stop1), Ok(stop2)) => (stop1, stop2),
        (r1, r2) => {
            assert_eq!(r1, r2);
            assert!(!definitely_valid);
            return;
        }
    };

    let timeout = 1000;
    for arg in [0, 1, 10] {
        let er1 = evaluator::evaluate_with_result(&parser1.nodes, stop1, arg, timeout);
        let er2 = evaluator::evaluate_with_result(&parser2.nodes, stop2, arg, timeout);
        if er1 == EResult::Timeout || er2 == EResult::Timeout {
            continue;
        }
        assert_eq!(er1, er2);
    }
}