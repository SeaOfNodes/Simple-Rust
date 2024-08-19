#![no_main]

use libfuzzer_sys::fuzz_target;
use simple_rust::datastructures::arena::Arena;
use simple_rust::sea_of_nodes::graph_evaluator;
use simple_rust::sea_of_nodes::graph_evaluator::EResult;
use simple_rust::sea_of_nodes::parser::Parser;
use simple_rust::sea_of_nodes::types::Types;

fuzz_target!(|source: &str| {
    let arena1 = Arena::new();
    let mut types1 = Types::new(&arena1);
    let mut parser1 = Parser::new(source, &mut types1);
    parser1.disable_show_graph_println = true;
    parser1.nodes.disable_peephole = true;
    let result1 = parser1.parse();

    let arena2 = Arena::new();
    let mut types2 = Types::new(&arena2);
    let mut parser2 = Parser::new(source, &mut types2);
    parser2.disable_show_graph_println = true;
    parser2.nodes.disable_peephole = false;
    let result2 = parser2.parse();

    let (stop1, stop2) = match (result1, result2) {
        (Ok(stop1), Ok(stop2)) => (stop1, stop2),
        (r1,r2) => return assert_eq!(r1, r2)
    };

    let timeout = 1000;
    for arg in [0, 1, 10] {
        let er1 = graph_evaluator::evaluate_with_result(&parser1.nodes, stop1, arg, timeout);
        let er2 = graph_evaluator::evaluate_with_result(&parser2.nodes, stop2, arg, timeout);
        if er1 == EResult::Timeout || er2 == EResult::Timeout {
            continue;
        }
        assert_eq!(er1, er2);
    }

});
