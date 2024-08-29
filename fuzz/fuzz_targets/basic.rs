#![no_main]

use libfuzzer_sys::fuzz_target;
use simple_rust::datastructures::arena ::DroplessArena;
use simple_rust::sea_of_nodes::parser::Parser;
use simple_rust::sea_of_nodes::types::Types;

fuzz_target!(|source: &str| {
    let arena = DroplessArena::new();
    let mut types = Types::new(&arena);
    let mut parser = Parser::new(source, &mut types);
    parser.disable_show_graph_println = true;
    let _ = parser.parse();
});
