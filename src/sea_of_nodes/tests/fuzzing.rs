use crate::sea_of_nodes::tests::test_print_stop;

/// This used to crash with index out of bounds in the graph visualizer,
/// because the parse_while changed the current scope but not the x_scope.
#[test]
fn test_xscope_after_while() {
    test_print_stop("while(0)break;#showGraph;", "Stop[ ]");
}