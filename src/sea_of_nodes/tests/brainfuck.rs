use crate::datastructures::arena::DroplessArena;
use crate::sea_of_nodes::parser::Parser;
use crate::sea_of_nodes::tests::evaluator::evaluate;
use crate::sea_of_nodes::types::Types;

#[test]
fn brainfuck() {
    let program = b"++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.";

    use std::fmt::Write;
    let mut encoded = String::new();
    write!(&mut encoded, "u8[] !program = new u8[{}];", program.len()).unwrap();
    for (i, b) in program.iter().enumerate() {
        write!(&mut encoded, "program[{i}] = {b};").unwrap();
    }
    encoded.push_str(
        "\
var d = 0;
u8[] !output = new u8[0];
u8[] !data = new u8[100];

for( int pc = 0; pc < program#; pc++ ) {
    var command = program[pc];
    if (command == 62) {
        d++;
    } else if (command == 60) {
        d--;
    } else if (command == 43) {
        data[d]++;
    } else if (command == 45) {
        data[d]--;
    } else if (command == 46) {
        // Output a byte; increase the output array size
        var old = output;
        output = new u8[output# + 1];
        for( var i = 0; i < old#; i++ )
            output[i] = old[i];
        output[old#] = data[d]; // Add the extra byte on the end
    } else if (command == 44) {
        data[d] = 42;
    } else if (command == 91) {
        if (data[d] == 0) {
            for( var d = 1; d > 0; ) {
                command = program[++pc];
                if (command == 91) d++;
                if (command == 93) d--;
            }
        }
    } else if (command == 93) {
        if (data[d]) {
            for( var d = 1; d > 0; ) {
                command = program[--pc];
                if (command == 93) d++;
                if (command == 91) d--;
            }
        }
    }
}
return output;
",
    );

    let arena = DroplessArena::new();
    let types = Types::new(&arena);
    let mut parser = Parser::new(&encoded, &types);
    let stop = parser.parse().unwrap();
    parser.iterate(stop);
    parser.type_check(stop).unwrap();

    assert_eq!(
        "Hello World!\n",
        evaluate(&parser.nodes, stop, Some(0), Some(10000)).to_string()
    );
}
