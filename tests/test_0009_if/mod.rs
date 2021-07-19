use std::io::Write;

use crate::{Cmd, ExitCode, IO, test, Test};

#[test]
fn run() {
    test(Test {
        directory: directory!(),
        log: relative_path!("log.txt"),
        build_args: vec!["main.ro".to_string()],
        compiler_status: ExitCode::Success,
        commands: vec![
            Cmd {
                file: relative_path!("main"),
                args: vec![],
                stdin: vec![],
                reference: &main,
            }
        ],
    });
}

fn main(io: &mut IO) -> i32 {
    writeln!(io.stdout, "if").unwrap();
    foo(io, false, false);
    foo(io, false, true);
    foo(io, true, false);
    foo(io, true, true);
    0
}

fn foo(io: &mut IO, first_parameter: bool, second_parameter: bool) {
    writeln!(io.stdout, "foo:").unwrap();
    if first_parameter {
        if second_parameter {
            writeln!(io.stdout, "both are true").unwrap();
        } else {
            writeln!(io.stdout, "only the first one is true").unwrap();
        }
        writeln!(io.stdout, "first_parameter was true").unwrap();
    } else if second_parameter {
        writeln!(io.stdout, "only the second parameter is true").unwrap();
    } else {
        writeln!(io.stdout, "both parameters are false").unwrap();
    }
    writeln!(io.stdout, ":oof").unwrap();
}
