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
    if foo(io, true) {
        writeln!(io.stdout, "1 true").unwrap();
    } else {
        writeln!(io.stdout, "1 false").unwrap();
    }
    if foo(io, false) {
        writeln!(io.stdout, "2 true").unwrap();
    } else {
        writeln!(io.stdout, "2 false").unwrap();
    }
    return 0;
}

fn foo(io: &mut IO, first_parameter: bool) -> bool {
    writeln!(io.stdout, "begin foo").unwrap();
    if first_parameter {
        writeln!(io.stdout, "return false from foo").unwrap();
        return false;
    }
    writeln!(io.stdout, "return true from foo").unwrap();
    return true;
}
