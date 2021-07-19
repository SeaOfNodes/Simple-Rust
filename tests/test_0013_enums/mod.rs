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
    writeln!(io.stdout, "first").unwrap();
    writeln!(io.stdout, "second").unwrap();
    return 5;
}
