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
    for _ in 0..10 {
        writeln!(io.stdout, "a").unwrap();
    }
    writeln!(io.stdout, "b").unwrap();
    return 64;
}
