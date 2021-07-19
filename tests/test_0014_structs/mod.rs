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

#[derive(Clone)]
struct Tst<'a> {
    x: i32,
    y: &'a str,
}

fn main(io: &mut IO) -> i32 {
    let mut t: Tst = new_test("initial");

    print_by_value(io, t.clone(), 3);
    t.x = t.x + 1;
    print_by_value(io, t.clone(), 4);
    print_by_pointer(io, &mut t, 4);
    print_by_value(io, t.clone(), 5);

    return t.x;
}

fn new_test(y: &str) -> Tst {
    Tst { x: 3, y }
}

fn print_by_value(io: &mut IO, t: Tst, expected: i32) {
    writeln!(io.stdout, "{}", t.y).unwrap();
    if t.x == expected {
        writeln!(io.stdout, "ok").unwrap();
    }
}

fn print_by_pointer(io: &mut IO, t: &mut Tst, expected: i32) {
    writeln!(io.stdout, "{}", t.y).unwrap();
    if t.x == expected {
        writeln!(io.stdout, "ok").unwrap();
    }
    t.x = t.x + 1;
    t.y = "modified";
}
