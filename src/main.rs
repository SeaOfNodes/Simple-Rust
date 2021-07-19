fn main() {
    let exit_code = {
        let mut args = std::env::args();
        let executable = args.next().unwrap();

        if let Some(file) = args.next() {
            ro::run(file.into(), args.collect())
        } else {
            eprintln!("Usage: {} build.ro [build_args...]", executable);
            ro::ExitCode::Usage
        }
    };

    std::process::exit(exit_code as i32);
}
