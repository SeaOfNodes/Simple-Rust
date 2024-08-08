use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

pub use ro::ExitCode;

macro_rules! directory {
    () => {{
        let mut path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        path.push(file!());
        path.pop();
        path
    }};
}

macro_rules! relative_path {
    ($e:expr) => {{
        let mut path = directory!();
        path.push($e);
        path
    }};
}

// mod test_0001_return_0;
// mod test_0002_return_42;
// mod test_0003_return_addition;
// mod test_0004_function_call;
// mod test_0005_two_statements;
// mod test_0006_params_and_args;
// mod test_0007_call_putchar;
// mod test_0008_puts_string;
// mod test_0009_if;
// mod test_0010_conditional_return;
// mod test_0011_variable;
// mod test_0012_pointer;
// mod test_0013_enums;

struct Test<'a> {
    directory: PathBuf,
    log: PathBuf,
    build_args: Vec<String>,
    compiler_status: ExitCode,
    commands: Vec<Cmd<'a>>,
}

impl Test<'_> {
    fn expected_log(&self) -> PathBuf {
        self.log.with_extension("expected.txt")
    }
}

struct Cmd<'a> {
    file: PathBuf,
    args: Vec<String>,
    stdin: Vec<u8>,
    reference: &'a dyn Fn(&mut IO) -> i32,
}

struct IO {
    args: Vec<String>,
    stdin: Vec<u8>,
    stdout: Vec<u8>,
    stderr: Vec<u8>,
}

/// Compile main once and verify that all commands match the reference
/// implementation. The log file makes it easier to see what the test
/// does and it makes changes of the compiler output visible in version
/// control. Tests do not fail if the log file changes.
fn test(test: Test) {
    // :0 is so that intellij makes it a clickable link
    println!("{}:0", test.log.to_str().unwrap());

    let expected_log = test.expected_log();
    let mut log = Log::create(&test.log, &expected_log).unwrap();
    log.path(DIRECTORY, &test.directory);

    let mut success;
    {
        // compile in debugger
        let status = if test.build_args.len() > 0 {
            delete_files(&test);
            // NOTE: we can't change the current working directory because the tests run in parallel
            let main = {
                let mut path = test.directory.clone();
                path.push(&test.build_args[0]);
                path
            };
            ro::run(main, test.build_args.clone())
        } else { ExitCode::Usage };

        // compile again in separate process and capture output
        delete_files(&test);
        let compiler = Path::new(env!("CARGO_BIN_EXE_ro"));
        let output = Command::new(compiler)
            .args(&test.build_args)
            .current_dir(&test.directory)
            .output()
            .unwrap();

        log.separator();
        log.command(filename_str(&compiler), &test.build_args);
        log.status(STATUS, output.status.code());
        log.io(STDOUT, &output.stdout);
        log.io(STDERR, &output.stderr);

        success = status == test.compiler_status;
        assert_eq!(status as i32, output.status.code().unwrap());
    };

    for command in &test.commands {
        log.separator();
        log.command(relative_path_str(&test.directory, &command.file), &command.args);
        log.io(STDIN, &command.stdin);

        let mut reference_io = IO {
            args: command.args.clone(),
            stdin: command.stdin.clone(),
            stdout: vec![],
            stderr: vec![],
        };
        let reference_status: i32 = (&command.reference)(&mut reference_io);

        log.to(false, true);
        log.status(STATUS, Some(reference_status));
        log.io(STDOUT, &reference_io.stdout);
        log.io(STDERR, &reference_io.stderr);

        log.to(true, false);

        if command.file.exists() {
            let child = Command::new(&command.file)
                .args(&command.args)
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .current_dir(&test.directory)
                .spawn()
                .unwrap();
            child.stdin.as_ref().unwrap().write_all(&command.stdin).unwrap();

            let output = child.wait_with_output().unwrap();

            log.status(STATUS, output.status.code());
            if Some(reference_status) != output.status.code() {
                success = false;
            }

            log.io(STDOUT, &output.stdout);
            if reference_io.stdout != output.stdout {
                success = false;
            }

            log.io(STDERR, &output.stderr);
            if reference_io.stderr != output.stderr {
                success = false;
            }
        } else {
            log.io(ERROR, "Executable file doesn't exist!".as_bytes());
            success = false;
        }

        log.to(true, true)
    }

    drop(log);

    if success {
        std::fs::remove_file(&expected_log).unwrap();
        delete_files(&test);
    } else {
        // Intellij provides a nice clickable link to view the diff:
        assert_eq!(std::fs::read_to_string(&test.log).unwrap(), std::fs::read_to_string(&expected_log).unwrap());
        unreachable!();
    }
}

fn delete_files(test: &Test) {
    for command in &test.commands {
        let _ = std::fs::remove_file(&command.file);
    }
}

fn relative_path_str<P>(base: P, path: &Path) -> &str
    where P: AsRef<Path>
{
    let stripped = path.strip_prefix(base).unwrap();
    return stripped.to_str().unwrap();
}

fn filename_str(path: &Path) -> &str {
    let filename = path.file_name().unwrap();
    return filename.to_str().unwrap();
}

const DIRECTORY: &str = "directory: ";
const COMMAND: &str = "command: ";
const STATUS: &str = "status: ";
const STDIN: &str = "stdin: ";
const STDOUT: &str = "stdout: ";
const STDERR: &str = "stderr: ";
const ERROR: &str = "error: ";

struct Log {
    actual: bool,
    expected: bool,
    actual_write: BufWriter<File>,
    expected_write: BufWriter<File>,
}

impl Log {
    fn create(path: &Path, path_expected: &Path) -> std::io::Result<Log> {
        Ok(Self {
            actual: true,
            expected: true,
            actual_write: BufWriter::new(File::create(path)?),
            expected_write: BufWriter::new(File::create(path_expected)?),
        })
    }

    fn to(&mut self, actual: bool, expected: bool) {
        self.actual = actual;
        self.expected = expected;
    }

    fn path(&mut self, prefix: &str, path: &Path) {
        writeln!(self, "{}{}", prefix, relative_path_str(env!("CARGO_MANIFEST_DIR"), path)).unwrap();
    }

    fn separator(&mut self) {
        writeln!(self).unwrap();
    }

    fn command(&mut self, executable: &str, args: &[String]) {
        write!(self, "{}{}", COMMAND, executable).unwrap();
        for arg in args {
            write!(self, " {:?}", arg).unwrap();
        }
        writeln!(self).unwrap();
    }

    fn status(&mut self, prefix: &str, status: Option<i32>) {
        write!(self, "{}", prefix).unwrap();
        if let Some(status) = status {
            write!(self, "{}", status).unwrap();
        } else {
            write!(self, "None").unwrap();
        }
        writeln!(self).unwrap();
    }

    fn io(&mut self, prefix: &str, data: &[u8]) {
        if data.len() > 0 {
            write!(self, "{}", prefix).unwrap();
            for &byte in data {
                self.write_all(&[byte]).unwrap();
                if byte == '\n' as u8 {
                    write!(self, "{}", prefix).unwrap();
                }
            }
            writeln!(self).unwrap();
        }
    }
}

impl Write for Log {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        if self.actual {
            self.actual_write.write_all(buf)?;
        }
        if self.expected {
            self.expected_write.write_all(buf)?;
        }
        Ok(buf.len())
    }
    fn flush(&mut self) -> std::io::Result<()> {
        if self.actual {
            self.actual_write.flush()?;
        }
        if self.expected {
            self.expected_write.flush()?;
        }
        Ok(())
    }
}

