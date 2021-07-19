#![no_main]

use libfuzzer_sys::fuzz_target;

use ro::syntax::ast::ModuleAst;
use ro::syntax::formatter::{CodeStyle, FormatCode};
use ro::syntax::parser::Parser;

fuzz_target!(|input: ModuleAst| {
    let f = format!("{}", input.format(&CodeStyle::DEFAULT));
    // println!("======================");
    // println!("{}", f);
    let reparsed = Parser::new(&f, "dummy.ro".as_ref()).parse().unwrap();
    let f2 = format!("{}", reparsed.format(&CodeStyle::DEFAULT));
    assert_eq!(f, f2);
});
