#![no_main]

use libfuzzer_sys::fuzz_target;
use simple_rust_fuzz::script_generator::GeneratedScript;

fuzz_target!(|script: GeneratedScript| {
    simple_rust_fuzz::run_and_compare_eval(&script.source, !script.maybe_invalid);
});
