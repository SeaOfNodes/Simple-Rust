#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|input: &str| {
    simple_rust_fuzz::run_and_compare_eval(input, false);
});
