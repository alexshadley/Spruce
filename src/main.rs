#[macro_use]
extern crate pest_derive;

#[macro_use]
extern crate lazy_static;

use std::fs;

mod parser;
mod name_analysis;
mod typecheck;
mod codegen;

fn main() {
    let prelude = fs::read_to_string("src/prelude.sp").expect("cannot read prelude");
    let unparsed_file = fs::read_to_string("src/samples/typecheck.sp").expect("cannot read file");

    let preprocessed = format!("{}{}", prelude, unparsed_file);
    let prog = parser::parse(&preprocessed).expect("Parse failed");
    println!("{:#?}", prog);

    let analyzed_prog = name_analysis::name_analysis(prog).expect("Name analysis failed");
    println!("{:#?}", analyzed_prog);

    let env = typecheck::check_prog(&analyzed_prog).expect("Typecheck failed");
    println!("{:?}", env);

    let mut out_file = fs::File::create("out.js").expect("failed to create file");
    codegen::gen_prog(&mut out_file, &analyzed_prog);
}
