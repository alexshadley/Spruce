#[macro_use]
extern crate pest_derive;

#[macro_use]
extern crate lazy_static;

use std::fs;

mod parser;
mod name_analysis;
mod codegen;

fn main() {
    let unparsed_file = fs::read_to_string("src/samples/test.sp").expect("cannot read file");

    let prog = parser::parse(&unparsed_file).expect("Parse failed");
    println!("{:#?}", prog);

    let analyzed_prog = name_analysis::name_analysis(prog).expect("Name analysis failed");
    println!("{:#?}", analyzed_prog);

    let mut out_file = fs::File::create("out.js").expect("failed to create file");
    codegen::gen_prog(&mut out_file, &analyzed_prog);
}
