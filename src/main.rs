#[macro_use]
extern crate pest_derive;

#[macro_use]
extern crate lazy_static;

use std::fs;
use std::collections::HashMap;

mod parser;
mod error;
mod name_analysis;
mod typecheck;
mod codegen;

fn main() {
    let prelude = fs::read_to_string("src/prelude.sp").expect("cannot read prelude");
    let unparsed_file = fs::read_to_string("src/samples/lists.sp").expect("cannot read file");
    let files = vec![(prelude.as_str(), String::from("prelude")), (unparsed_file.as_str(), String::from("main"))];

    let prog = parser::parse(files.clone()).expect("Parse failed");
    println!("{:#?}", prog);

    let analyzed_prog = match name_analysis::name_analysis(prog) {
        Ok(p) => p,
        Err(name_err) => {
            print!("{}", name_err.as_str(&files));
            return;
        }
    };
    println!("{:#?}", analyzed_prog);

    let check_res = typecheck::check_prog(&analyzed_prog);
    match check_res {
        Ok(env) => {
            println!("{}", env.as_str(&analyzed_prog));
        }
        Err(type_err) => {
            print!("{}", type_err.as_str(&files));
        }
    }

    let mut out_file = fs::File::create("out.js").expect("failed to create file");
    codegen::gen_prog(&mut out_file, &analyzed_prog);
}
