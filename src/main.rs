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

/// Compilation takes place in four phases: Parsing, Name Analysis, Type
/// Checking, and Code Generation. Parsing and Name Analysis both emit their
/// own IR, Type Checking simply emits a mapping from symbols to types, and
/// Code Generation writes the compiled javascript to a file
fn main() {
    let prelude = fs::read_to_string("src/prelude.sp").expect("cannot read prelude");
    let unparsed_file = fs::read_to_string("samples/lists.sp").expect("cannot read file");
    let files = vec![(prelude.as_str(), String::from("prelude")), (unparsed_file.as_str(), String::from("main"))];

    let (analyzed_prog, environment) = match compile(files.clone()){
        Ok(r) => r,
        Err(e) => {
            println!("{}", e.as_str(&files));
            return;
        }
    };

    let mut out_file = fs::File::create("out.js").expect("failed to create file");
    codegen::gen_prog(&mut out_file, &analyzed_prog, &environment);
}

pub fn compile(files: Vec<(&str, String)>) -> Result<(name_analysis::Prog, typecheck::Environment), error::SpruceErr> {
    let prog = parser::parse(files.clone())?;
    println!("{:#?}", prog);

    let analyzed_prog = name_analysis::name_analysis(prog)?;
    println!("{:#?}", analyzed_prog);

    let environment = typecheck::check_prog(&analyzed_prog)?;
    println!("{}", environment.as_str(&analyzed_prog));

    Ok((analyzed_prog, environment))
}

#[test]
fn test_prelude() {
    let prelude = fs::read_to_string("src/prelude.sp").expect("cannot read prelude");
    let files = vec![(prelude.as_str(), String::from("prelude"))];
    let res = compile(files);
    assert_eq!(res.is_ok(), true);
}

#[test]
fn test_scope() {
    let pass_prog = "
x = 0
f() {
    x
}

g() {
    y = True
    case y {
        True -> y
        False -> y
    }
}
";

    let prelude = fs::read_to_string("src/prelude.sp").expect("cannot read prelude");
    let files = vec![(prelude.as_str(), String::from("prelude")), (pass_prog, String::from("Main"))];
    let res = compile(files);
    assert_eq!(res.is_ok(), true);

    let fail_prog = "
f(b) {
    case b {
        True -> {
            x = 1
            x
        }
        False -> {
            y = x
            y
        }
    }
}
";

    let prelude = fs::read_to_string("src/prelude.sp").expect("cannot read prelude");
    let files = vec![(prelude.as_str(), String::from("prelude")), (fail_prog, String::from("Main"))];
    let res = compile(files);
    assert_eq!(res.is_ok(), false);
}

#[test]
fn test_mut() {
    let pass_prog = "
mut x = 0
f() {
    x := 1
}

g() {
    mut y = True
    case y {
        True -> {
            y := False
        }
        False -> {
            y := True
        }
    }

    y
}
";

    let prelude = fs::read_to_string("src/prelude.sp").expect("cannot read prelude");
    let files = vec![(prelude.as_str(), String::from("prelude")), (pass_prog, String::from("Main"))];
    let res = compile(files);
    assert_eq!(res.is_ok(), true);

    let fail_prog = "
f() {
    x = 1
    x := 2
}
";

    let prelude = fs::read_to_string("src/prelude.sp").expect("cannot read prelude");
    let files = vec![(prelude.as_str(), String::from("prelude")), (fail_prog, String::from("Main"))];
    let res = compile(files);
    assert_eq!(res.is_ok(), false);
}

#[test]
fn test_type() {
    let pass_prog = "
type FooBar(a, b) {
    Foo(a)
    Bar(b)
}

func(fb) {
    case fb {
        Foo(v) -> True
        Bar(b) -> b
    }
}

main() {
    fb = Foo(3)
    res = func(fb)

    fb2 = Bar(True)
    res2 = func(fb2)
}
";

    let prelude = fs::read_to_string("src/prelude.sp").expect("cannot read prelude");
    let files = vec![(prelude.as_str(), String::from("prelude")), (pass_prog, String::from("Main"))];
    let res = compile(files);
    assert_eq!(res.is_ok(), true);

    let fail_prog = "
type FooBar(a, b) {
    Foo(a)
    Bar(b)
}

func(fb) {
    case fb {
        Foo(v) -> True
        Bar(b) -> b
    }
}

main() {
    fb = Foo(3)
    res = func(fb)

    fb2 = Bar(2)
    res2 = func(fb2)
}
";

    let prelude = fs::read_to_string("src/prelude.sp").expect("cannot read prelude");
    let files = vec![(prelude.as_str(), String::from("prelude")), (fail_prog, String::from("Main"))];
    let res = compile(files);
    assert_eq!(res.is_ok(), false);
}
