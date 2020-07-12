#![feature(str_strip)]

#[macro_use]
extern crate pest_derive;

#[macro_use]
extern crate lazy_static;

use std::fs;
use std::env;
use std::path::Path;
use std::collections::HashSet;
use std::iter::FromIterator;

mod parser;
mod error;
mod import_analysis;
mod name_analysis;
mod typecheck;
mod codegen;

/// Compilation takes place in four phases: Parsing, Name Analysis, Type
/// Checking, and Code Generation. Parsing and Name Analysis both emit their
/// own IR, Type Checking simply emits a mapping from symbols to types, and
/// Code Generation writes the compiled javascript to a file
fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        println!("usage: spc <source> <output>");
        return;
    }

    let spruce_code: &String = &args[1];
    let spruce_path = Path::new(spruce_code);

    let prelude = fs::read_to_string("src/Prelude.sp").expect("cannot read prelude");
    let unparsed_file = fs::read_to_string(spruce_path).expect(&format!("Cannot find file {}", spruce_code));
    let files = vec![(prelude, String::from("Prelude.sp")), (unparsed_file, spruce_code.clone())];

    let (analyzed_prog, environment) = match compile(files.clone()){
        Ok(r) => r,
        Err((e, files)) => {
            println!("{}", e.as_str(&files));
            return;
        }
    };

    let out_file_name = &args[2];
    let mut out_file = fs::File::create(out_file_name).expect("failed to create file");
    codegen::gen_prog(&mut out_file, &analyzed_prog, &environment);
}

pub fn compile(files: Vec<(String, String)>) -> Result<(name_analysis::Prog, typecheck::Environment), (error::SpruceErr, Vec<(String, String)>)> {
    let mut imports: HashSet<String> = HashSet::from_iter(vec![String::from("Prelude")].into_iter());
    let mut unparsed_files = files.clone();
    let mut parsed_files = Vec::new();

    // used in error reporting
    let mut all_files = files;

    // each iteration takes a file off the stack, parses, finds all files that need to
    // be imported, then adds those to the stack
    while unparsed_files.len() > 0 {
        let (file, name) = unparsed_files.pop().expect("unreachable");
        all_files.push((file.clone(), name.clone()));

        let parsed = parser::parse(file.as_str(), name).map_err(|err| (err, all_files.clone()) )?;
        println!("{:#?}", parsed);

        for import in &parsed.imports {
            if !imports.contains(import) {
                let import_file = format!("{}.sp", import);
                let unparsed_file = fs::read_to_string(import_file.clone()).expect(&format!("Cannot find file {}", import_file));
                unparsed_files.push((unparsed_file, import_file));
            }
        }

        imports.extend(parsed.imports.clone());
        parsed_files.push(parsed);
    }

    let ordered_modules = import_analysis::order_modules(parsed_files);

    let analyzed_prog = name_analysis::name_analysis(ordered_modules).map_err(|err| (err, all_files.clone()) )?;
    println!("{:#?}", analyzed_prog);

    let environment = typecheck::check_prog(&analyzed_prog).map_err(|err| (err, all_files.clone()) )?;
    println!("{}", environment.as_str(&analyzed_prog));

    Ok((analyzed_prog, environment))
}

#[test]
fn test_prelude() {
    let prelude = fs::read_to_string("src/Prelude.sp").expect("cannot read prelude");
    let files = vec![(prelude, String::from("Prelude.sp"))];
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

    let prelude = fs::read_to_string("src/Prelude.sp").expect("cannot read prelude");
    let files = vec![(prelude, String::from("Prelude.sp")), (String::from(pass_prog), String::from("Main.sp"))];
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

    let prelude = fs::read_to_string("src/Prelude.sp").expect("cannot read prelude");
    let files = vec![(prelude, String::from("Prelude.sp")), (String::from(fail_prog), String::from("Main.sp"))];
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

    let prelude = fs::read_to_string("src/Prelude.sp").expect("cannot read prelude");
    let files = vec![(prelude, String::from("Prelude.sp")), (String::from(pass_prog), String::from("Main.sp"))];
    let res = compile(files);
    assert_eq!(res.is_ok(), true);

    let fail_prog = "
f() {
    x = 1
    x := 2
}
";

    let prelude = fs::read_to_string("src/Prelude.sp").expect("cannot read prelude");
    let files = vec![(prelude, String::from("Prelude.sp")), (String::from(fail_prog), String::from("Main.sp"))];
    let res = compile(files);
    assert_eq!(res.is_ok(), false);
}

#[test]
fn test_curry() {
    let pass_prog = "
add(x, y) {
    x + y
}

main() {
    add3 = add@(3, _)
    add3(5)
}
";

    let prelude = fs::read_to_string("src/Prelude.sp").expect("cannot read prelude");
    let files = vec![(prelude, String::from("Prelude.sp")), (String::from(pass_prog), String::from("Main.sp"))];
    let res = compile(files);
    assert_eq!(res.is_ok(), true);

    let pass_prog = "
add(x, y) {
    x + y
}

main() {
    myList = Cons(Cons(Nil, 2), 1)
    listMap(myList, add@(3, _))
}
";

    let prelude = fs::read_to_string("src/Prelude.sp").expect("cannot read prelude");
    let files = vec![(prelude, String::from("Prelude.sp")), (String::from(pass_prog), String::from("Main.sp"))];
    let res = compile(files);
    assert_eq!(res.is_ok(), true);

    let fail_prog = "
add(x, y) {
    x + y
}

main() {
    myList = Cons(Cons(Nil), 2), 1)
    listMap(myList, add@(True, _))
}
";

    let prelude = fs::read_to_string("src/Prelude.sp").expect("cannot read prelude");
    let files = vec![(prelude, String::from("Prelude.sp")), (String::from(fail_prog), String::from("Main.sp"))];
    let res = compile(files);
    assert_eq!(res.is_ok(), false);
}

#[test]
fn test_annotations() {
    let pass_prog = "
type FooBar(a, b) {
    Foo(a)
    Bar(b)
}

func(fb: FooBar(a, Bool)) -> Bool {
    case fb {
        Foo(v) -> True
        Bar(b) -> b
    }
}
";

    let prelude = fs::read_to_string("src/Prelude.sp").expect("cannot read prelude");
    let files = vec![(prelude, String::from("Prelude.sp")), (String::from(pass_prog), String::from("Main.sp"))];
    let res = compile(files);
    assert_eq!(res.is_ok(), true);

    let fail_prog = "
type FooBar(a, b) {
    Foo(a)
    Bar(b)
}

func(fb: FooBar(Bool, Int)) -> Bool {
    case fb {
        Foo(v) -> True
        Bar(b) -> b
    }
}
";

    let prelude = fs::read_to_string("src/Prelude.sp").expect("cannot read prelude");
    let files = vec![(prelude, String::from("Prelude.sp")), (String::from(fail_prog), String::from("Main.sp"))];
    let res = compile(files);
    assert_eq!(res.is_ok(), false);

    let fail_prog = "
badId(x: a) -> b {
    x
}
";

    let prelude = fs::read_to_string("src/Prelude.sp").expect("cannot read prelude");
    let files = vec![(prelude, String::from("Prelude.sp")), (String::from(fail_prog), String::from("Main.sp"))];
    let res = compile(files);
    assert_eq!(res.is_ok(), false);
}
