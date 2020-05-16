use std::fs;
use std::io::Write;


use crate::name_analysis::*;

pub fn gen_prog(out: &mut fs::File, prog: &Prog) {
    for t in &prog.types {
        write!(out, "{}", gen_type(prog, t)).expect("failed to write line");
    }

    for def in &prog.definitions {
        write!(out, "{}", gen_stmt(prog, def)).expect("failed to write line");
    }

    for func in &prog.functions {
        write!(out, "{}", gen_func(prog, func)).expect("failed to write line");
    }
}

fn gen_type(prog: &Prog, t: &Type) -> String {
    let mut output = format!("const {} = {{\n", t.name);

    for opt in &t.options {
        output = format!("{}{}: '{}',\n", output, opt.name.to_ascii_uppercase(), opt.name);
    }

    output = format!("{}}}\n", output);

    output
}

fn gen_func(prog: &Prog, func: &Func) -> String {
    let mut output = format!("function {}(", gen_sym(&prog.symbol_table, &func.name));

    output = format!("{}){{\n", output);
    output = format!("{}{}", output, gen_body(prog, &func.body));

    format!("{}}}\n", output)
}

fn gen_body(prog: &Prog, body: &Body) -> String {
    let mut output = String::from("");

    for stmt in &body.stmts {
        output = format!("{}{}", output, gen_stmt(prog, &stmt));
    }

    body.expr.as_ref().map(|expr| {
        output = format!("return {};\n", gen_expr(prog, &expr));
    });

    output
}

fn gen_stmt(prog: &Prog, stmt: &Stmt) -> String {
    match stmt {
        Stmt::Assign(tgt, expr) => {
            format!("{} = {};\n", gen_target(prog, tgt), gen_expr(prog, expr))
        }
        Stmt::Case(case) => gen_case(prog, &case),
        Stmt::FnCall(fn_id, args) => {
            let mut output = format!("{}(", gen_sym(&prog.symbol_table, fn_id).to_owned());
            for arg in args {
                output = format!("{}, {}", output, gen_expr(prog, arg));
            }
            format!("{});\n", output)
        }
    }
}

fn gen_case(prog: &Prog, case: &Case) -> String {
    let mut output = format!("var _case_expr{} = {}\n", case.id, gen_expr(prog, &case.expr));
    output = format!("{}switch(_case_expr[0]){{\n", output);

    for opt in &case.options {
        output = format!("{}{}", output, gen_case_option(&prog, &opt, case.id));
    }

    format!("{}}}\n", output)
}

fn gen_case_option(prog: &Prog, option: &CaseOption, case_id: CaseID) -> String {
    let mut output = format!("case {} :\n", gen_pattern(prog, &option.pattern));

    for (i, arg) in option.pattern.args.iter().enumerate() {
        output = format!("{}var {} = _case_expr{}[{}]\n", output, gen_sym(&prog.symbol_table, arg), case_id, i + 1);
    }

    output
}

fn gen_pattern(prog: &Prog, pattern: &CasePattern) -> String {
    gen_adtval(&prog.type_table, &pattern.base)
}

fn gen_target(prog: &Prog, tgt: &Target) -> String {
    match tgt {
        Target::Mutable(sym) | Target::Var(sym) => {
            format!("var {}", gen_sym(&prog.symbol_table, sym))
        }
        Target::Update(sym) => {
            gen_sym(&prog.symbol_table, sym)
        }
    }
}

fn gen_expr(prog: &Prog, expr: &Expr) -> String {
    match expr {
        Expr::Add(left, right) => format!("({} + {})", gen_expr(prog, left), gen_expr(prog, right)),
        Expr::Subt(left, right) => format!("({} - {})", gen_expr(prog, left), gen_expr(prog, right)),
        Expr::Mult(left, right) => format!("({} * {})", gen_expr(prog, left), gen_expr(prog, right)),
        Expr::Div(left, right) => format!("({} / {})", gen_expr(prog, left), gen_expr(prog, right)),
        Expr::Pow(left, right) => unimplemented!(),
        Expr::Lit(l) => format!("{}", l),
        Expr::Id(id) => gen_sym(&prog.symbol_table, id),
        Expr::FnCall(fn_id, args) => {
            let mut output = format!("{}(", gen_sym(&prog.symbol_table, fn_id).to_owned());
            for arg in args {
                output = format!("{}, {}", output, gen_expr(prog, arg));
            }
            format!("{})", output)
        }
        Expr::ADTVal(base, args) => {
            let mut output = format!("[{}", gen_adtval(&prog.type_table, base));

            for arg in args {
                output = format!("{}, {}", output, gen_expr(prog, &arg));
            }

            format!("{}]", output)
        }
    }
}

/// Currently just uses the symbol's name. This may need a more sophisticated
/// scheme later on if we want to implement variable shadowing
fn gen_sym(table: &SymbolTable, id: &SymbolID) -> String {
    let sym = table.lookup_id(id).expect("Symbol not found");
    String::from(format!("{}", sym.name))
}

fn gen_adtval(types: &TypeTableExt, id: &ADTValID) -> String {
    let val = types.values.get(id).expect("Symbol not found");
    String::from(format!("{}.{}", val.data_type, val.name.to_ascii_uppercase()))
}