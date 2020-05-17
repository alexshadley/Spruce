use std::fs;
use std::io::Write;


use crate::name_analysis::*;

pub fn gen_prog(out: &mut fs::File, prog: &Prog) {
    let js_helpers = fs::read_to_string("src/helper.js").expect("cannot read js helpers file");
    write!(out, "{}", js_helpers).expect("failed to write helpers");

    for t in &prog.types {
        write!(out, "{}", gen_type(prog, t)).expect("failed to write line");
    }

    for def in &prog.definitions {
        let (stmt_str, _) = gen_stmt(prog, def);
        write!(out, "{}", stmt_str).expect("failed to write line");
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

    func.args.first().as_ref().map(|arg| {
        output = format!("{}{}", output, gen_sym(&prog.symbol_table, arg));
    });
    for arg in func.args.iter().skip(1) {
        output = format!("{}, {}", output, gen_sym(&prog.symbol_table, arg));
    };

    output = format!("{}){{\n", output);
    let (body_str, body_val) = gen_body(prog, &func.body);
    output = format!("{}{}", output, body_str);

    body_val.map(|val| {
        output = format!("{}return {};\n", output, val);
    });

    format!("{}}}\n", output)
}

fn gen_body(prog: &Prog, body: &Body) -> (String, Option<String>) {
    let mut output = String::from("");

    for stmt in &body.stmts {
        let (stmt_str, _) = gen_stmt(prog, &stmt);
        output = format!("{}{}", output, stmt_str);
    }

    body.expr.as_ref().map(|expr| {
        output = format!("{}var _body_val = {};\n", output, gen_expr(prog, &expr));
    });

    let val_handle = match (&body.expr, body.stmts.last()) {
        (Some(expr), _) => Some(String::from("_body_val")),
        (_, Some(stmt)) => {
            let (_, stmt_val) = gen_stmt(prog, &stmt);
            stmt_val
        }
        (_, _) => None
    };

    (output, val_handle)
}

fn gen_stmt(prog: &Prog, stmt: &Stmt) -> (String, Option<String>) {
    match stmt {
        Stmt::Assign(tgt, expr) => {
            let (tgt_str, tgt_var) = gen_target(prog, tgt);
            (format!("{} = {};\n", tgt_str, gen_expr(prog, expr)), Some(tgt_var))
        }
        Stmt::Case(case) => gen_case(prog, &case),
        Stmt::FnCall(fn_id, args) => {
            let mut output = format!("var _fn_val = {}(", gen_sym(&prog.symbol_table, fn_id).to_owned());

            args.first().as_ref().map(|arg| {
                output = format!("{}{}", output, gen_expr(prog, arg));
            });
            for arg in args.iter().skip(1) {
                output = format!("{}, {}", output, gen_expr(prog, arg));
            };

            (format!("{});\n", output), Some(String::from("_fn_val")))
        }
    }
}

fn gen_case(prog: &Prog, case: &Case) -> (String, Option<String>) {
    let mut output = format!("var _case_expr{} = {};\n", case.id, gen_expr(prog, &case.expr));
    output = format!("{}var _case_val{};\n", output, case.id);
    output = format!("{}switch(_case_expr{}[0]){{\n", output, case.id);

    for opt in &case.options {
        output = format!("{}{}", output, gen_case_option(&prog, &opt, case.id));
    }

    (format!("{}}}\n", output), Some(format!("_case_val{}", case.id)))
}

fn gen_case_option(prog: &Prog, option: &CaseOption, case_id: CaseID) -> String {
    let mut output = format!("case {}:\n", gen_pattern(prog, &option.pattern));

    for (i, arg) in option.pattern.args.iter().enumerate() {
        output = format!("{}var {} = _case_expr{}[{}];\n", output, gen_sym(&prog.symbol_table, arg), case_id, i + 1);
    }

    match &option.body {
        CaseBody::Body(body) => {
            let (body, val_handle) = gen_body(prog, body);
            output = format!("{}{}", output, body);
            val_handle.map(|handle| {
                output = format!("{}_case_val{} = {};\n", output, case_id, handle);
            });
        }
        CaseBody::Expr(expr) => {
            output = format!("{}_case_val{} = {};\n", output, case_id, gen_expr(prog, expr));
        }
    }

    format!("{}break;\n", output)
}

fn gen_pattern(prog: &Prog, pattern: &CasePattern) -> String {
    gen_adtval(&prog.type_table, &pattern.base)
}

fn gen_target(prog: &Prog, tgt: &Target) -> (String, String) {
    match tgt {
        Target::Mutable(sym) | Target::Var(sym) => {
            let var_name = gen_sym(&prog.symbol_table, sym);
            (format!("var {}", var_name), var_name)
        }
        Target::Update(sym) => {
            let var_name = gen_sym(&prog.symbol_table, sym);
            (var_name.clone(), var_name)
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
        Expr::Eq(left, right) => format!("_to_bool({} == {})", gen_expr(prog, left), gen_expr(prog, right)),
        Expr::NotEq(left, right) => format!("_to_bool({} != {})", gen_expr(prog, left), gen_expr(prog, right)),
        Expr::LtEq(left, right) => format!("_to_bool({} <= {})", gen_expr(prog, left), gen_expr(prog, right)),
        Expr::GtEq(left, right) => format!("_to_bool({} >= {})", gen_expr(prog, left), gen_expr(prog, right)),
        Expr::Lt(left, right) => format!("_to_bool({} < {})", gen_expr(prog, left), gen_expr(prog, right)),
        Expr::Gt(left, right) => format!("_to_bool({} > {})", gen_expr(prog, left), gen_expr(prog, right)),
        Expr::Lit(l) => format!("{}", l),
        Expr::Id(id) => gen_sym(&prog.symbol_table, id),
        Expr::FnCall(fn_id, args) => {
            let mut output = format!("{}(", gen_sym(&prog.symbol_table, fn_id).to_owned());

            args.first().as_ref().map(|arg| {
                output = format!("{}{}", output, gen_expr(prog, arg));
            });
            for arg in args.iter().skip(1) {
                output = format!("{}, {}", output, gen_expr(prog, arg));
            };

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