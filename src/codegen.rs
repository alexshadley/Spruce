use std::fs::File;

use crate::name_analysis::*;


fn gen_prog(out: File, prog: &Prog) {

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
        Expr::ADTVal(_, _) => unimplemented!()
    }
}

/// This function will eventually have a much more intelligent naming scheme
fn gen_sym(table: &SymbolTable, id: &SymbolID) -> String {
    let sym = table.lookup_id(id).expect("Symbol not found");
    String::from(format!("sym_{}", sym.id))
}