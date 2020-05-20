use std::collections::HashMap;
use std::collections::HashSet;
use std::iter::FromIterator;

use crate::name_analysis as na;

type TVarID = u32;

#[derive(Clone, Debug, PartialEq)]
enum Type {
    Unit,
    Prim(String),
    TVar(TVarID),
    ADT(String),
    Func(Vec<Box<Type>>, Box<Type>)
}

#[derive(Debug)]
pub struct Environment {
    next_type_var: TVarID,
    sym_type: HashMap<na::SymbolID, Type>,
    val_type: HashMap<na::ADTValID, Type>,
    //in_progress: HashSet<na::SymbolID>
}

impl Environment {
    fn new() -> Self {
        Environment {next_type_var: 0, sym_type: HashMap::new(), val_type: HashMap::new()}
    }

    fn new_tvar(&mut self) -> Type {
        self.next_type_var += 1;
        Type::TVar(self.next_type_var - 1)
    }

    fn apply_subs(&mut self, subs: &TSubst) {
        self.sym_type = self.sym_type.iter().map(|(k, ty)| {
            (*k, apply(subs, (*ty).clone()))
        }).collect();
    }
}

type TSubst = HashMap<TVarID, Type>;

fn str_to_type(prog: &na::Prog, s: &String) -> Type {
    if prog.type_table.primitives.contains(s) {
        Type::Prim(s.clone())
    }
    else {
        Type::ADT(s.clone())
    }
}

pub fn check_prog(prog: &na::Prog) -> Result<Environment, String> {
    let mut env = Environment::new();

    for (_, val) in &prog.type_table.values {
        let args = val.args.iter().map(|arg| { Box::from(str_to_type(prog, arg)) }).collect();
        let out = str_to_type(prog, &val.data_type);

        env.val_type.insert(val.id, Type::Func(args, Box::from(out)));
    }

    for stmt in &prog.definitions {
        match stmt {
            na::Stmt::Assign(tgt, expr) => {
                let stmt_tvar = env.new_tvar();
                let subs = typecheck(&mut env, expr, &stmt_tvar);
                match subs {
                    Some(s) => {
                        let stmt_type = apply(&s, stmt_tvar);
                        env.sym_type.insert(tgt.id(), stmt_type);
                    }
                    None => {
                        return Err(String::from(format!("Unification failed for expr: {:?}", expr)))
                    }
                }
            }
            _ => unreachable!()
        }
    }

    for func in &prog.functions {
        check_func(&mut env, func)?;
    }

    Ok(env)
}

fn check_func(env: &mut Environment, func: &na::Func) -> Result<bool, String> {
    let mut arg_types = Vec::new();
    for arg in &func.args {
        let arg_tvar = env.new_tvar();
        env.sym_type.insert(*arg, arg_tvar.clone());
        arg_types.push(Box::from(arg_tvar));
    }
    let ret_tvar = env.new_tvar();
    let fn_type = Type::Func(arg_types, Box::from(ret_tvar.clone()));
    env.sym_type.insert(func.name, fn_type);

    let ret_type = check_body(env, &func.body, &ret_tvar);

    Ok(true)
}

/// Returns tuple of type subs and a bool indicating if the case has a valid
/// type. Unlinke other structures, it's ok for a case not to have a type in
/// some circumstances.
fn check_case(env: &mut Environment, case: &na::Case, ty: &Type) -> Result<TSubst, String> {
    let mut subs = HashMap::new();

    let expr_tvar = env.new_tvar();
    let expr_subs = typecheck(env, &case.expr, &expr_tvar).expect("failed typecheck");
    let expr_type = apply(&expr_subs, expr_tvar);
    env.apply_subs(&expr_subs);
    subs.extend(expr_subs);


    // start by analyzing patterns
    let mut pattern_type = String::from("");
    for opt in &case.options {
        let opt_pat_type = match env.val_type.get(&opt.pattern.base).expect("dangling type id") {
            Type::Func(args, out) => {
                match &**out {
                    Type::ADT(name) => name,
                    _ => unreachable!()
                }
            }
            _ => unreachable!()
        };
        if pattern_type == "" {
            pattern_type = (*opt_pat_type).clone();
        }
        else {
            if pattern_type != *opt_pat_type {
                return Err(format!("case statement has patterns of both types {} and {}", pattern_type, opt_pat_type));
            }
        }
    }

    let pattern_subs = unify(&expr_type, &Type::ADT(pattern_type)).expect("unification between expr and pattern failed");
    env.apply_subs(&pattern_subs);
    subs.extend(pattern_subs);

    let mut is_unit = false;
    let mut has_expr = false;
    for opt in &case.options {
        let pattern_arg_types = match env.val_type.get(&opt.pattern.base).expect("dangling type id") {
            Type::Func(args, _) => args,
            _ => unreachable!()
        };

        for (arg, ty) in opt.pattern.args.iter().zip(pattern_arg_types) {
            let arg_type: Type = (**ty).clone();
            env.sym_type.insert(*arg, arg_type);
        }


        match &opt.body {
            na::CaseBody::Body(body) => {
                let opt_tvar = env.new_tvar();
                let opt_subs = check_body(env, &body, &opt_tvar)?;
                let opt_type = apply(&opt_subs, opt_tvar);
                env.apply_subs(&opt_subs);
                subs.extend(opt_subs);

                match unify(&apply(&subs, (*ty).clone()), &opt_type) {
                    Some(uni_subs) => {
                        env.apply_subs(&uni_subs);
                        subs.extend(uni_subs);
                    }
                    None => {
                        is_unit = true;
                    }
                }
            }
            na::CaseBody::Expr(expr) => {
                has_expr = true;

                let opt_subs = typecheck(env, &expr, &apply(&subs, (*ty).clone())).expect("case expr failed typecheck");
                env.apply_subs(&opt_subs);
                subs.extend(opt_subs);
            }
        }
    }

    if is_unit {
        if has_expr {
            return Err(String::from("Case with expr must have type"));
        }
        else {
            let unit_subs = unify(ty, &Type::Unit).expect("unreachable");
            subs.extend(unit_subs);
        }
    }

    Ok(subs)
}

fn check_body(env: &mut Environment, body: &na::Body, ty: &Type) -> Result<TSubst, String> {
    let mut stmt_types = Vec::new();
    let mut subs = HashMap::new();
    for stmt in &body.stmts {
        match stmt {
            na::Stmt::Assign(tgt, expr) => {
                let new_tvar = env.new_tvar();
                let stmt_subs = typecheck(env, expr, &new_tvar).expect("typecheck stmt failed");
                let var_type = apply(&stmt_subs, new_tvar);
                
                env.sym_type.insert(tgt.id(), var_type.clone());
                env.apply_subs(&stmt_subs);

                stmt_types.push(var_type);
                subs.extend(stmt_subs);
            }
            na::Stmt::Case(case) => {
                let new_tvar = env.new_tvar();
                let case_subs = check_case(env, case, &new_tvar)?;
                let var_type = apply(&case_subs, new_tvar);

                env.apply_subs(&case_subs);

                stmt_types.push(var_type);
                subs.extend(case_subs);
            }
            _ => unimplemented!()
        }
    }

    match &body.expr {
        Some(expr) => {
            let subs = typecheck(env, &expr, ty).expect("typecheck return expr failed");
            env.apply_subs(&subs);
        }
        None => {
            let last_stmt_type = stmt_types.last().expect("unreachable");
            let stmt_subs = unify(last_stmt_type, ty).expect("unreachable");
            env.apply_subs(&stmt_subs);
        }
    };

    Ok(subs)
}

macro_rules! int_prim {
    () => {
        Type::Prim(String::from("Int"))
    };
}

// TODO: add apply_env everywhere
fn typecheck(env: &mut Environment, expr: &na::Expr, ty: &Type) -> Option<TSubst> {
    match expr {
        na::Expr::Lit(_) => unify(ty, &int_prim!()),
        na::Expr::Add(left, right) | na::Expr::Subt(left, right) | na::Expr::Mult(left, right) |
        na::Expr::Div(left, right) | na::Expr::Pow(left, right) => {
            let mut subs = unify(ty, &int_prim!())?;

            let subs1 = typecheck(env, left, &int_prim!())?;
            let subs2 = typecheck(env, right, &int_prim!())?;
            subs.extend(subs1);
            subs.extend(subs2);
            Some(subs)
        }
        na::Expr::Eq(left, right) | na::Expr::NotEq(left, right) => {
            let mut subs = unify(ty, &Type::ADT(String::from("Bool")))?;

            let new_tvar = env.new_tvar();
            let subs1 = typecheck(env, left, &new_tvar)?;

            let updated_tvar = apply(&subs1, new_tvar);
            let subs2 = typecheck(env, right, &updated_tvar)?;

            subs.extend(subs1);
            subs.extend(subs2);
            Some(subs)
        }
        na::Expr::LtEq(left, right) | na::Expr::GtEq(left, right) | na::Expr::Lt(left, right) |
        na::Expr::Gt(left, right) => {
            let mut subs = unify(ty, &Type::ADT(String::from("Bool")))?;

            let subs1 = typecheck(env, left, &int_prim!())?;
            let subs2 = typecheck(env, right, &int_prim!())?;
            subs.extend(subs1);
            subs.extend(subs2);
            Some(subs)
        }

        na::Expr::Id(id) => {
            match env.sym_type.get(id) {
                Some(sym_type) => {
                    unify(ty, sym_type)
                }
                None => None
            }
        }

        na::Expr::FnCall(id, args) => {
            // TODO: finish
            let mut subs = HashMap::new();
            let mut arg_types = Vec::new();
            for arg in args {
                let arg_tvar = env.new_tvar();
                let arg_subs = typecheck(env, arg, &arg_tvar)?;
                arg_types.push(Box::from(apply(&arg_subs, arg_tvar)));
                subs.extend(arg_subs);
            }

            let out_tvar = env.new_tvar();
            let out_subs = unify(&ty, &out_tvar)?;
            let out_type = apply(&out_subs, out_tvar);
            subs.extend(out_subs);

            let fn_type = Type::Func(arg_types, Box::from(out_type));

            let fn_sym_type = env.sym_type.get(id).expect("dangling function id");
            let fn_subs = unify(fn_sym_type, &fn_type)?;
            subs.extend(fn_subs);

            Some(subs)
        }

        na::Expr::ADTVal(_, _) => {
            unify(ty, &Type::ADT(String::from("Bool")))
        }
    }
}


fn apply(subs: &TSubst, ty: Type) -> Type {
    match &ty {
        Type::TVar(id) => {
            match &subs.get(id) {
                Some(sub_ty) => apply(subs, (**sub_ty).clone()),
                None => ty
            }
        }
        Type::Unit => ty,
        Type::Prim(_) => ty,
        Type::ADT(_) => ty,
        Type::Func(args, out) => {
            let new_args = args.iter().map(|arg| { Box::from(apply(subs, (**arg).clone())) }).collect();

            Type::Func(new_args, Box::from(apply(subs, (**out).clone())))
        }
    }
}

fn unify(left: &Type, right: &Type) -> Option<TSubst> {
    match (left, right) {
        (Type::TVar(id1), Type::TVar(id2)) => {
            if id1 == id2 {
                Some(HashMap::new())
            }
            else {
                Some(HashMap::from_iter(vec![(*id1, right.clone())]))
            }
        }

        (Type::TVar(id), _) => {
            if tvars(right).contains(id) {
                None
            }
            else {
                Some(HashMap::from_iter(vec![(*id, right.clone())]))
            }
        }

        (_, Type::TVar(id)) => {
            if tvars(left).contains(id) {
                None
            }
            else {
                Some(HashMap::from_iter(vec![(*id, left.clone())]))
            }
        }

        (Type::Prim(p1), Type::Prim(p2)) => {
            if p1 == p2 {
                Some(HashMap::new())
            }
            else {
                None
            }
        }

        (Type::ADT(ty1), Type::ADT(ty2)) => {
            if ty1 == ty2 {
                Some(HashMap::new())
            }
            else {
                None
            }
        }
        (Type::Func(args1, out1), Type::Func(args2, out2)) => {
            if args1.len() != args2.len() {
                return None
            }

            let mut subs = HashMap::new();
            for (arg1, arg2) in args1.iter().zip(args2) {
                let arg_subs = unify(&apply(&subs, *arg1.clone()), &apply(&subs, *arg2.clone()))?;
                subs.extend(arg_subs);
            }

            let out_subs = unify(&apply(&subs, *out1.clone()), &apply(&subs, *out2.clone()))?;
            subs.extend(out_subs);

            Some(subs)
        }

        _ => None
    }
}

fn tvars(ty: &Type) -> HashSet<TVarID> {
    match ty {
        Type::TVar(id) => HashSet::from_iter(vec![*id]),
        Type::Unit => HashSet::new(),
        Type::Prim(_) => HashSet::new(),
        Type::ADT(_) => HashSet::new(),
        Type::Func(args, out) => {
            let mut vars = HashSet::new();
            for arg in args {
                let arg_vars = tvars(arg);
                vars.extend(arg_vars);
            }
            vars
        }
    }
}

#[test]
fn unify_prim() {
    let res = unify(&int_prim!(), &int_prim!());
    assert_eq!(res.is_some(), true);

    let res = unify(&int_prim!(), &Type::Prim(String::from("Float")));
    assert_eq!(res.is_some(), false);
}

#[test]
fn unify_fn() {
    let res = unify(
        &Type::Func(vec![Box::from(Type::TVar(0))], Box::from(Type::TVar(0))),
        &Type::Func(vec![Box::from(int_prim!())], Box::from(int_prim!()))
    );
    assert_eq!(res.is_some(), true);
    assert_eq!(*res.expect("").get(&0).expect(""), int_prim!());

    let res = unify(
        &Type::Func(vec![Box::from(Type::TVar(0))], Box::from(Type::TVar(0))),
        &Type::Func(vec![Box::from(int_prim!())], Box::from(Type::ADT(String::from("Bool"))))
    );
    assert_eq!(res.is_some(), false);
}
