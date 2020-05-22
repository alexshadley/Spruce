use std::collections::HashMap;
use std::collections::HashSet;
use std::iter::FromIterator;

use crate::error::TypeErr;
use crate::name_analysis as na;
use crate::parser::Span;

type TVarID = u32;

#[derive(Clone, Debug, PartialEq)]
enum Type {
    Unit,
    Prim(String),
    TVar(TVarID),
    // the ADT, followed by type params
    ADT(na::ADTID, Vec<Box<Type>>),
    Func(Vec<Box<Type>>, Box<Type>)
}

impl Type {
    fn as_str(&self, prog: &na::Prog) -> String {
        let mut tvar_names: HashMap<TVarID, String> = HashMap::new();
        let mut next_name = 0u8;
        self.as_str_inner(prog, &mut tvar_names, &mut next_name)
    }

    fn as_str_inner(&self, prog: &na::Prog, tvar_names: &mut HashMap<TVarID, String>, next_name: &mut u8) -> String {
        match self {
            Type::TVar(id) => {
                match tvar_names.get(id) {
                    Some(repr) => repr.clone(),
                    None => {
                        let tvar_repr = ((*next_name + 97) as char).to_string();
                        *next_name += 1;
                        tvar_names.insert(*id, tvar_repr.clone());
                        tvar_repr
                    }
                }
            }
            Type::Unit => String::from("()"),
            Type::Prim(name) => name.clone(),
            Type::ADT(id, args) => {
                let name = prog.type_table.types.get(id).expect("dangling type id").name;
                if args.len() == 0 {
                    name.clone()
                }
                else {
                    let mut output = format!("{}(", name);
                    args.first().as_ref().map(|arg| {
                        output = format!("{}{}", output, arg.as_str_inner(prog, tvar_names, next_name));
                    });
                    for arg in args.iter().skip(1) {
                        output = format!("{}, {}", output, arg.as_str_inner(prog, tvar_names, next_name));
                    };
                    output
                }
            }
            Type::Func(args, out) => {
                let mut output = String::from("(");
                args.first().as_ref().map(|arg| {
                    output = format!("{}{}", output, arg.as_str_inner(prog, tvar_names, next_name));
                });
                for arg in args.iter().skip(1) {
                    output = format!("{}, {}", output, arg.as_str_inner(prog, tvar_names, next_name));
                };

                format!("{}) -> {}", output, out.as_str_inner(prog, tvar_names, next_name))
            }
        }
    }
}

// TODO: split symbols into "active" and "static", that way we don't continue
// refining fn types when we apply them
#[derive(Debug)]
pub struct Environment {
    next_type_var: TVarID,
    sym_type: HashMap<na::SymbolID, Type>,
    adt_type: HashMap<na::ADTID, Type>,
    val_type: HashMap<na::ADTValID, Type>,
}

impl Environment {
    fn new() -> Self {
        Environment {next_type_var: 0, sym_type: HashMap::new(), val_type: HashMap::new(), adt_type: HashMap::new()}
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

    pub fn as_str(&self, prog: &na::Prog) -> String {
        let mut output = String::from("");
        for (id, ty) in &self.sym_type {
            let name = prog.symbol_table.store.get(id).expect("dangling symbol id").name.clone();
            output = format!("{}{} : {}\n", output, name, ty.as_str(prog));
        }

        output
    }
}

type TSubst = HashMap<TVarID, Type>;

/*fn id_to_type(prog: &na::Prog, id: &na::TypeID) -> Type {
    match id {
        na::TypeID::ADT(adt_id) => {
            let adt = prog.type_table.types.get(adt_id).expect("dangling type id");
            Type::ADT(adt_id, adt.)
        }
    }
    if prog.type_table.primitives.contains(s) {
        Type::Prim(s.clone())
    }
    else {
    }
}*/

pub fn check_prog(prog: &na::Prog) -> Result<Environment, TypeErr> {
    let mut env = Environment::new();

    let mut tparams: HashMap<na::TParamID, Type> = HashMap::new();
    let mut adts: HashMap<na::ADTID, Type> = HashMap::new();
    for (_, ty) in &prog.type_table.types {
        let adt_params = ty.type_params.iter();
        let tvars = adt_params.map(|id| {
            let tvar = env.new_tvar();
            tparams.insert(*id, tvar);
            Box::from(tvar)
        }).collect();

        env.adt_type.insert(ty.id, Type::ADT(ty.id, tvars));
    }

    for (_, val) in &prog.type_table.values {
        let args = val.args.iter().map(|arg| {
            match arg {
                na::TypeID::TParam(id) => {
                    Box::from(tparams.get(id).expect("dangling tparam id").clone())
                }
                na::TypeID::ADT(id) => {
                    Box::from(env.adt_type.get(id).expect("dangling type id").clone())
                }
                na::TypeID::Prim(s) => {
                    Box::from(Type::Prim(s.clone()))
                }
            }
        }).collect();
        let out = env.adt_type.get(&val.data_type).expect("dangling adt id");

        env.val_type.insert(val.id, Type::Func(args, Box::from(out.clone())));
    }

    for stmt in &prog.definitions {
        match &stmt.val {
            na::Stmt::Assign(tgt, expr) => {
                let stmt_tvar = env.new_tvar();
                let subs = typecheck(&mut env, &expr, &stmt_tvar)?;
                let stmt_type = apply(&subs, stmt_tvar);
                env.sym_type.insert(tgt.val.id(), stmt_type);
            }
            _ => unreachable!()
        }
    }

    for func in &prog.functions {
        check_func(&mut env, func)?;
    }

    Ok(env)
}

fn check_func(env: &mut Environment, func: &na::FuncNode) -> Result<bool, TypeErr> {
    let mut arg_types = Vec::new();
    for arg in &func.val.args {
        let arg_tvar = env.new_tvar();
        env.sym_type.insert(*arg, arg_tvar.clone());
        arg_types.push(Box::from(arg_tvar));
    }
    let ret_tvar = env.new_tvar();
    let fn_type = Type::Func(arg_types, Box::from(ret_tvar.clone()));
    let body_subs = check_body(env, &func.val.body, &ret_tvar)?;

    let refined_fn_type = apply(&body_subs, fn_type);
    env.apply_subs(&body_subs);

    // it's possible that the function id is already assigned a type from an
    // earlier typecheck if it appeared in a function call
    match env.sym_type.get(&func.val.name) {
        Some(env_fn_type) => {
            match unify(env_fn_type, &refined_fn_type, &func.span) {
                Ok(subs) => {
                    env.apply_subs(&subs);
                }
                Err(type_err) => {
                    return Err(TypeErr {
                        message: String::from("Function definiton incompatible with earlier function call"),
                        span: type_err.span.clone()
                    })
                }
            };
        }
        None => {
            env.sym_type.insert(func.val.name, refined_fn_type);
        }
    };

    Ok(true)
}


fn check_case(env: &mut Environment, case: &na::CaseNode, ty: &Type) -> Result<TSubst, TypeErr> {
    let mut subs = HashMap::new();

    let expr_tvar = env.new_tvar();
    let expr_subs = typecheck(env, &case.val.expr, &expr_tvar).expect("failed typecheck");
    let expr_type = apply(&expr_subs, expr_tvar);
    env.apply_subs(&expr_subs);
    subs.extend(expr_subs);


    // start by analyzing patterns
    let mut pattern_type = String::from("");
    for opt in &case.val.options {
        let opt_pat_type = match env.val_type.get(&opt.val.pattern.val.base).expect("dangling type id") {
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
                return Err(TypeErr {
                    message: format!("case statement has patterns of both types {} and {}", pattern_type, opt_pat_type),
                    span: opt.val.pattern.span.clone()
                })
            }
        }
    }

    let pattern_subs = unify(&expr_type, &Type::ADT(pattern_type), &case.span)?;
    env.apply_subs(&pattern_subs);
    subs.extend(pattern_subs);

    let mut is_unit = false;
    let mut has_expr = false;
    for opt in &case.val.options {
        let pattern_arg_types = match env.val_type.get(&opt.val.pattern.val.base).expect("dangling type id") {
            Type::Func(args, _) => args,
            _ => unreachable!()
        };

        for (arg, ty) in opt.val.pattern.val.args.iter().zip(pattern_arg_types) {
            let arg_type: Type = (**ty).clone();
            env.sym_type.insert(*arg, arg_type);
        }


        match &opt.val.body.val {
            na::CaseBody::Body(body) => {
                let opt_tvar = env.new_tvar();
                let opt_subs = check_body(env, &body, &opt_tvar)?;
                let opt_type = apply(&opt_subs, opt_tvar);
                env.apply_subs(&opt_subs);
                subs.extend(opt_subs);

                match unify(&apply(&subs, (*ty).clone()), &opt_type, &body.span) {
                    Ok(uni_subs) => {
                        env.apply_subs(&uni_subs);
                        subs.extend(uni_subs);
                    }
                    Err(_) => {
                        is_unit = true;
                    }
                }
            }
            na::CaseBody::Expr(expr) => {
                has_expr = true;

                let opt_subs = typecheck(env, &expr, &apply(&subs, (*ty).clone()))?;
                env.apply_subs(&opt_subs);
                subs.extend(opt_subs);
            }
        }
    }

    if is_unit {
        if has_expr {
            return Err(TypeErr {
                message: String::from("Case with expr must have type"),
                span: case.span.clone()
            });
        }
        else {
            let unit_subs = unify(ty, &Type::Unit, &case.span).expect("unreachable");
            subs.extend(unit_subs);
        }
    }

    Ok(subs)
}

fn check_body(env: &mut Environment, body: &na::BodyNode, ty: &Type) -> Result<TSubst, TypeErr> {
    let mut stmt_types = Vec::new();
    let mut subs = HashMap::new();
    for stmt in &body.val.stmts {
        match &stmt.val {
            na::Stmt::Assign(tgt, expr) => {
                match &tgt.val {
                    na::Target::Update(id) => {
                        let sym_type = env.sym_type.get(id).expect("Dangling symbol id").clone();
                        let stmt_subs = typecheck(env, expr, &sym_type)?;
                        env.apply_subs(&stmt_subs);

                        let var_type = apply(&stmt_subs, sym_type);
                        stmt_types.push(var_type);
                        subs.extend(stmt_subs);
                    }
                    _ => {
                        let new_tvar = env.new_tvar();
                        let stmt_subs = typecheck(env, expr, &new_tvar)?;
                        let var_type = apply(&stmt_subs, new_tvar);
                        
                        env.sym_type.insert(tgt.val.id(), var_type.clone());
                        env.apply_subs(&stmt_subs);

                        stmt_types.push(var_type);
                        subs.extend(stmt_subs);
                    }
                }
            }
            na::Stmt::Case(case) => {
                let new_tvar = env.new_tvar();
                let case_subs = check_case(env, case, &new_tvar)?;
                let var_type = apply(&case_subs, new_tvar);

                env.apply_subs(&case_subs);

                stmt_types.push(var_type);
                subs.extend(case_subs);
            }
            // it's annoying that fn call doesn't carry a single expr; we
            // might want to make this change soon
            na::Stmt::FnCall(id, args) => {
                let cloned_args = args.iter().map(|arg| { Box::from(arg.clone()) }).collect();
                let fn_expr = na::ExprNode {
                    val: na::Expr::FnCall(id.clone(), cloned_args),
                    span: stmt.span.clone()
                };

                let new_tvar = env.new_tvar();
                let fn_subs = typecheck(env, &fn_expr, &new_tvar)?;
                let fn_type = apply(&fn_subs, new_tvar);

                env.apply_subs(&fn_subs);

                stmt_types.push(fn_type);
                subs.extend(fn_subs);
            }
        }
    }

    match &body.val.expr {
        Some(expr) => {
            let expr_subs = typecheck(env, &expr, ty)?;
            env.apply_subs(&expr_subs);
            subs.extend(expr_subs);
        }
        None => {
            let last_stmt_type = stmt_types.last().expect("unreachable");
            let stmt_subs = unify(last_stmt_type, ty, &body.span).expect("unreachable");

            env.apply_subs(&stmt_subs);
            subs.extend(stmt_subs);
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
fn typecheck(env: &mut Environment, expr: &na::ExprNode, ty: &Type) -> Result<TSubst, TypeErr> {
    match &expr.val {
        na::Expr::Lit(_) => unify(ty, &int_prim!(), &expr.span),
        na::Expr::Add(left, right) | na::Expr::Subt(left, right) | na::Expr::Mult(left, right) |
        na::Expr::Div(left, right) | na::Expr::Pow(left, right) => {
            let mut subs = unify(ty, &int_prim!(), &expr.span)?;

            let subs1 = typecheck(env, &*left, &int_prim!())?;
            let subs2 = typecheck(env, &*right, &int_prim!())?;
            subs.extend(subs1);
            subs.extend(subs2);
            Ok(subs)
        }
        na::Expr::Eq(left, right) | na::Expr::NotEq(left, right) => {
            let mut subs = unify(ty, &Type::ADT(String::from("Bool")), &expr.span)?;

            let new_tvar = env.new_tvar();
            let subs1 = typecheck(env, &*left, &new_tvar)?;

            let updated_tvar = apply(&subs1, new_tvar);
            let subs2 = typecheck(env, &*right, &updated_tvar)?;

            subs.extend(subs1);
            subs.extend(subs2);
            Ok(subs)
        }
        na::Expr::LtEq(left, right) | na::Expr::GtEq(left, right) | na::Expr::Lt(left, right) |
        na::Expr::Gt(left, right) => {
            let mut subs = unify(ty, &Type::ADT(String::from("Bool")), &expr.span)?;

            let subs1 = typecheck(env, &*left, &int_prim!())?;
            let subs2 = typecheck(env, &*right, &int_prim!())?;
            subs.extend(subs1);
            subs.extend(subs2);
            Ok(subs)
        }

        na::Expr::Id(id) => {
            match env.sym_type.get(&id) {
                Some(sym_type) => {
                    unify(ty, sym_type, &expr.span)
                }
                // if we encounter an id without an id, make a tvar and keep
                // going. we'll verify the type later when we check whatever
                // the id refers to
                None => {
                    let id_tvar = env.new_tvar();
                    env.sym_type.insert(*id, id_tvar.clone());
                    unify(ty, &id_tvar, &expr.span)
                }
            }
        }

        na::Expr::FnCall(id, args) => {
            let mut subs = HashMap::new();
            let mut arg_types = Vec::new();
            for arg in args {
                let arg_tvar = env.new_tvar();
                let arg_subs = typecheck(env, &*arg, &arg_tvar)?;
                arg_types.push(Box::from(apply(&arg_subs, arg_tvar)));
                subs.extend(arg_subs);
            }

            let out_tvar = env.new_tvar();
            let out_subs = unify(&ty, &out_tvar, &expr.span)?;
            let out_type = apply(&out_subs, out_tvar);
            subs.extend(out_subs);

            let fn_type = Type::Func(arg_types, Box::from(out_type));

            let fn_sym_type = match env.sym_type.get(&id) {
                Some(sym) => sym.clone(),
                None => {
                    let fn_tvar = env.new_tvar();
                    env.sym_type.insert(*id, fn_tvar.clone());
                    fn_tvar
                }
            };
            let fn_subs = unify(&fn_sym_type, &fn_type, &expr.span)?;
            subs.extend(fn_subs);

            Ok(subs)
        }

        na::Expr::ADTVal(id, args) => {
            let (val_arg_types, val_out_type) = match env.val_type.get(&id).expect("dangling type id") {
                Type::Func(args, out) => (args.clone(), out.clone()),
                _ => panic!("ADT Value with non-function type")
            };

            let mut subs = HashMap::new();
            for (arg, arg_type) in args.iter().zip(val_arg_types) {
                let arg_subs = typecheck(env, arg, &arg_type)?;
                subs.extend(arg_subs);
            }

            let out_subs = unify(&ty, &val_out_type, &expr.span)?;
            subs.extend(out_subs);

            Ok(subs)
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

fn unify(left: &Type, right: &Type, span: &Span) -> Result<TSubst, TypeErr> {
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
                None
            }
            else {
                let mut subs = HashMap::new();
                for (arg1, arg2) in args1.iter().zip(args2) {
                    let arg_subs = unify(&apply(&subs, *arg1.clone()), &apply(&subs, *arg2.clone()), span)?;
                    subs.extend(arg_subs);
                }

                let out_subs = unify(&apply(&subs, *out1.clone()), &apply(&subs, *out2.clone()), span)?;
                subs.extend(out_subs);

                Some(subs)
            }

        }

        _ => None
    }.ok_or(TypeErr {message: String::from("Unification failed"), span: span.clone()})
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
    let res = unify(&int_prim!(), &int_prim!(), &Span {start: 0, end: 0});
    assert_eq!(res.is_ok(), true);

    let res = unify(&int_prim!(), &Type::Prim(String::from("Float")), &Span {start: 0, end: 0});
    assert_eq!(res.is_ok(), false);
}

#[test]
fn unify_fn() {
    let res = unify(
        &Type::Func(vec![Box::from(Type::TVar(0))], Box::from(Type::TVar(0))),
        &Type::Func(vec![Box::from(int_prim!())], Box::from(int_prim!())),
        &Span {start: 0, end: 0}
    );
    assert_eq!(res.is_ok(), true);
    assert_eq!(*res.expect("").get(&0).expect(""), int_prim!());

    let res = unify(
        &Type::Func(vec![Box::from(Type::TVar(0))], Box::from(Type::TVar(0))),
        &Type::Func(vec![Box::from(int_prim!())], Box::from(Type::ADT(String::from("Bool")))),
        &Span {start: 0, end: 0}
    );
    assert_eq!(res.is_ok(), false);
}
