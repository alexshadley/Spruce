/*
Name analysis:
0. make sure names follow rules (types are upper, fns and vars are lower, etc)
1. make sure we don't use names we haven't declared yet
2. make sure we don't re-define anything
3. build the symbol table
*/

use std::collections::HashMap;
use std::collections::HashSet;
use std::iter::FromIterator;

use crate::error::{NameErr};

use crate::parser;
use crate::parser::{Span};


fn double_decl(name: &String, span: Span) -> NameErr {
    NameErr{
        message: String::from(format!("'{}' declared twice", name)),
        span: span
    }
}

fn undeclared(name: &String, span: Span) -> NameErr {
    NameErr{
        message: String::from(format!("'{}' used but not declared", name)),
        span: span
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Add(Box<ExprNode>, Box<ExprNode>),
    Mult(Box<ExprNode>, Box<ExprNode>),
    Subt(Box<ExprNode>, Box<ExprNode>),
    Div(Box<ExprNode>, Box<ExprNode>),
    Pow(Box<ExprNode>, Box<ExprNode>),
    Mod(Box<ExprNode>, Box<ExprNode>),
    FnCall(SymbolID, Vec<Box<ExprNode>>),
    Id(SymbolID),
    ADTVal(ADTValID, Vec<Box<ExprNode>>),
    Lit(f64),
    Eq(Box<ExprNode>, Box<ExprNode>),
    NotEq(Box<ExprNode>, Box<ExprNode>),
    LtEq(Box<ExprNode>, Box<ExprNode>),
    GtEq(Box<ExprNode>, Box<ExprNode>),
    Lt(Box<ExprNode>, Box<ExprNode>),
    Gt(Box<ExprNode>, Box<ExprNode>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExprNode {
    pub val: Expr,
    pub span: Span
}

#[derive(Debug, PartialEq)]
pub struct Body {
    pub stmts: Vec<StmtNode>,
    pub expr: Option<ExprNode>
}

#[derive(Debug, PartialEq)]
pub struct BodyNode {
    pub val: Body,
    pub span: Span
}

#[derive(Debug, PartialEq)]
pub struct Case {
    pub id: CaseID,
    pub expr: ExprNode,
    pub options: Vec<CaseOptionNode>
}

#[derive(Debug, PartialEq)]
pub struct CaseNode {
    pub val: Case,
    pub span: Span
}

#[derive(Debug, PartialEq)]
pub struct CasePattern {
    pub base: ADTValID,
    pub args: Vec<SymbolID> 
}

#[derive(Debug, PartialEq)]
pub struct CasePatternNode {
    pub val: CasePattern,
    pub span: Span
}

#[derive(Debug, PartialEq)]
pub struct CaseOption {
    pub pattern: CasePatternNode,
    pub body: CaseBodyNode
}

#[derive(Debug, PartialEq)]
pub struct CaseOptionNode {
    pub val: CaseOption,
    pub span: Span
}

#[derive(Debug, PartialEq)]
pub enum CaseBody {
    Expr(ExprNode),
    Body(BodyNode)
}

#[derive(Debug, PartialEq)]
pub struct CaseBodyNode {
    pub val: CaseBody,
    pub span: Span
}

#[derive(Debug, PartialEq)]
pub enum Valued {
    Expr(ExprNode),
    Case(CaseNode)
}

#[derive(Debug, PartialEq)]
pub struct ValuedNode {
    pub val: Valued,
    pub span: Span
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Assign(TargetNode, ExprNode),
    FnCall(SymbolID, Vec<ExprNode>),
    Case(CaseNode)
}

#[derive(Debug, PartialEq)]
pub struct StmtNode {
    pub val: Stmt,
    pub span: Span
}

#[derive(Debug, PartialEq)]
pub enum Target {
    Var(SymbolID),
    Mutable(SymbolID),
    Update(SymbolID)
}

impl Target {
    pub fn id(&self) -> SymbolID {
        match self {
            Target::Var(id) | Target::Mutable(id) |
            Target::Update(id) => id.clone()
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct TargetNode {
    pub val: Target,
    pub span: Span
}

#[derive(Debug, PartialEq)]
pub struct Type {
    pub name: String,
    pub options: Vec<TypeOptionNode>
}

#[derive(Debug, PartialEq)]
pub struct TypeNode {
    pub val: Type,
    pub span: Span
}

#[derive(Debug, PartialEq)]
pub struct TypeOption {
    pub name: String,
    pub args: Vec<String>
}

/*#[derive(Debug, PartialEq)]
pub struct TypeIdentifier {
    pub name: String,
    pub args: Vec<Box<TypeIdentifier>>
}*/

#[derive(Debug, PartialEq)]
pub struct TypeOptionNode {
    pub val: TypeOption,
    pub span: Span
}

#[derive(Debug, PartialEq)]
pub struct Func {
    pub name: SymbolID,
    pub args: Vec<SymbolID>,
    pub body: BodyNode
}

#[derive(Debug, PartialEq)]
pub struct FuncNode {
    pub val: Func,
    pub span: Span
}

#[derive(Debug, PartialEq)]
pub struct Prog {
    pub functions: Vec<FuncNode>,
    pub definitions: Vec<StmtNode>,
    pub types: Vec<TypeNode>,
    pub symbol_table: SymbolTable,
    pub type_table: TypeTableExt,

    pub bool_id: ADTID
}


pub fn name_analysis(prog: parser::Prog) -> Result<Prog, NameErr> {
    let (types, type_table) = analyze_types(&prog)?;
    let (mut sym_table, fn_ids, targets) = collect_decls(&prog)?;

    let mut defs = Vec::new();
    for (def, target) in prog.definitions.iter().zip(targets.into_iter()) {
        defs.push(check_global(&mut sym_table, &type_table, def, target)?);
    }

    let mut funcs = Vec::new();
    for (func, id) in prog.functions.iter().zip(fn_ids.into_iter()) {
        funcs.push(check_function(&mut sym_table, &type_table, func, id)?);
    }
    
    sym_table.pop_layer();

    let bool_id = type_table.get_type(&String::from("Bool")).expect("Could not find Bool id").id;
    let out_prog = Prog {functions: funcs, definitions: defs, types: types, symbol_table: sym_table, type_table: type_table.to_ext(), bool_id: bool_id};
    Ok(out_prog)
}

pub type SymbolID = u32;
pub type CaseID = u32;

#[derive(Debug, PartialEq)]
pub enum SymbolType {
    Const,
    Mutable,
    Function
}

#[derive(Debug, PartialEq)]
pub struct Symbol {
    pub id: SymbolID,
    pub name: String,
    pub sym_type: SymbolType
}

type SymbolLayer = HashMap<String, Symbol>;

#[derive(Debug, PartialEq)]
pub struct SymbolTable {
    next_id: SymbolID,
    next_case_id: CaseID,
    layers: Vec<SymbolLayer>,
    pub store: HashMap<SymbolID, Symbol>
}

impl SymbolTable {
    fn new() -> Self {
        SymbolTable { next_id: 0, next_case_id: 0, layers: vec![], store: HashMap::new() }
    }

    fn push_layer(&mut self) {
        let new_layer = HashMap::new();
        self.layers.push(new_layer);
    }

    fn pop_layer(&mut self) {
        self.layers.pop().map(|mut layer| {
            for (_, v) in layer.drain() {
                self.store.insert(v.id, v);
            }
        });
    }

    /// Returns SymbolID if insert was successful
    fn attempt_insert(&mut self, name: &String, sym_type: SymbolType) -> Option<SymbolID> {
        if self.conflicts(name) {
            return None;
        }

        let id = self.next_id;
        let ret = self.layers.last_mut().and_then(|layer| {
            let symbol = Symbol { id: id, name: name.clone(), sym_type: sym_type };
            layer.insert(name.clone(), symbol);
            Some(id)
        });
        self.next_id += 1;

        ret
    }

    fn lookup(&self, name: &String) -> Option<&Symbol> {
        for layer in self.layers.iter().rev() {
            match layer.get(name) {
                Some(sym) => {
                    return Option::Some(sym);
                }
                None => ()
            }
        }

        None
    }

    pub fn lookup_id(&self, id: &SymbolID) -> Option<&Symbol> {
        self.store.get(id)
    }

    // TODO: allow variable shadowing
    fn conflicts(&self, name: &String) -> bool {
        for layer in self.layers.iter().rev() {
            if layer.get(name).is_some() {
                return true;
            }
        }

        false
    }

    fn new_case_id(&mut self) -> CaseID {
        self.next_case_id += 1;
        self.next_case_id - 1
    }
}

/// collects top-level name declarations
fn collect_decls(prog: &parser::Prog) -> Result<(SymbolTable, Vec<SymbolID>, Vec<TargetNode>), NameErr> {
    let mut table = SymbolTable::new();
    table.push_layer();

    let mut fn_ids = Vec::new();
    for func in &prog.functions {
        if table.conflicts(&func.val.name) {
            return Err(double_decl(&func.val.name, func.span.clone()));
        }
        let id = table.attempt_insert(&func.val.name, SymbolType::Function).expect("unreachable");
        fn_ids.push(id);
    }

    let mut tgts = Vec::new();
    for var in &prog.definitions {
        let tgt_val = match &var.val {
            parser::Stmt::Assign(tgt, _) => {
                match &tgt.val {
                    parser::Target::Var(name) => {
                        if table.conflicts(name) {
                            return Err(double_decl(name, tgt.span.clone()));
                        }
                        let id = table.attempt_insert(name, SymbolType::Const).expect("unreachable");
                        Target::Var(id)
                    }
                    parser::Target::Mutable(name) => {
                        if table.conflicts(name) {
                            return Err(double_decl(name, tgt.span.clone()));
                        }
                        let id = table.attempt_insert(name, SymbolType::Mutable).expect("unreachable");
                        Target::Mutable(id)
                    }
                    parser::Target::Update(_) => {
                        return Err(NameErr {
                            message: String::from(format!("Updates not allowed in program level-statements")),
                            span: var.span.clone()
                        });
                    }
                }
            }
            _ => unreachable!()
        };

        tgts.push(TargetNode {
            val: tgt_val,
            span: var.span.clone()
        });
    }
    
    Ok((table, fn_ids, tgts))
}

fn check_global(table: &mut SymbolTable, types: &TypeTable, stmt: &parser::StmtNode, tgt: TargetNode) -> Result<StmtNode, NameErr> {
    match &stmt.val {
        parser::Stmt::Assign(_, expr) => {
            let new_expr = check_expr(table, types, expr)?;

            Ok(StmtNode {
                val: Stmt::Assign(tgt, new_expr),
                span: stmt.span.clone()
            })
        }
        _ => unreachable!()
    }
}

fn check_function(table: &mut SymbolTable, types: &TypeTable, func: &parser::FuncNode, id: SymbolID) -> Result<FuncNode, NameErr> {
    table.push_layer();

    let mut arg_symbols = Vec::new();
    for arg in &func.val.args {
        match table.attempt_insert(&arg, SymbolType::Const) {
            Some(id) => {
                arg_symbols.push(id);
            }
            None => {
                // TODO: implement variable shadowing with arguments
                return Err(double_decl(arg, func.span.clone()));
            }
        }
    }

    let body = check_body(table, types, &func.val.body)?;

    table.pop_layer();

    Ok(FuncNode {
        val: Func {name: id, args: arg_symbols, body: body},
        span: func.span.clone()
    })
}

fn check_body(table: &mut SymbolTable, types: &TypeTable, body: &parser::BodyNode) -> Result<BodyNode, NameErr> {
    let mut stmts = Vec::new();
    for stmt in &body.val.stmts {
        stmts.push(check_stmt(table, types, stmt)?);
    }

    let expr = match &body.val.expr {
        Some(e) => Some(check_expr(table, types, &e)?),
        None => None
    };

    Ok(BodyNode {
        val: Body {stmts: stmts, expr: expr},
        span: body.span.clone()
    })
}

fn check_stmt(table: &mut SymbolTable, types: &TypeTable, stmt: &parser::StmtNode) -> Result<StmtNode, NameErr> {
    let stmt_val = match &stmt.val {
        parser::Stmt::Assign(tgt, expr) => {
            let new_tgt = check_target(table, tgt)?;
            let new_expr = check_expr(table, types, expr)?;

            Stmt::Assign(new_tgt, new_expr)
        }
        parser::Stmt::Case(case) => {
            let id = table.new_case_id();

            let expr = check_expr(table, types, &case.val.expr)?;

            let mut options = Vec::new();
            for opt in &case.val.options {
                options.push(check_case_option(table, types, opt)?);
            }

            Stmt::Case(CaseNode {
                val: Case {id: id, expr: expr, options: options},
                span: case.span.clone()
            })
        }
        parser::Stmt::FnCall(name, args) => {
            match table.lookup(&name) {
                Some(sym) => {
                    let mut checked_args = Vec::new();
                    for arg in args {
                        let checked = check_expr(table, types, &arg)?;
                        checked_args.push(checked);
                    }

                    Stmt::FnCall(sym.id, checked_args)
                }
                None => {
                    return Err(undeclared(name, stmt.span.clone()));
                }
            }
        }
    };

    Ok(StmtNode {
        val: stmt_val,
        span: stmt.span.clone()
    })
}

fn check_case_option(table: &mut SymbolTable, types: &TypeTable, opt: &parser::CaseOptionNode) -> Result<CaseOptionNode, NameErr> {
    table.push_layer();
    let pattern = check_case_pattern(table, types, &opt.val.pattern)?;
    let body_val = match &opt.val.body.val {
        parser::CaseBody::Body(body) => CaseBody::Body(check_body(table, types, &body)?),
        parser::CaseBody::Expr(expr) => CaseBody::Expr(check_expr(table, types, &expr)?)
    };
    let body = CaseBodyNode {
        val: body_val,
        span: opt.val.body.span.clone()
    };
    table.pop_layer();

    Ok(CaseOptionNode {
        val: CaseOption {pattern: pattern, body: body },
        span: opt.span.clone()
    })
}

fn check_case_pattern(table: &mut SymbolTable, types: &TypeTable, pattern: &parser::CasePatternNode) -> Result<CasePatternNode, NameErr> {
    let id = match types.get_value(&pattern.val.base) {
        Some(val) => {
            val.id
        }
        None => {
            return Err(NameErr {
                message: String::from(format!("'{}' is not an ADT value", pattern.val.base)),
                span: pattern.span.clone()
            });
        }
    };

    let mut arg_symbols = Vec::new();
    for arg in &pattern.val.args {
        match table.attempt_insert(&arg, SymbolType::Const) {
            Some(id) => {
                arg_symbols.push(id);
            }
            None => {
                // TODO: implement variable shadowing with arguments
                return Err(double_decl(arg, pattern.span.clone()));
            }
        }
    }


    Ok(CasePatternNode {
        val: CasePattern {base: id, args: arg_symbols},
        span: pattern.span.clone()
    })
}

fn check_target(table: &mut SymbolTable, tgt: &parser::TargetNode) -> Result<TargetNode, NameErr> {
    let tgt_val = match &tgt.val {
        parser::Target::Var(name) => {
            let id_result = table.attempt_insert(name, SymbolType::Const);
            match id_result {
                Some(id) => Ok(Target::Var(id)),
                None => {
                    Err(double_decl(name, tgt.span.clone()))
                }
            }
        }
        parser::Target::Mutable(name) => {
            let id_result = table.attempt_insert(&name, SymbolType::Mutable);
            match id_result {
                Some(id) => Ok(Target::Mutable(id)),
                None => Err(double_decl(name, tgt.span.clone()))

            }
        }
        parser::Target::Update(name) => {
            let sym_result = table.lookup(&name);
            match sym_result {
                Some(sym) => {
                    match sym.sym_type {
                        SymbolType::Mutable => Ok(Target::Update(sym.id)),
                        _ => Err(NameErr {
                            message: String::from(format!("attempt to update non-mutable '{}'", name)),
                            span: tgt.span.clone()
                        })
                    }
                }
                None => Err(NameErr {
                    message: String::from(format!("'{}' not declared before attempting update", name)),
                    span: tgt.span.clone()
                })
            }
        }
    }?;

    Ok(TargetNode {
        val: tgt_val,
        span: tgt.span.clone()
    })
}

fn check_expr(table: &SymbolTable, types: &TypeTable, expr: &parser::ExprNode) -> Result<ExprNode, NameErr> {
    let expr_val = match &expr.val {
        parser::Expr::Id(name) => {
            match (table.lookup(&name), types.get_value(&name)) {
                (Some(sym), _) => Ok(Expr::Id(sym.id)),
                (_, Some(val)) => Ok(Expr::ADTVal(val.id, vec![])),
                (None, None) => Err(undeclared(name, expr.span.clone()))
            }
        }

        parser::Expr::Lit(val) => Ok(Expr::Lit(*val)),

        parser::Expr::Add(l, r) => {
            let left = check_expr(table, types, &*l)?;
            let right = check_expr(table, types, &*r)?;
            Ok(Expr::Add(Box::from(left), Box::from(right)))
        }
        parser::Expr::Subt(l, r) => {
            let left = check_expr(table, types, &*l)?;
            let right = check_expr(table, types, &*r)?;
            Ok(Expr::Subt(Box::from(left), Box::from(right)))
        }
        parser::Expr::Mult(l, r) => {
            let left = check_expr(table, types, &*l)?;
            let right = check_expr(table, types, &*r)?;
            Ok(Expr::Mult(Box::from(left), Box::from(right)))
        }
        parser::Expr::Div(l, r) => {
            let left = check_expr(table, types, &*l)?;
            let right = check_expr(table, types, &*r)?;
            Ok(Expr::Div(Box::from(left), Box::from(right)))
        }
        parser::Expr::Pow(l, r) => {
            let left = check_expr(table, types, &*l)?;
            let right = check_expr(table, types, &*r)?;
            Ok(Expr::Pow(Box::from(left), Box::from(right)))
        }
        parser::Expr::Mod(l, r) => {
            let left = check_expr(table, types, &*l)?;
            let right = check_expr(table, types, &*r)?;
            Ok(Expr::Mod(Box::from(left), Box::from(right)))
        }
        parser::Expr::Eq(l, r) => {
            let left = check_expr(table, types, &*l)?;
            let right = check_expr(table, types, &*r)?;
            Ok(Expr::Eq(Box::from(left), Box::from(right)))
        }
        parser::Expr::NotEq(l, r) => {
            let left = check_expr(table, types, &*l)?;
            let right = check_expr(table, types, &*r)?;
            Ok(Expr::NotEq(Box::from(left), Box::from(right)))
        }
        parser::Expr::LtEq(l, r) => {
            let left = check_expr(table, types, &*l)?;
            let right = check_expr(table, types, &*r)?;
            Ok(Expr::LtEq(Box::from(left), Box::from(right)))
        }
        parser::Expr::GtEq(l, r) => {
            let left = check_expr(table, types, &*l)?;
            let right = check_expr(table, types, &*r)?;
            Ok(Expr::GtEq(Box::from(left), Box::from(right)))
        }
        parser::Expr::Lt(l, r) => {
            let left = check_expr(table, types, &*l)?;
            let right = check_expr(table, types, &*r)?;
            Ok(Expr::Lt(Box::from(left), Box::from(right)))
        }
        parser::Expr::Gt(l, r) => {
            let left = check_expr(table, types, &*l)?;
            let right = check_expr(table, types, &*r)?;
            Ok(Expr::Gt(Box::from(left), Box::from(right)))
        }

        parser::Expr::FnCall(fn_name, args) => {
            match (table.lookup(&fn_name), types.get_value(&fn_name)) {

                (Some(sym), _) => {
                    let mut checked_args = Vec::new();
                    for arg in args {
                        let checked = check_expr(table, types, &*arg)?;
                        checked_args.push(Box::from(checked));
                    }

                    Ok(Expr::FnCall(sym.id, checked_args))
                }

                (_, Some(value)) => {
                    let mut checked_args = Vec::new();
                    for arg in args {
                        let checked = check_expr(table, types, &*arg)?;
                        checked_args.push(Box::from(checked));
                    }

                    Ok(Expr::ADTVal(value.id, checked_args))
                }

                (None, None) => {
                    return Err(undeclared(fn_name, expr.span.clone()));
                }
            }
        }


    }?;

    Ok(ExprNode {
        val: expr_val,
        span: expr.span.clone()
    })
}

pub type ADTValID = u32;
pub type ADTID = u32;
pub type TParamID = u32;

#[derive(Debug, PartialEq)]
pub struct TParam {
    pub id: TParamID,
    pub name: String
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeID {
    TParam(TParamID),
    ADT(ADTID, Vec<Box<TypeID>>),
    Prim(String),
}

#[derive(Debug, PartialEq)]
pub struct ADTValue {
    pub id: ADTValID,
    pub name: String,
    pub args: Vec<TypeID>,
    pub data_type: ADTID
}

#[derive(Debug, PartialEq)]
pub struct ADT {
    pub id: ADTID,
    pub type_params: Vec<TParamID>,
    pub name: String
}

#[derive(Debug, PartialEq)]
struct TypeTable {
    next_type_id: ADTID,
    next_val_id: ADTValID,
    next_tparam_id: TParamID,
    primitives: HashSet<String>,
    types: HashMap<String, ADT>,
    values: HashMap<String, ADTValue>,
    type_params: HashMap<TParamID, TParam>
}

/// Version of type table that is exported. Note that values are indexed by
/// SymbolID. The approach to type symbols needs to be better thought out
#[derive(Debug, PartialEq)]
pub struct TypeTableExt {
    pub types: HashMap<ADTID, ADT>,
    pub values: HashMap<ADTValID, ADTValue>,
    pub primitives: HashSet<String>
}

impl TypeTable {
    fn new() -> Self {
        // TODO: figure out the proper way to do this in rust
        let primitives = vec![String::from("Int"), String::from("Float")];

        TypeTable {
            next_type_id: 0,
            next_val_id: 0,
            next_tparam_id: 0,
            primitives: HashSet::from_iter(primitives),
            types: HashMap::default(),
            values: HashMap::default(),
            type_params: HashMap::default()
        }
    }

    fn add_type(&mut self, name: &String, params: Vec<TParamID>) {
        let new_adt = ADT {name: name.clone(), id: self.next_type_id, type_params: params};
        self.next_type_id += 1;
        self.types.insert(name.clone(), new_adt);
    }

    fn add_value(&mut self, name: &String, args: &Vec<TypeID>, data_type: &String) {
        let mut r = self.types.get_mut(data_type);
        match r {
            Some(adt) => {
                let new_val = ADTValue {name: name.clone(), args: (*args).clone(), data_type: adt.id, id: self.next_val_id};
                self.next_val_id += 1;
                self.values.insert(new_val.name.clone(), new_val);
            }
            None => unreachable!()
        }
    }

    fn add_tparam(&mut self, name: &String) -> TParamID {
        let new_tparam = TParam {name: name.clone(), id: self.next_tparam_id};
        self.next_tparam_id += 1;
        self.type_params.insert(new_tparam.id, new_tparam);

        self.next_tparam_id - 1
    }

    pub fn get_type(&self, name: &String) -> Option<&ADT> {
        self.types.get(name)
    }

    pub fn get_value(&self, name: &String) -> Option<&ADTValue> {
        self.values.get(name)
    }

    pub fn get_tparam(&self, id: &TParamID) -> Option<&TParam> {
        self.type_params.get(id)
    }

    fn has_type(&self, name: &String) -> bool {
        self.types.contains_key(name) || self.primitives.contains(name)
    }
    
    fn has_value(&self, name: &String) -> bool {
        self.values.contains_key(name)
    }

    fn to_ext(self) -> TypeTableExt {
        TypeTableExt {
            types: self.types.into_iter().map(|(k, v)| {(v.id, v)}).collect(),
            values: self.values.into_iter().map(|(k, v)| {(v.id, v)}).collect(),
            primitives: self.primitives
        }
    }
}

/// Makes two passes, first over types and second over their values this is
/// because values might contain other ADTs
fn analyze_types(prog: & parser::Prog) -> Result<(Vec<TypeNode>, TypeTable), NameErr>{
    let mut type_table = TypeTable::new();

    for t in &prog.types {
        if type_table.has_type(&t.val.name) {
            return Err(double_decl(&t.val.name, t.span.clone()))
        }

        if !t.val.name.chars().next().unwrap().is_uppercase() {
            return Err(NameErr {
                message: String::from(format!("{} is an invalid type name: types must be uppercase", t.val.name)),
                span: t.span.clone()
            });
        }

        let mut params = Vec::new();
        for param in &t.val.type_params {
            let id = type_table.add_tparam(&param);
            params.push(id);
        }
        type_table.add_type(&t.val.name, params);
    }

    for t in &prog.types {
        let type_symbol = type_table.get_type(&t.val.name).expect("unreachable");
        let params: HashMap<String, TParamID> = type_symbol.type_params.iter().map(|id| {
            let tparam = type_table.get_tparam(id).expect("unreachable");
            (tparam.name.clone(), tparam.id)
        }).collect();

        for v in &t.val.options {
            if type_table.has_value(&v.val.name) {
                return Err(double_decl(&v.val.name, v.span.clone()))
            }

            let mut arg_ids = Vec::new();
            for arg in &v.val.args {
                arg_ids.push(
                    check_type_identifier(arg, &params, &type_table, &v.span)?
                );
            }

            type_table.add_value(&v.val.name, &arg_ids, &t.val.name);
        }
    }

    let new_types = prog.types.iter().map(|t| {
        TypeNode {
            val: Type {
                name: t.val.name.clone(),
                options: t.val.options.iter().map(|o| { 
                    TypeOptionNode {
                        val: TypeOption {name: o.val.name.clone(), args: o.val.args.iter().map(|id| {id.name.clone()}).collect() },
                        span: o.span.clone()
                    }
                }).collect()
            },
            span: t.span.clone()
        }
    }).collect();

    Ok((new_types, type_table))
}

fn check_type_identifier(ident: &parser::TypeIdentifier, params: &HashMap<String, TParamID>, type_table: &TypeTable, span: &Span) -> Result<TypeID, NameErr> {
    let mut args = Vec::new();
    for arg in &ident.args {
        args.push(Box::from(check_type_identifier(&**arg, params, type_table, &span)?));
    }

    match (params.get(&ident.name), type_table.types.get(&ident.name), type_table.primitives.get(&ident.name)) {
        (Some(tparam_id), _, _) => {
            Ok(TypeID::TParam(*tparam_id))
        }
        (_, Some(adt), _) => {
            Ok(TypeID::ADT(adt.id, args))
        }
        (_, _, Some(s)) => {
            Ok(TypeID::Prim(s.clone()))
        }
        _ => {
            return Err(NameErr {
                message: String::from(format!("type does not exist: {}", ident.name)),
                span: span.clone()
            });
        }
    }
}
