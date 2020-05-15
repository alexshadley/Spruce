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

use crate::parser;

#[derive(Debug, PartialEq)]
pub enum Expr {
    Add(Box<Expr>, Box<Expr>),
    Mult(Box<Expr>, Box<Expr>),
    Subt(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Pow(Box<Expr>, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    FnCall(SymbolID, Vec<Box<Expr>>),
    Id(SymbolID),
    Lit(f64)
}


#[derive(Debug, PartialEq)]
pub struct Body {
    pub stmts: Vec<Stmt>,
    pub expr: Option<Expr>
}

#[derive(Debug, PartialEq)]
pub struct Case {
    expr: Expr,
    options: Vec<CaseOption>
}

#[derive(Debug, PartialEq)]
pub struct CasePattern {
    pub base: String,
    pub args: Vec<String> 
}

#[derive(Debug, PartialEq)]
pub struct CaseOption {
    pub pattern: CasePattern,
    pub body: CaseBody
}

#[derive(Debug, PartialEq)]
pub enum CaseBody {
    Expr(Expr),
    Body(Body)
}

#[derive(Debug, PartialEq)]
pub enum Valued {
    Expr(Expr),
    Case(Case)
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Assign(Target, Expr),
    FnCall(SymbolID, Vec<Expr>),
    Case(Case)
}

#[derive(Debug, PartialEq)]
pub enum Target {
    Var(SymbolID),
    Mutable(SymbolID),
    Update(SymbolID)
}

#[derive(Debug, PartialEq)]
pub struct Type {
    pub name: String,
    pub options: Vec<TypeOption>
}

#[derive(Debug, PartialEq)]
pub struct TypeOption {
    pub name: String,
    pub args: Vec<String>
}

#[derive(Debug, PartialEq)]
pub struct Func {
    pub name: SymbolID,
    pub args: Vec<String>,
    pub body: Body
}

#[derive(Debug, PartialEq)]
pub struct Prog {
    pub functions: Vec<Func>,
    pub definitions: Vec<Stmt>,
    pub types: Vec<Type>
}


pub fn name_analysis(prog: parser::Prog) -> Result<SymbolTable, String> {
    let type_table = analyze_types(&prog)?;
    let mut sym_table = collect_decls(prog)?;
    sym_table.pop_layer();

    Ok(sym_table)
}

type SymbolID = u32;

#[derive(Debug, PartialEq)]
pub enum SymbolType {
    Const,
    Mutable,
    Function
}

#[derive(Debug, PartialEq)]
pub struct Symbol {
    id: SymbolID,
    name: String,
    sym_type: SymbolType
}

type SymbolLayer = HashMap<String, Symbol>;

#[derive(Debug, PartialEq)]
pub struct SymbolTable {
    next_id: SymbolID,
    layers: Vec<SymbolLayer>,
    store: HashMap<SymbolID, Symbol>
}

impl SymbolTable {
    fn new() -> Self {
        SymbolTable { next_id: 0, layers: vec![], store: HashMap::new() }
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
        let ret = self.layers.first_mut().and_then(|layer| {
            let symbol = Symbol { id: id, name: name.clone(), sym_type: sym_type };
            layer.insert(name.clone(), symbol);
            Some(id)
        });
        self.next_id += 1;

        ret
    }

    fn lookup(&self, name: &String) -> Option<&Symbol> {
        for layer in &self.layers {
            match layer.get(name) {
                Some(sym) => {
                    return Option::Some(sym);
                }
                None => ()
            }
        }

        None
    }

    fn conflicts(&self, name: &String) -> bool {
        for layer in &self.layers {
            if layer.get(name).is_some() {
                return true;
            }
        }

        false
    }
}

/// collects top-level name declarations
fn collect_decls(prog: parser::Prog) -> Result<SymbolTable, String> {
    let mut table = SymbolTable::new();
    table.push_layer();

    for var in &prog.definitions {
        match var {
            parser::Stmt::Assign(tgt, _) => {
                match tgt {
                    parser::Target::Var(name) => {
                        if table.conflicts(name) {
                            return Err(String::from(format!("'{}' declared twice", name)));
                        }
                        table.attempt_insert(name, SymbolType::Const);
                    }
                    parser::Target::Mutable(name) => {
                        if table.conflicts(name) {
                            return Err(String::from(format!("'{}' declared twice", name)));
                        }
                        table.attempt_insert(name, SymbolType::Mutable);
                    }
                    parser::Target::Update(_) => {
                        return Err(String::from(format!("Updates not allowed in program level-statements")));
                    }
                }
            }
            _ => unreachable!()
        }
    }

    for func in &prog.functions {
        if table.conflicts(&func.name) {
            return Err(String::from(format!("'{}' declared twice", func.name)));
        }
        table.attempt_insert(&func.name, SymbolType::Function);
    }
    
    Ok(table)
}

fn check_stmt(table: &mut SymbolTable, stmt: parser::Stmt) -> Result<Stmt, String> {
    match(stmt) {
        parser::Stmt::Assign(tgt, expr) => {
            let new_tgt = check_target(table, tgt)?;
            let new_expr = check_expr(table, expr)?;

            Ok(Stmt::Assign(new_tgt, new_expr))
        }
        parser::Stmt::Case(case) => {
            unimplemented!()
        }
        parser::Stmt::FnCall(name, args) => {
            unimplemented!()
        }
    }
}

fn check_target(table: &mut SymbolTable, tgt: parser::Target) -> Result<Target, String> {
    match tgt {
        parser::Target::Var(name) => {
            let id_result = table.attempt_insert(&name, SymbolType::Const);
            match id_result {
                Some(id) => Ok(Target::Var(id)),
                None => {
                    Err(String::from(format!("'{}' declared twice", name)))
                }
            }
        }
        parser::Target::Mutable(name) => {
            let id_result = table.attempt_insert(&name, SymbolType::Const);
            match id_result {
                Some(id) => Ok(Target::Var(id)),
                None => Err(String::from(format!("'{}' declared twice", name)))
            }
        }
        parser::Target::Update(name) => {
            let sym_result = table.lookup(&name);
            match sym_result {
                Some(sym) => Ok(Target::Update(sym.id)),
                None => Err(String::from(format!("'{}' not declared before attempting update", name)))
            }
        }
    }
}

fn check_expr(table: &SymbolTable, expr: parser::Expr) -> Result<Expr, String> {
    match expr {
        parser::Expr::Id(name) => {
            match table.lookup(&name) {
                Some(sym) => Ok(Expr::Id(sym.id)),
                None => Err(String::from(format!("'{}' used but not declared", name)))
            }
        }

        parser::Expr::Add(l, r) | parser::Expr::Subt(l, r) | parser::Expr::Mult(l, r) |
        parser::Expr::Div(l, r) | parser::Expr::Pow(l, r) => {
            let left = check_expr(table, *l)?;
            let right = check_expr(table, *r)?;
            Ok(Expr::Add(Box::from(left), Box::from(right)))
        }

        parser::Expr::FnCall(fn_name, args) => {
            let fn_id = match table.lookup(&fn_name) {
                Some(sym) => {
                    if sym.sym_type != SymbolType::Function {
                        return Err(String::from(format!("'{}' is not a function", fn_name)));
                    }
                    else {
                        sym.id
                    }
                }
                None => {
                    return Err(String::from(format!("'{}' used but not declared", fn_name)));
                }
            };

            let mut checked_args = Vec::new();
            for arg in args {
                let checked = check_expr(table, *arg)?;
                checked_args.push(Box::from(checked));
            }

            Ok(Expr::FnCall(fn_id, checked_args))
        }
    }
}

#[derive(Debug, PartialEq)]
struct ADTValue {
    name: String,
    args: Vec<String>,
    data_type: String
}

#[derive(Debug, PartialEq)]
struct ADT {
    name: String,
    values: Vec<ADTValue>
}

#[derive(Debug, PartialEq)]
struct TypeTable {
    primitives: HashSet<String>,
    types: HashMap<String, ADT>,
    // directory of values and the types they belongto
    values: HashMap<String, String>
}


impl TypeTable {
    fn new() -> Self {
        // TODO: figure out the proper way to do this in rust
        let primitives = vec![String::from("Int"), String::from("Float")];

        TypeTable {primitives: HashSet::from_iter(primitives), types: HashMap::default(), values: HashMap::default()}
    }

    fn add_type(&mut self, dt: ADT) {
        self.types.insert(dt.name.clone(), dt);
    }

    fn add_value(&mut self, val: ADTValue) {
        let mut r = self.types.get_mut(&val.data_type);
        match r {
            Some(adt) => {
                self.values.insert(val.name.clone(), val.data_type.clone());
                adt.values.push(val);
            }
            None => unreachable!()
        }
    }

    fn get_type(&self, name: &String) -> Option<&ADT> {
        self.types.get(name)
    }

    /*fn getTypeMut(&mut self, name: &'a String) -> Option<&'a mut ADT> {
        self.types.get_mut(name)
    }*/

    fn has_type(&self, name: &String) -> bool {
        self.types.contains_key(name) || self.primitives.contains(name)
    }
    
    fn has_value(&self, name: &String) -> bool {
        self.values.contains_key(name)
    }
}

/// Makes two passes, first over types and second over their values this is
/// because values might contain other ADTs
fn analyze_types(prog: & parser::Prog) -> Result<TypeTable, String>{
    let mut type_table = TypeTable::new();

    for t in &prog.types {
        if type_table.has_type(&t.name) {
            return Err(String::from(format!("{} declared twice", t.name)))
        }

        if !t.name.chars().next().unwrap().is_uppercase() {
            return Err(String::from(format!("{} is an invalid type name: types must be uppercase", t.name)))
        }

        let new_type = ADT {name: t.name.clone(), values: vec![] };
        type_table.add_type(new_type)
    }

    for t in &prog.types {
        for v in &t.options {
            if type_table.has_value(&v.name) {
                return Err(String::from(format!("value declared twice: {}", v.name)))
            }
            for arg in &v.args {
                if !type_table.has_type(&arg) {
                    return Err(String::from(format!("type does not exist: {}", arg)));
                }
            }

            match type_table.get_type(&t.name) {
                Some(adt) => {
                    let new_val = ADTValue {name: v.name.clone(), args: v.args.clone(), data_type: adt.name.clone()};
                    type_table.add_value(new_val);
                }
                None => {
                    unreachable!()
                }
            }
        }
    }

    Ok(type_table)
}
