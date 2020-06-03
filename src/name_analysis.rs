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

use crate::error::{SpruceErr};

use crate::parser;
use crate::parser::{NodeInfo};


fn double_decl(name: &String, info: NodeInfo) -> SpruceErr {
    SpruceErr{
        message: String::from(format!("'{}' declared twice", name)),
        info: info
    }
}

fn undeclared(name: &String, info: NodeInfo) -> SpruceErr {
    SpruceErr{
        message: String::from(format!("'{}' used but not declared", name)),
        info: info
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
    Curry(SymbolID, Vec<Option<Box<ExprNode>>>),
    Id(SymbolID),
    ADTVal(ADTValID, Vec<Box<ExprNode>>),
    Lit(f64),
    StringLit(String),
    Eq(Box<ExprNode>, Box<ExprNode>),
    NotEq(Box<ExprNode>, Box<ExprNode>),
    LtEq(Box<ExprNode>, Box<ExprNode>),
    GtEq(Box<ExprNode>, Box<ExprNode>),
    Lt(Box<ExprNode>, Box<ExprNode>),
    Gt(Box<ExprNode>, Box<ExprNode>),
    Concat(Box<ExprNode>, Box<ExprNode>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExprNode {
    pub val: Expr,
    pub info: NodeInfo
}

#[derive(Debug, PartialEq)]
pub struct Body {
    pub stmts: Vec<StmtNode>,
    pub expr: Option<ExprNode>
}

#[derive(Debug, PartialEq)]
pub struct BodyNode {
    pub val: Body,
    pub info: NodeInfo
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
    pub info: NodeInfo
}

#[derive(Debug, PartialEq)]
pub struct CasePattern {
    pub base: ADTValID,
    pub args: Vec<SymbolID> 
}

#[derive(Debug, PartialEq)]
pub struct CasePatternNode {
    pub val: CasePattern,
    pub info: NodeInfo
}

#[derive(Debug, PartialEq)]
pub struct CaseOption {
    pub pattern: CasePatternNode,
    pub body: CaseBodyNode
}

#[derive(Debug, PartialEq)]
pub struct CaseOptionNode {
    pub val: CaseOption,
    pub info: NodeInfo
}

#[derive(Debug, PartialEq)]
pub enum CaseBody {
    Expr(ExprNode),
    Body(BodyNode)
}

#[derive(Debug, PartialEq)]
pub struct CaseBodyNode {
    pub val: CaseBody,
    pub info: NodeInfo
}

#[derive(Debug, PartialEq)]
pub enum Valued {
    Expr(ExprNode),
    Case(CaseNode)
}

#[derive(Debug, PartialEq)]
pub struct ValuedNode {
    pub val: Valued,
    pub info: NodeInfo
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
    pub info: NodeInfo
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
    pub info: NodeInfo
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeAnnotation {
    Unit,
    ADT(ADTID, Vec<Box<TypeAnnotationNode>>),
    Prim(String),
    TVar(TParamID),
    Func(Vec<Box<TypeAnnotationNode>>, Box<TypeAnnotationNode>)
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeAnnotationNode {
    pub val: TypeAnnotation,
    pub info: NodeInfo
}

#[derive(Debug, PartialEq)]
pub struct InteropFunc {
    pub name: SymbolID,
    pub args: Vec<TypeAnnotationNode>,
    pub out_ann: TypeAnnotationNode
}

#[derive(Debug, PartialEq)]
pub struct InteropFuncNode {
    pub val: InteropFunc,
    pub info: NodeInfo
}

#[derive(Debug, PartialEq)]
pub struct Func {
    pub name: SymbolID,
    pub args: Vec<(SymbolID, Option<TypeAnnotationNode>)>,
    pub body: BodyNode,
    pub out_ann: Option<TypeAnnotationNode>
}

#[derive(Debug, PartialEq)]
pub struct FuncNode {
    pub val: Func,
    pub info: NodeInfo
}

// exists to record types of adts and values we need in codegen
#[derive(Debug, PartialEq, Clone)]
pub struct InternalTypes {
    pub bool_id: ADTID,
    pub maybe_id: ADTID,

    pub list_id: ADTID,
    pub cons_id: ADTValID,
    pub nil_id: ADTValID
}

#[derive(Debug, PartialEq)]
pub struct Prog {
    pub interop_functions: Vec<InteropFuncNode>,
    pub functions: Vec<FuncNode>,
    pub definitions: Vec<StmtNode>,
    pub symbol_table: SymbolTable,
    pub type_table: TypeTableExt,
    pub internal_types: InternalTypes
}


pub fn name_analysis(prog: parser::Prog) -> Result<Prog, SpruceErr> {
    let mut type_table = analyze_types(&prog)?;
    let (mut sym_table, interop_ids, fn_ids, targets) = collect_decls(&prog)?;

    let mut defs = Vec::new();
    for (def, target) in prog.definitions.iter().zip(targets.into_iter()) {
        defs.push(check_global(&mut sym_table, &type_table, def, target)?);
    }

    let mut interops = Vec::new();
    for (interop, id) in prog.interop_functions.iter().zip(interop_ids.into_iter()) {
        interops.push(check_interop(&mut sym_table, &mut type_table, interop, id)?);
    }

    let mut funcs = Vec::new();
    for (func, id) in prog.functions.iter().zip(fn_ids.into_iter()) {
        funcs.push(check_function(&mut sym_table, &mut type_table, func, id)?);
    }
    
    sym_table.pop_layer();

    let internal_types = InternalTypes {
        bool_id: type_table.get_type(&String::from("Bool")).expect("Could not find Bool id").id,
        maybe_id: type_table.get_type(&String::from("Maybe")).expect("Could not find Maybe id").id,
        list_id: type_table.get_type(&String::from("List")).expect("Could not find List id").id,
        cons_id: type_table.get_value(&String::from("Cons")).expect("Could not find Cons id").id,
        nil_id: type_table.get_value(&String::from("Nil")).expect("Could not find Nil id").id,
    };

    let out_prog = Prog {
        interop_functions: interops,
        functions: funcs, 
        definitions: defs,
        symbol_table: sym_table,
        type_table: type_table.to_ext(),
        internal_types: internal_types
    };
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
fn collect_decls(prog: &parser::Prog) -> Result<(SymbolTable, Vec<SymbolID>, Vec<SymbolID>, Vec<TargetNode>), SpruceErr> {
    let mut table = SymbolTable::new();
    table.push_layer();

    let mut interop_ids = Vec::new();
    for func in &prog.interop_functions {
        if table.conflicts(&func.val.name) {
            return Err(double_decl(&func.val.name, func.info.clone()));
        }
        let id = table.attempt_insert(&func.val.name, SymbolType::Function).expect("unreachable");
        interop_ids.push(id);
    }

    let mut fn_ids = Vec::new();
    for func in &prog.functions {
        if table.conflicts(&func.val.name) {
            return Err(double_decl(&func.val.name, func.info.clone()));
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
                            return Err(double_decl(name, tgt.info.clone()));
                        }
                        let id = table.attempt_insert(name, SymbolType::Const).expect("unreachable");
                        Target::Var(id)
                    }
                    parser::Target::Mutable(name) => {
                        if table.conflicts(name) {
                            return Err(double_decl(name, tgt.info.clone()));
                        }
                        let id = table.attempt_insert(name, SymbolType::Mutable).expect("unreachable");
                        Target::Mutable(id)
                    }
                    parser::Target::Update(_) => {
                        return Err(SpruceErr {
                            message: String::from(format!("Updates not allowed in program level-statements")),
                            info: var.info.clone()
                        });
                    }
                }
            }
            _ => unreachable!()
        };

        tgts.push(TargetNode {
            val: tgt_val,
            info: var.info.clone()
        });
    }
    
    Ok((table, interop_ids, fn_ids, tgts))
}

fn check_global(table: &mut SymbolTable, types: &TypeTable, stmt: &parser::StmtNode, tgt: TargetNode) -> Result<StmtNode, SpruceErr> {
    match &stmt.val {
        parser::Stmt::Assign(_, expr) => {
            let new_expr = check_expr(table, types, expr)?;

            Ok(StmtNode {
                val: Stmt::Assign(tgt, new_expr),
                info: stmt.info.clone()
            })
        }
        _ => unreachable!()
    }
}

/// the arg names in an interop definition don't matter (they will never be
/// referenced, since the interop has no body), so we don't add these symbols
/// to the table
fn check_interop(table: &mut SymbolTable, types: &mut TypeTable, func: &parser::InteropFuncNode, id: SymbolID) -> Result<InteropFuncNode, SpruceErr> {
    let mut local_tvars = HashMap::new();

    let mut arg_anns = Vec::new();
    for (_arg, ann) in &func.val.args {
        match ann {
            Some(a) => {
                arg_anns.push(
                    check_annotation(table, types, a, &mut local_tvars)?
                );
            }
            None => {
                return Err(SpruceErr {
                    message: String::from("interop functions must specify all argument types"),
                    info: func.info.clone()
                });
            }
        }
    }

    let checked_out_ann = match &func.val.out_ann {
        Some(ann) => check_annotation(table, types, &ann, &mut local_tvars)?,
        None => {
            return Err(SpruceErr {
                message: String::from("interop functions must specify return types"),
                info: func.info.clone()
            });
        }
    };

    Ok(InteropFuncNode {
        val: InteropFunc {name: id, args: arg_anns, out_ann: checked_out_ann},
        info: func.info.clone()
    })
}

fn check_function(table: &mut SymbolTable, types: &mut TypeTable, func: &parser::FuncNode, id: SymbolID) -> Result<FuncNode, SpruceErr> {
    table.push_layer();

    let mut local_tvars = HashMap::new();

    let mut arg_symbols = Vec::new();
    for (arg, ann) in &func.val.args {
        match table.attempt_insert(&arg, SymbolType::Const) {
            Some(id) => {
                let checked_ann = match ann {
                    Some(a) => {
                        Some(
                            check_annotation(table, types, a, &mut local_tvars)?
                        )
                    }
                    None => None
                };
                arg_symbols.push(
                    (id, checked_ann)
                );
            }
            None => {
                // TODO: implement variable shadowing with arguments
                return Err(double_decl(arg, func.info.clone()));
            }
        }
    }

    let checked_out_ann = match &func.val.out_ann {
        Some(ann) => Some(check_annotation(table, types, &ann, &mut local_tvars)?),
        None => None
    };

    let body = check_body(table, types, &func.val.body)?;

    table.pop_layer();

    Ok(FuncNode {
        val: Func {name: id, args: arg_symbols, body: body, out_ann: checked_out_ann},
        info: func.info.clone()
    })
}

fn check_annotation(table: &mut SymbolTable, types: &mut TypeTable, ann: &parser::TypeAnnotationNode, local_tvars: &mut HashMap<String, TParamID>) -> Result<TypeAnnotationNode, SpruceErr> {
    let ann_info = ann.info.clone();

    let ann_val = match &ann.val {
        parser::TypeAnnotation::Unit => TypeAnnotation::Unit,
        parser::TypeAnnotation::TVar(name) => {
            match local_tvars.get(name) {
                Some(id) => TypeAnnotation::TVar(*id),
                None => {
                    let new_id = types.add_tparam(name);
                    local_tvars.insert(name.clone(), new_id);
                    TypeAnnotation::TVar(new_id)
                }
            }
        }
        parser::TypeAnnotation::ADT(name, subanns) => {
            let mut checked_anns = Vec::new();
            for subann in subanns {
                checked_anns.push(
                    Box::from(check_annotation(table, types, subann, local_tvars)?)
                )
            }

            if types.primitives.contains(name) {
                TypeAnnotation::Prim(name.clone())
            }
            else {
                match types.get_type(&name) {
                    Some(adt) => {
                        TypeAnnotation::ADT(adt.id, checked_anns)
                    }
                    None => {
                        return Err(undeclared(&name, ann_info));
                    }
                }
            }
        }
        parser::TypeAnnotation::Func(args, out) => {
            let mut checked_args = Vec::new();
            for arg in args {
                checked_args.push(
                    Box::from(check_annotation(table, types, arg, local_tvars)?)
                )
            }

            let checked_out = Box::from(
                check_annotation(table, types, out, local_tvars)?
            );
            TypeAnnotation::Func(checked_args, checked_out)
        }
        _ => unimplemented!()
    };

    Ok(TypeAnnotationNode {
        val: ann_val,
        info: ann_info
    })
}

fn check_body(table: &mut SymbolTable, types: &TypeTable, body: &parser::BodyNode) -> Result<BodyNode, SpruceErr> {
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
        info: body.info.clone()
    })
}

fn check_stmt(table: &mut SymbolTable, types: &TypeTable, stmt: &parser::StmtNode) -> Result<StmtNode, SpruceErr> {
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
                info: case.info.clone()
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
                    return Err(undeclared(name, stmt.info.clone()));
                }
            }
        }
    };

    Ok(StmtNode {
        val: stmt_val,
        info: stmt.info.clone()
    })
}

fn check_case_option(table: &mut SymbolTable, types: &TypeTable, opt: &parser::CaseOptionNode) -> Result<CaseOptionNode, SpruceErr> {
    table.push_layer();
    let pattern = check_case_pattern(table, types, &opt.val.pattern)?;
    let body_val = match &opt.val.body.val {
        parser::CaseBody::Body(body) => CaseBody::Body(check_body(table, types, &body)?),
        parser::CaseBody::Expr(expr) => CaseBody::Expr(check_expr(table, types, &expr)?)
    };
    let body = CaseBodyNode {
        val: body_val,
        info: opt.val.body.info.clone()
    };
    table.pop_layer();

    Ok(CaseOptionNode {
        val: CaseOption {pattern: pattern, body: body },
        info: opt.info.clone()
    })
}

fn check_case_pattern(table: &mut SymbolTable, types: &TypeTable, pattern: &parser::CasePatternNode) -> Result<CasePatternNode, SpruceErr> {
    let id = match types.get_value(&pattern.val.base) {
        Some(val) => {
            val.id
        }
        None => {
            return Err(SpruceErr {
                message: String::from(format!("'{}' is not an ADT value", pattern.val.base)),
                info: pattern.info.clone()
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
                return Err(double_decl(arg, pattern.info.clone()));
            }
        }
    }


    Ok(CasePatternNode {
        val: CasePattern {base: id, args: arg_symbols},
        info: pattern.info.clone()
    })
}

fn check_target(table: &mut SymbolTable, tgt: &parser::TargetNode) -> Result<TargetNode, SpruceErr> {
    let tgt_val = match &tgt.val {
        parser::Target::Var(name) => {
            let id_result = table.attempt_insert(name, SymbolType::Const);
            match id_result {
                Some(id) => Ok(Target::Var(id)),
                None => {
                    Err(double_decl(name, tgt.info.clone()))
                }
            }
        }
        parser::Target::Mutable(name) => {
            let id_result = table.attempt_insert(&name, SymbolType::Mutable);
            match id_result {
                Some(id) => Ok(Target::Mutable(id)),
                None => Err(double_decl(name, tgt.info.clone()))

            }
        }
        parser::Target::Update(name) => {
            let sym_result = table.lookup(&name);
            match sym_result {
                Some(sym) => {
                    match sym.sym_type {
                        SymbolType::Mutable => Ok(Target::Update(sym.id)),
                        _ => Err(SpruceErr {
                            message: String::from(format!("attempt to update non-mutable '{}'", name)),
                            info: tgt.info.clone()
                        })
                    }
                }
                None => Err(SpruceErr {
                    message: String::from(format!("'{}' not declared before attempting update", name)),
                    info: tgt.info.clone()
                })
            }
        }
    }?;

    Ok(TargetNode {
        val: tgt_val,
        info: tgt.info.clone()
    })
}

fn check_expr(table: &SymbolTable, types: &TypeTable, expr: &parser::ExprNode) -> Result<ExprNode, SpruceErr> {
    let expr_val = match &expr.val {
        parser::Expr::Id(name) => {
            match (table.lookup(&name), types.get_value(&name)) {
                (Some(sym), _) => Ok(Expr::Id(sym.id)),
                (_, Some(val)) => Ok(Expr::ADTVal(val.id, vec![])),
                (None, None) => Err(undeclared(name, expr.info.clone()))
            }
        }

        parser::Expr::Lit(val) => Ok(Expr::Lit(*val)),

        parser::Expr::StringLit(val) => Ok(Expr::StringLit(val.clone())),

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
        parser::Expr::Concat(l, r) => {
            let left = check_expr(table, types, &*l)?;
            let right = check_expr(table, types, &*r)?;
            Ok(Expr::Concat(Box::from(left), Box::from(right)))
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
                    return Err(undeclared(fn_name, expr.info.clone()));
                }
            }
        }
        parser::Expr::Curry(fn_name, args) => {
            match table.lookup(&fn_name) {
                Some(sym) => {
                    let mut checked_args = Vec::new();
                    for arg in args {
                        match arg {
                            Some(a) => {
                                let checked = check_expr(table, types, &*a)?;
                                checked_args.push(Some(Box::from(checked)));
                            }
                            None => {
                                checked_args.push(None)
                            }
                        }
                    }

                    Ok(Expr::Curry(sym.id, checked_args))
                }

                None => {
                    return Err(undeclared(fn_name, expr.info.clone()));
                }
            }
        }

    }?;

    Ok(ExprNode {
        val: expr_val,
        info: expr.info.clone()
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

#[derive(Debug, PartialEq, Clone)]
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
    pub name: String,
    pub values: Vec<ADTValID>
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
        let primitives = vec![String::from("Int"), String::from("Float"), String::from("Char")];

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
        let new_adt = ADT {name: name.clone(), id: self.next_type_id, type_params: params, values: vec![]};
        self.next_type_id += 1;
        self.types.insert(name.clone(), new_adt);
    }

    fn add_value(&mut self, name: &String, args: &Vec<TypeID>, data_type: &String) {
        let mut adt = self.types.get_mut(data_type).expect("unreachable");
        adt.values.push(self.next_val_id);

        let new_val = ADTValue {name: name.clone(), args: (*args).clone(), data_type: adt.id, id: self.next_val_id};
        self.next_val_id += 1;
        self.values.insert(new_val.name.clone(), new_val);
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
            types: self.types.into_iter().map(|(_k, v)| {(v.id, v)}).collect(),
            values: self.values.into_iter().map(|(_k, v)| {(v.id, v)}).collect(),
            primitives: self.primitives
        }
    }
}

/// Makes two passes, first over types and second over their values this is
/// because values might contain other ADTs
fn analyze_types(prog: & parser::Prog) -> Result<TypeTable, SpruceErr>{
    let mut type_table = TypeTable::new();

    for t in &prog.types {
        if type_table.has_type(&t.val.name) {
            return Err(double_decl(&t.val.name, t.info.clone()))
        }

        if !t.val.name.chars().next().unwrap().is_uppercase() {
            return Err(SpruceErr {
                message: String::from(format!("{} is an invalid type name: types must be uppercase", t.val.name)),
                info: t.info.clone()
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
                return Err(double_decl(&v.val.name, v.info.clone()))
            }

            let mut arg_ids = Vec::new();
            for arg in &v.val.args {
                arg_ids.push(
                    check_type_identifier(arg, &params, &type_table, &v.info)?
                );
            }

            type_table.add_value(&v.val.name, &arg_ids, &t.val.name);
        }
    }

    Ok(type_table)
}

fn check_type_identifier(ident: &parser::TypeIdentifier, params: &HashMap<String, TParamID>, type_table: &TypeTable, info: &NodeInfo) -> Result<TypeID, SpruceErr> {
    let mut args = Vec::new();
    for arg in &ident.args {
        args.push(Box::from(check_type_identifier(&**arg, params, type_table, &info)?));
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
            return Err(SpruceErr {
                message: String::from(format!("type does not exist: {}", ident.name)),
                info: info.clone()
            });
        }
    }
}
