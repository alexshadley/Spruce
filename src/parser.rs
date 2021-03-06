
extern crate pest;

use pest::{Parser};
use pest::iterators::{Pairs, Pair};
use pest::prec_climber::{PrecClimber, Operator, Assoc};
use pest::error::InputLocation;

use crate::error::SpruceErr;


#[derive(Parser)]
#[grammar = "spruce.pest"]
pub struct ExprParser;

lazy_static! {
    static ref PREC_CLIMBER: PrecClimber<Rule> = {
        use Rule::*;
        use Assoc::*;

        PrecClimber::new(vec![
            Operator::new(eq, Left) | Operator::new(not_eq, Left),
            Operator::new(lt_eq, Left) | Operator::new(gt_eq, Left) | Operator::new(lt, Left) | Operator::new(gt, Left),
            Operator::new(modulus, Left),
            Operator::new(add, Left) | Operator::new(subtract, Left),
            Operator::new(multiply, Left) | Operator::new(divide, Left),
            Operator::new(power, Right),
            Operator::new(concat, Left),
            Operator::new(access, Left)
        ])
    };
}

#[derive(Debug, PartialEq, Clone)]
pub struct NodeInfo {
    pub span: Span,
    pub file: String
}

#[derive(Debug, PartialEq, Clone)]
pub struct Span {
    pub start: usize,
    pub end: usize
}

impl Span {
    fn from(sp: pest::Span) -> Self {
        Span {start: sp.start(), end: sp.end() }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Add(Box<ExprNode>, Box<ExprNode>),
    Mult(Box<ExprNode>, Box<ExprNode>),
    Subt(Box<ExprNode>, Box<ExprNode>),
    Div(Box<ExprNode>, Box<ExprNode>),
    Pow(Box<ExprNode>, Box<ExprNode>),
    Mod(Box<ExprNode>, Box<ExprNode>),
    FnCall(String, Vec<Box<ExprNode>>),
    Curry(String, Vec<Option<Box<ExprNode>>>),
    Id(String),
    Lit(f64),
    StringLit(String),
    StructVal(Vec<(String, Box<ExprNode>)>),
    Eq(Box<ExprNode>, Box<ExprNode>),
    NotEq(Box<ExprNode>, Box<ExprNode>),
    LtEq(Box<ExprNode>, Box<ExprNode>),
    GtEq(Box<ExprNode>, Box<ExprNode>),
    Lt(Box<ExprNode>, Box<ExprNode>),
    Gt(Box<ExprNode>, Box<ExprNode>),
    Concat(Box<ExprNode>, Box<ExprNode>),
    Access(Box<ExprNode>, String)
}

#[derive(Debug, PartialEq)]
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
pub struct While {
    pub expr: ExprNode,
    pub body: BodyNode
}

#[derive(Debug, PartialEq)]
pub struct WhileNode {
    pub val: While,
    pub info: NodeInfo
}

#[derive(Debug, PartialEq)]
pub struct Case {
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
    pub base: String,
    pub args: Vec<String> 
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

// this will allow case statements to be assigned to variables, when 
// that feature is impelmented
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
    FnCall(String, Vec<ExprNode>),
    Case(CaseNode),
    While(WhileNode)
}

#[derive(Debug, PartialEq)]
pub struct StmtNode {
    pub val: Stmt,
    pub info: NodeInfo
}

#[derive(Debug, PartialEq)]
pub enum Target {
    Var(String),
    Mutable(String),
    Update(String)
}

#[derive(Debug, PartialEq)]
pub struct TargetNode {
    pub val: Target,
    pub info: NodeInfo
}

#[derive(Debug, PartialEq)]
pub struct Type {
    pub name: String,
    pub type_params: Vec<String>,
    pub options: Vec<TypeOptionNode>
}

#[derive(Debug, PartialEq)]
pub struct TypeNode {
    pub val: Type,
    pub info: NodeInfo
}

#[derive(Debug, PartialEq)]
pub struct TypeOption {
    pub name: String,
    pub args: Vec<TypeIdentifier>
}

#[derive(Debug, PartialEq)]
pub struct TypeIdentifier {
    pub name: String,
    pub args: Vec<Box<TypeIdentifier>>
}

#[derive(Debug, PartialEq)]
pub struct TypeOptionNode {
    pub val: TypeOption,
    pub info: NodeInfo
}

#[derive(Debug, PartialEq)]
pub enum TypeAnnotation {
    Unit,
    ADT(String, Vec<Box<TypeAnnotationNode>>),
    TVar(String),
    Func(Vec<Box<TypeAnnotationNode>>, Box<TypeAnnotationNode>)
}

#[derive(Debug, PartialEq)]
pub struct TypeAnnotationNode {
    pub val: TypeAnnotation,
    pub info: NodeInfo
}

#[derive(Debug, PartialEq)]
pub struct InteropFunc {
    pub name: String,
    pub args: Vec<(String, Option<TypeAnnotationNode>)>,
    pub out_ann: Option<TypeAnnotationNode>
}

#[derive(Debug, PartialEq)]
pub struct InteropFuncNode {
    pub val: InteropFunc,
    pub info: NodeInfo
}

#[derive(Debug, PartialEq)]
pub struct Func {
    pub name: String,
    pub args: Vec<(String, Option<TypeAnnotationNode>)>,
    pub body: BodyNode,
    pub out_ann: Option<TypeAnnotationNode>
}

#[derive(Debug, PartialEq)]
pub struct FuncNode {
    pub val: Func,
    pub info: NodeInfo
}

#[derive(Debug, PartialEq)]
pub struct Module {
    pub name: String,
    pub imports: Vec<String>,
    pub interop_functions: Vec<InteropFuncNode>,
    pub functions: Vec<FuncNode>,
    pub definitions: Vec<StmtNode>,
    pub types: Vec<TypeNode>
}

impl Module {
    pub fn extend(&mut self, other: Module) {
        self.imports.extend(other.imports);
        self.interop_functions.extend(other.interop_functions);
        self.functions.extend(other.functions);
        self.definitions.extend(other.definitions);
        self.types.extend(other.types);
    }
}

fn to_expr(expr: Pair<Rule>, file_name: &String) -> ExprNode {
    PREC_CLIMBER.climb(
        expr.into_inner(),
        |pair: Pair<Rule>| match pair.as_rule() {
            Rule::up_id => ExprNode {
                val: Expr::Id(String::from(pair.as_str())),
                info: NodeInfo {span: Span::from(pair.as_span()), file: file_name.clone()}
            },
            Rule::lo_id => ExprNode {
                val: Expr::Id(String::from(pair.as_str())),
                info: NodeInfo {span: Span::from(pair.as_span()), file: file_name.clone()}
            },
            Rule::num => ExprNode {
                val: Expr::Lit(pair.as_str().parse::<f64>().unwrap()),
                info: NodeInfo {span: Span::from(pair.as_span()), file: file_name.clone()}
            },
            Rule::string => ExprNode {
                val: Expr::StringLit(String::from(pair.as_str())),
                info: NodeInfo {span: Span::from(pair.as_span()), file:file_name.clone()}
            },
            Rule::expr => to_expr(pair, file_name),
            Rule::fn_call => {
                let pair_span = pair.as_span();

                let mut children = pair.into_inner();
                let id = String::from(children.next().unwrap().as_str());
                let args = children.into_iter().map(|arg| { Box::from(to_expr(arg, file_name)) }).collect();

                ExprNode {
                    val: Expr::FnCall(id, args),
                    info: NodeInfo {span: Span::from(pair_span), file: file_name.clone()}
                }
            }
            Rule::val_call => {
                let pair_span = pair.as_span();

                let mut children = pair.into_inner();
                let id = String::from(children.next().unwrap().as_str());
                let args = children.into_iter().map(|arg| { Box::from(to_expr(arg, file_name)) }).collect();

                ExprNode {
                    val: Expr::FnCall(id, args),
                    info: NodeInfo {span: Span::from(pair_span), file: file_name.clone()}
                }
            }
            Rule::curry => {
                let pair_span = pair.as_span();

                let mut children = pair.into_inner();
                let id = String::from(children.next().unwrap().as_str());
                let args = children.into_iter().map(|arg| {
                    match arg.as_rule() {
                        Rule::expr => {
                            Some(Box::from(
                                to_expr(arg, file_name)
                            ))
                        }
                        Rule::blank => {
                            None
                        }
                        _ => unreachable!()
                    }
                }).collect();

                ExprNode {
                    val: Expr::Curry(id, args),
                    info: NodeInfo {span: Span::from(pair_span), file: file_name.clone()}
                }
            }
            Rule::struct_val => {
                let pair_span = pair.as_span();

                let struct_fields = pair.into_inner().into_iter().map(|struct_field| {
                    match struct_field.as_rule() {
                        Rule::struct_val_field => {
                            let mut field_children = struct_field.into_inner();
                            let id = String::from(field_children.next().unwrap().as_str());
                            let expr = field_children.next().unwrap();

                            (
                                id,
                                Box::from(
                                    to_expr(expr, file_name)
                                )
                            )
                        }
                        _ => unreachable!()
                    }
                }).collect();

                ExprNode {
                    val: Expr::StructVal(struct_fields),
                    info: NodeInfo {span: Span::from(pair_span), file: file_name.clone()}
                }
            }
            _ => unreachable!(),
        },
        |lhs: ExprNode, op: Pair<Rule>, rhs: ExprNode| {
            let expr = match op.as_rule() {
                Rule::add      => Expr::Add(Box::from(lhs), Box::from(rhs)),
                Rule::subtract => Expr::Subt(Box::from(lhs), Box::from(rhs)),
                Rule::multiply => Expr::Mult(Box::from(lhs), Box::from(rhs)),
                Rule::divide   => Expr::Div(Box::from(lhs), Box::from(rhs)),
                Rule::power    => Expr::Pow(Box::from(lhs), Box::from(rhs)),
                Rule::modulus  => Expr::Mod(Box::from(lhs), Box::from(rhs)),
                Rule::eq       => Expr::Eq(Box::from(lhs), Box::from(rhs)),
                Rule::not_eq   => Expr::NotEq(Box::from(lhs), Box::from(rhs)),
                Rule::lt_eq    => Expr::LtEq(Box::from(lhs), Box::from(rhs)),
                Rule::gt_eq    => Expr::GtEq(Box::from(lhs), Box::from(rhs)),
                Rule::lt       => Expr::Lt(Box::from(lhs), Box::from(rhs)),
                Rule::gt       => Expr::Gt(Box::from(lhs), Box::from(rhs)),
                Rule::concat   => Expr::Concat(Box::from(lhs), Box::from(rhs)),
                Rule::access   => {
                    let accessField = match rhs.val {
                        Expr::Id(s) => s,
                        // TODO: configure parsing to throw errors, implement error for this
                        _ => unreachable!()
                    };
                    
                    Expr::Access(Box::from(lhs), accessField)
                }
                _ => unreachable!(),
            };

            ExprNode {
                val: expr,
                info: NodeInfo{span: Span::from(op.as_span()), file: file_name.clone()}
            }
        },
    )
}

fn to_body(body: Pair<Rule>, file_name: &String) -> BodyNode {
    let body_span = body.as_span();

    let mut stmts = Vec::new();
    let mut expr = Option::None;
    for element in body.into_inner() {
        match element.as_rule() {
            Rule::while_stmt | Rule::assign | Rule::fn_call | Rule::case => {
                stmts.push(to_stmt(element, file_name));
            }
            Rule::expr => {
                expr = Option::Some(to_expr(element, file_name));
            }
            _ => {
                println!("rule {:?} encountered", element.as_rule());
                unreachable!();
            }
        }
    };

    let body_struct = Body {
        stmts: stmts,
        expr: expr
    };

    BodyNode {
        val: body_struct,
        info: NodeInfo {span: Span::from(body_span), file: file_name.clone()}
    }
}

fn to_case_option(option: Pair<Rule>, file_name: &String) -> CaseOptionNode {
    let option_span = option.as_span();

    let mut children = option.into_inner();
    let mut pattern_children = children.next().unwrap().into_inner();
    let pattern_token = pattern_children.next().unwrap();
    let pattern_base = String::from(pattern_token.as_str());
    let pattern_args = pattern_children.into_iter().map(|arg| { String::from(arg.as_str()) }).collect();
    let pattern = CasePatternNode {
        val: CasePattern { base: pattern_base, args: pattern_args },
        // TODO: make span both base and args
        info: NodeInfo {span: Span::from(pattern_token.as_span()), file: file_name.clone()}
    };

    let body_token = children.next().unwrap();
    let body_span = body_token.as_span();
    let body = CaseBodyNode {
        val: match body_token.as_rule() {
            Rule::expr => CaseBody::Expr(to_expr(body_token, file_name)),
            Rule::body => CaseBody::Body(to_body(body_token, file_name)),
            _ => unreachable!()
        },
        info: NodeInfo {span: Span::from(body_span), file: file_name.clone()}
    };

    let case_option = CaseOption {
        pattern: pattern,
        body: body
    };

    CaseOptionNode {
        val: case_option,
        info: NodeInfo {span: Span::from(option_span), file: file_name.clone()}
    }
}

fn to_case(case: Pair<Rule>, file_name: &String) -> CaseNode {
    let case_span = case.as_span();

    let mut children = case.into_inner();
    let expr = to_expr(children.next().unwrap(), file_name);

    let mut options = Vec::new();
    for option in children {
        options.push(to_case_option(option, file_name));
    }

    let case_struct = Case {
        expr: expr,
        options: options
    };

    CaseNode {
        val: case_struct,
        info: NodeInfo {span: Span::from(case_span), file: file_name.clone()}
    }
}

fn to_stmt(stmt: Pair<Rule>, file_name: &String) -> StmtNode {
    let stmt_span = stmt.as_span();

    let stmt_val = match stmt.as_rule() {
        Rule::assign => {
            let mut children = stmt.into_inner();
            let tgt = children.next().unwrap();
            let tgt_span = tgt.as_span();
            let target_val = match tgt.as_rule() {
                Rule::lo_id => Target::Var(String::from(tgt.as_str())),
                Rule::mutable_tgt => Target::Mutable(String::from(tgt.into_inner().next().unwrap().as_str())),
                Rule::update_tgt => Target::Update(String::from(tgt.into_inner().next().unwrap().as_str())),
                _ => unreachable!()
            };
            let target = TargetNode {
                val: target_val,
                info: NodeInfo {span: Span::from(tgt_span), file: file_name.clone()}
            };
            let expr = to_expr(children.next().unwrap(), file_name);
            Stmt::Assign(target, expr)
        }
        Rule::fn_call => {
            let mut children = stmt.into_inner();
            let id = String::from(children.next().unwrap().as_str());

            let mut args = Vec::new();
            for arg in children {
                args.push(to_expr(arg, file_name));
            }

            Stmt::FnCall(id, args)
        }
        Rule::case => {
            Stmt::Case(to_case(stmt, file_name))
        }
        Rule::while_stmt => {
            let mut children = stmt.into_inner();
            let expr = children.next().unwrap();
            let body = children.next().unwrap();

            let while_node = WhileNode {
                val: While {
                    expr: to_expr(expr, file_name),
                    body: to_body(body, file_name)
                },
                info: NodeInfo {span: Span::from(stmt_span.clone()), file: file_name.clone()}
            };

            Stmt::While(while_node)
        }
        _ => {
            println!("rule {:?} encountered", stmt.as_rule());
            unreachable!();
        }
    };

    StmtNode {
        val: stmt_val,
        info: NodeInfo {span: Span::from(stmt_span), file: file_name.clone()}
    }
}

fn to_type_annotation(mut p: Pair<Rule>, file_name: &String) -> TypeAnnotationNode {
    let ann_span = p.as_span();
    let type_ann = match p.as_rule() {
        Rule::unit => TypeAnnotation::Unit,
        Rule::tvar => TypeAnnotation::TVar(
            String::from(p.as_str())
        ),
        Rule::adt => {
            let mut children = p.into_inner();
            let id = String::from(
                children.next().unwrap().as_str()
            );

            let mut args = Vec::new();
            for arg in children {
                args.push(
                    Box::from(to_type_annotation(arg, file_name))
                );
            }
            TypeAnnotation::ADT(id, args)
        }
        Rule::func => {
            let mut children = p.into_inner();
            let mut sub_exprs = Vec::new();
            for c in children {
                sub_exprs.push(
                    Box::from(to_type_annotation(c, file_name))
                );
            }

            let out = sub_exprs.pop().expect("unreachable");
            TypeAnnotation::Func(sub_exprs, out)
        }
        _ => unreachable!()
    };
    TypeAnnotationNode {
        val: type_ann,
        info: NodeInfo {
            span: Span::from(ann_span),
            file: file_name.clone()
        }
    }
}

fn to_args(mut args: Pair<Rule>, file_name: &String) -> (Vec<(String, Option<TypeAnnotationNode>)>, Option<TypeAnnotationNode>) {
    let mut arg_vec = Vec::new();
    let mut out_ann = None;
    for arg in args.into_inner() {
        match arg.as_rule() {
            Rule::fn_arg => {
                let mut arg_parts = arg.into_inner();
                let id = String::from(
                    arg_parts.next().unwrap().as_str()
                );
                let ann = match arg_parts.next() {
                    Some(annotation) => {
                        Some(to_type_annotation(annotation, file_name))
                    }
                    None => {
                        None
                    }
                };
                arg_vec.push((id, ann));
            }
            Rule::func | Rule::unit | Rule::adt | Rule::tvar => {
                out_ann = Some(to_type_annotation(arg, file_name))
            }
            _ => unreachable!()
        }
    }

    (arg_vec, out_ann)
}

fn to_interop(mut p: Pair<Rule>, file_name: &String) -> InteropFuncNode {
    let interop_span = p.as_span();
    let mut interop = p.into_inner();

    let id = String::from(interop.next().unwrap().as_str());

    let args = interop.next().unwrap();
    let (arg_vec, out_ann) = to_args(args, file_name);

    let val = InteropFunc {
        name: id,
        args: arg_vec,
        out_ann: out_ann
    };

    InteropFuncNode {
        val: val,
        info: NodeInfo {span: Span::from(interop_span), file: file_name.clone()}
    }
}

fn to_func(mut p: Pair<Rule>, file_name: &String) -> FuncNode {
    let func_span = p.as_span();
    let mut func = p.into_inner();

    let id = String::from(func.next().unwrap().as_str());

    let args = func.next().unwrap();
    let (arg_vec, out_ann) = to_args(args, file_name);

    let body = to_body(func.next().unwrap(), file_name);

    let func = Func {
        name: id,
        args: arg_vec,
        body: body,
        out_ann: out_ann
    };

    FuncNode {
        val: func,
        info: NodeInfo {span: Span::from(func_span), file: file_name.clone()}
    }
}

fn to_type_option(option: Pair<Rule>, file_name: &String) -> TypeOptionNode {
    let option_span = option.as_span();

    let mut children = option.into_inner();

    let name = String::from(children.next().unwrap().as_str());

    let mut args = Vec::new();
    for arg in children {
        args.push(to_type_identifier(arg));
    }
    let type_option_val = TypeOption {
        name: name,
        args: args
    };

    TypeOptionNode {
        val: type_option_val,
        info: NodeInfo {span: Span::from(option_span), file: file_name.clone() }
    }
}

fn to_type_identifier(ident: Pair<Rule>) -> TypeIdentifier {
    let _ident_span = ident.as_span();

    let mut children = ident.into_inner();

    let name = String::from(children.next().unwrap().as_str());

    let mut args = Vec::new();
    for arg in children {
        args.push(Box::from(to_type_identifier(arg)));
    }
    TypeIdentifier {
        name: name,
        args: args
    }
}

fn to_type(t: Pair<Rule>, file_name: &String) -> TypeNode {
    let type_span = t.as_span();
    let mut children = t.into_inner();

    let name = String::from(children.next().unwrap().as_str());

    let mut params = Vec::new();
    let param_tokens = children.next().unwrap().into_inner();
    for param_token in param_tokens {
        params.push(String::from(param_token.as_str()));
    }

    let mut options = Vec::new();
    for option in children {
        options.push(to_type_option(option, file_name));
    }

    let type_val = Type {
        name: name,
        type_params: params,
        options: options
    };

    TypeNode {
        val: type_val,
        info: NodeInfo {span: Span::from(type_span), file: file_name.clone() }
    }
}

fn to_ast(file: Pairs<Rule>, file_name: String) -> Module {
    let mut imports = Vec::new();
    let mut stmts = Vec::new();
    let mut functions = Vec::new();
    let mut interop_functions = Vec::new();
    let mut types = Vec::new();

    for element in file {
        match element.as_rule() {
            Rule::import => {
                imports.push(String::from(
                    element.into_inner().next().unwrap().as_str()
                ));
            }
            Rule::interop_decl => {
                interop_functions.push( to_interop(element, &file_name) );
            }
            Rule::function_decl => {
                functions.push( to_func(element, &file_name) );
            }
            Rule::assign => {
                stmts.push( to_stmt(element, &file_name) );
            }
            Rule::type_decl => {
                types.push( to_type(element, &file_name) );
            }
            Rule::EOI => (),
            _ => unreachable!()
        }
    }


    // all modules depend on the Prelude except Prelude itself
    if file_name != "Prelude.sp" {
        imports.push(String::from("Prelude"));
    }

    // strip off '.sp'
    let mod_name = &file_name[..(file_name.len() - 3)];

    Module {
        name: String::from(mod_name),
        imports: imports,
        interop_functions: interop_functions,
        functions: functions,
        definitions: stmts,
        types: types
    }

}

pub fn parse(unparsed: &str, file_name: String) -> Result<Module, SpruceErr> {
    let parsed = ExprParser::parse(Rule::file, &unparsed);
    match parsed {
        Ok(pairs) => {
            Ok(to_ast(pairs, file_name))
        }
        Err(e) => {
            let err = match e.location {
                InputLocation::Pos(pos) => {
                    SpruceErr {
                        message: String::from("Parse error"),
                        info: NodeInfo {
                            span: Span {start: pos, end: pos},
                            file: file_name.clone()
                        }
                    }
                }
                InputLocation::Span((start, end)) => {
                    SpruceErr {
                        message: String::from("Parse error"),
                        info: NodeInfo {
                            span: Span {start: start, end: end},
                            file: file_name.clone()
                        }
                    }
                }
            };
            Err(err)
        }
    }
}
