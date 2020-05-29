
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
    Id(String),
    Lit(f64),
    Eq(Box<ExprNode>, Box<ExprNode>),
    NotEq(Box<ExprNode>, Box<ExprNode>),
    LtEq(Box<ExprNode>, Box<ExprNode>),
    GtEq(Box<ExprNode>, Box<ExprNode>),
    Lt(Box<ExprNode>, Box<ExprNode>),
    Gt(Box<ExprNode>, Box<ExprNode>),
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
    Case(CaseNode)
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
pub struct Func {
    pub name: String,
    pub args: Vec<String>,
    pub body: BodyNode
}

#[derive(Debug, PartialEq)]
pub struct FuncNode {
    pub val: Func,
    pub info: NodeInfo
}

#[derive(Debug, PartialEq)]
pub struct Prog {
    pub functions: Vec<FuncNode>,
    pub definitions: Vec<StmtNode>,
    pub types: Vec<TypeNode>
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
            Rule::assign | Rule::fn_call | Rule::case => {
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

fn to_func(mut p: Pair<Rule>, file_name: &String) -> FuncNode {
    let func_span = p.as_span();
    let mut func = p.into_inner();

    let id = String::from(func.next().unwrap().as_str());

    let args = func.next().unwrap();
    let mut arg_vec = Vec::new();
    for arg in args.into_inner() {
        arg_vec.push(String::from(arg.as_str()));
    }

    let body = to_body(func.next().unwrap(), file_name);

    let func = Func {
        name: id,
        args: arg_vec,
        body: body
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
    let ident_span = ident.as_span();

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

fn to_type(mut t: Pair<Rule>, file_name: &String) -> TypeNode {
    let type_span = t.as_span();
    let mut children = t.into_inner();

    let name = String::from(children.next().unwrap().as_str());

    let mut params = Vec::new();
    let mut param_tokens = children.next().unwrap().into_inner();
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

fn to_ast(files: Vec<(Pairs<Rule>, String)>) -> Prog {
    let mut stmts = Vec::new();
    let mut functions = Vec::new();
    let mut types = Vec::new();

    for (file, name) in files {
        for element in file {
            match element.as_rule() {
                Rule::function_decl => {
                    functions.push( to_func(element, &name) );
                }
                Rule::assign => {
                    stmts.push( to_stmt(element, &name) );
                }
                Rule::type_decl => {
                    types.push( to_type(element, &name) );
                }
                Rule::EOI => (),
                _ => unreachable!()
            }
        }
    }

    Prog {
        functions: functions,
        definitions: stmts,
        types: types
    }
}

pub fn parse(unparsed: Vec<(&str, String)>) -> Result<Prog, SpruceErr> {
    let mut parse_results = Vec::new();
    for (file, name) in unparsed {
        let parsed = ExprParser::parse(Rule::file, &file);
        match parsed {
            Ok(pairs) => {
                parse_results.push((pairs, name));
            }
            Err(e) => {
                let err = match e.location {
                    InputLocation::Pos(pos) => {
                        SpruceErr {
                            message: String::from("Parse error"),
                            info: NodeInfo {
                                span: Span {start: pos, end: pos},
                                file: name.clone()
                            }
                        }
                    }
                    InputLocation::Span((start, end)) => {
                        SpruceErr {
                            message: String::from("Parse error"),
                            info: NodeInfo {
                                span: Span {start: start, end: end},
                                file: name.clone()
                            }
                        }
                    }
                };
                return Err(err);
            }
        }
    }

    Ok(to_ast(parse_results))
}
