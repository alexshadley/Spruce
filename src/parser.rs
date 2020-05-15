
extern crate pest;

use pest::{Parser};
use pest::iterators::{Pairs, Pair};
use pest::prec_climber::{PrecClimber, Operator, Assoc};
use pest::error::LineColLocation;


#[derive(Parser)]
#[grammar = "expr.pest"]
pub struct ExprParser;

lazy_static! {
    static ref PREC_CLIMBER: PrecClimber<Rule> = {
        use Rule::*;
        use Assoc::*;

        PrecClimber::new(vec![
            Operator::new(add, Left) | Operator::new(subtract, Left),
            Operator::new(multiply, Left) | Operator::new(divide, Left),
            Operator::new(power, Right),
        ])
    };
}

/*#[derive(Debug, PartialEq)]
/pub enum Literal {
    Int(i64),
    Float(f64),
}*/

#[derive(Debug, PartialEq)]
pub enum Expr {
    Add(Box<Expr>, Box<Expr>),
    Mult(Box<Expr>, Box<Expr>),
    Subt(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Pow(Box<Expr>, Box<Expr>),
    FnCall(String, Vec<Box<Expr>>),
    Id(String),
    ADTVal(String, Vec<Box<Expr>>),
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
    FnCall(String, Vec<Expr>),
    Case(Case)
}

#[derive(Debug, PartialEq)]
pub enum Target {
    Var(String),
    Mutable(String),
    Update(String)
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
    pub name: String,
    pub args: Vec<String>,
    pub body: Body
}

#[derive(Debug, PartialEq)]
pub struct Prog {
    pub functions: Vec<Func>,
    pub definitions: Vec<Stmt>,
    pub types: Vec<Type>
}

fn to_expr(expr: Pair<Rule>) -> Expr {
    PREC_CLIMBER.climb(
        expr.into_inner(),
        |pair: Pair<Rule>| match pair.as_rule() {
            Rule::id => Expr::Id(String::from(pair.as_str())),
            Rule::num => Expr::Lit(pair.as_str().parse::<f64>().unwrap()),
            Rule::expr => to_expr(pair),
            Rule::fn_call => {
                let mut children = pair.into_inner();
                let id = String::from(children.next().unwrap().as_str());
                let args = children.into_iter().map(|arg| { Box::from(to_expr(arg)) }).collect();

                Expr::FnCall(id, args)
            }
            _ => unreachable!(),
        },
        |lhs: Expr, op: Pair<Rule>, rhs: Expr| match op.as_rule() {
            Rule::add      => Expr::Add(Box::from(lhs), Box::from(rhs)),
            Rule::subtract => Expr::Subt(Box::from(lhs), Box::from(rhs)),
            Rule::multiply => Expr::Mult(Box::from(lhs), Box::from(rhs)),
            Rule::divide   => Expr::Div(Box::from(lhs), Box::from(rhs)),
            Rule::power    => Expr::Pow(Box::from(lhs), Box::from(rhs)),
            _ => unreachable!(),
        },
    )
}

fn to_body(body: Pair<Rule>) -> Body {
    let mut stmts = Vec::new();
    let mut expr = Option::None;
    for element in body.into_inner() {
        match element.as_rule() {
            Rule::assign | Rule::fn_call | Rule::case => {
                stmts.push(to_stmt(element));
            }
            Rule::expr => {
                expr = Option::Some(to_expr(element));
            }
            _ => {
                println!("rule {:?} encountered", element.as_rule());
                unreachable!();
            }
        }
    };

    Body {
        stmts: stmts,
        expr: expr
    }
}

fn to_case_option(option: Pair<Rule>) -> CaseOption {
    let mut children = option.into_inner();
    let mut pattern_children = children.next().unwrap().into_inner();
    let pattern_base = String::from(pattern_children.next().unwrap().as_str());
    let pattern_args = pattern_children.into_iter().map(|arg| { String::from(arg.as_str()) }).collect();
    let pattern = CasePattern { base: pattern_base, args: pattern_args };

    let body_token = children.next().unwrap();
    let body = match body_token.as_rule() {
        Rule::expr => CaseBody::Expr(to_expr(body_token)),
        Rule::body => CaseBody::Body(to_body(body_token)),
        _ => unreachable!()
    };

    CaseOption {
        pattern: pattern,
        body: body
    }
}

fn to_case(case: Pair<Rule>) -> Case {
    let mut children = case.into_inner();
    let expr = to_expr(children.next().unwrap());

    let mut options = Vec::new();
    for option in children {
        options.push(to_case_option(option));
    }

    Case {
        expr: expr,
        options: options
    }
}

fn to_stmt(stmt: Pair<Rule>) -> Stmt {
    match stmt.as_rule() {
        Rule::assign => {
            let mut children = stmt.into_inner();
            let tgt = children.next().unwrap();
            let target = match tgt.as_rule() {
                Rule::id => Target::Var(String::from(tgt.as_str())),
                Rule::mutable_tgt => Target::Mutable(String::from(tgt.as_str())),
                Rule::update_tgt => Target::Update(String::from(tgt.as_str())),
                _ => unreachable!()
            };
            let expr = to_expr(children.next().unwrap());
            Stmt::Assign(target, expr)
        }
        Rule::fn_call => {
            let mut children = stmt.into_inner();
            let id = String::from(children.next().unwrap().as_str());

            let mut args = Vec::new();
            for arg in children {
                args.push(to_expr(arg));
            }

            Stmt::FnCall(id, args)
        }
        Rule::case => {
            Stmt::Case(to_case(stmt))
        }
        _ => {
            println!("rule {:?} encountered", stmt.as_rule());
            unreachable!();
        }
    }
}

fn to_func(mut p: Pair<Rule>) -> Func {
    let mut func = p.into_inner();

    let id = String::from(func.next().unwrap().as_str());

    let args = func.next().unwrap();
    let mut arg_vec = Vec::new();
    for arg in args.into_inner() {
        arg_vec.push(String::from(arg.as_str()));
    }

    let body = to_body(func.next().unwrap());

    Func {
        name: id,
        args: arg_vec,
        body: body
    }
}

fn to_type_option(option: Pair<Rule>) -> TypeOption {
    let mut children = option.into_inner();

    let name = String::from(children.next().unwrap().as_str());

    let mut args = Vec::new();
    for arg in children {
        args.push(String::from(arg.as_str()));
    }
    TypeOption {
        name: name,
        args: args
    }
}

fn to_type(mut t: Pair<Rule>) -> Type {
    let mut children = t.into_inner();

    let name = String::from(children.next().unwrap().as_str());

    let mut options = Vec::new();
    for option in children {
        options.push(to_type_option(option));
    }

    Type {
        name: name,
        options: options
    }
}

fn to_ast(prog: Pairs<Rule>) -> Prog {
    let mut stmts = Vec::new();
    let mut functions = Vec::new();
    let mut types = Vec::new();

    for element in prog {
        match element.as_rule() {
            Rule::function_decl => {
                functions.push( to_func(element) );
            }
            Rule::assign => {
                stmts.push( to_stmt(element) );
            }
            Rule::type_decl => {
                types.push( to_type(element) );
            }
            Rule::EOI => (),
            _ => unreachable!()
        }
    }

    Prog {
        functions: functions,
        definitions: stmts,
        types: types
    }
}

pub fn parse(unparsed: &str) -> Result<Prog, String> {
    let parse_result = ExprParser::parse(Rule::file, &unparsed);
    match parse_result {
        Ok(pairs) => {
            Ok(to_ast(pairs))
        }

        Err(e) => {
            let err_msg = match e.line_col {
                LineColLocation::Pos((line, col)) => format!("Parse error at: {} {}", line, col),
                LineColLocation::Span((line, col), _) => format!("Parse error at: {} {}", line, col)
            };
            Err(err_msg)
        }
    }   
}
