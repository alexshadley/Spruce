
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
            Operator::new(eq, Left) | Operator::new(not_eq, Left),
            Operator::new(lt_eq, Left) | Operator::new(gt_eq, Left) | Operator::new(lt, Left) | Operator::new(gt, Left),
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
    pub base: String,
    pub args: Vec<String> 
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
    pub span: Span
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
    pub span: Span
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
    pub span: Span
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
    pub span: Span
}

#[derive(Debug, PartialEq)]
pub struct TypeOption {
    pub name: String,
    pub args: Vec<String>
}

#[derive(Debug, PartialEq)]
pub struct TypeOptionNode {
    pub val: TypeOption,
    pub span: Span
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
    pub span: Span
}

#[derive(Debug, PartialEq)]
pub struct Prog {
    pub functions: Vec<FuncNode>,
    pub definitions: Vec<StmtNode>,
    pub types: Vec<TypeNode>
}

fn to_expr(expr: Pair<Rule>) -> ExprNode {
    PREC_CLIMBER.climb(
        expr.into_inner(),
        |pair: Pair<Rule>| match pair.as_rule() {
            Rule::id => ExprNode {val: Expr::Id(String::from(pair.as_str())), span: Span::from(pair.as_span())},
            Rule::num => ExprNode {val: Expr::Lit(pair.as_str().parse::<f64>().unwrap()), span: Span::from(pair.as_span())},
            Rule::expr => to_expr(pair),
            Rule::fn_call => {
                let pair_span = pair.as_span();

                let mut children = pair.into_inner();
                let id = String::from(children.next().unwrap().as_str());
                let args = children.into_iter().map(|arg| { Box::from(to_expr(arg)) }).collect();

                ExprNode {val: Expr::FnCall(id, args), span: Span::from(pair_span)}
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
                Rule::eq       => Expr::Eq(Box::from(lhs), Box::from(rhs)),
                Rule::not_eq   => Expr::NotEq(Box::from(lhs), Box::from(rhs)),
                Rule::lt_eq    => Expr::LtEq(Box::from(lhs), Box::from(rhs)),
                Rule::gt_eq    => Expr::GtEq(Box::from(lhs), Box::from(rhs)),
                Rule::lt       => Expr::Lt(Box::from(lhs), Box::from(rhs)),
                Rule::gt       => Expr::Gt(Box::from(lhs), Box::from(rhs)),
                _ => unreachable!(),
            };

            ExprNode {val: expr, span: Span::from(op.as_span())}
        },
    )
}

fn to_body(body: Pair<Rule>) -> BodyNode {
    let body_span = body.as_span();

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

    let body_struct = Body {
        stmts: stmts,
        expr: expr
    };

    BodyNode {
        val: body_struct,
        span: Span::from(body_span)
    }
}

fn to_case_option(option: Pair<Rule>) -> CaseOptionNode {
    let option_span = option.as_span();

    let mut children = option.into_inner();
    let mut pattern_children = children.next().unwrap().into_inner();
    let pattern_token = pattern_children.next().unwrap();
    let pattern_base = String::from(pattern_token.as_str());
    let pattern_args = pattern_children.into_iter().map(|arg| { String::from(arg.as_str()) }).collect();
    let pattern = CasePatternNode {
        val: CasePattern { base: pattern_base, args: pattern_args },
        // TODO: make span both base and args
        span: Span::from(pattern_token.as_span())
    };

    let body_token = children.next().unwrap();
    let body_span = body_token.as_span();
    let body = CaseBodyNode {
        val: match body_token.as_rule() {
            Rule::expr => CaseBody::Expr(to_expr(body_token)),
            Rule::body => CaseBody::Body(to_body(body_token)),
            _ => unreachable!()
        },
        span: Span::from(body_span)
    };

    let case_option = CaseOption {
        pattern: pattern,
        body: body
    };

    CaseOptionNode {
        val: case_option,
        span: Span::from(option_span)
    }
}

fn to_case(case: Pair<Rule>) -> CaseNode {
    let case_span = case.as_span();

    let mut children = case.into_inner();
    let expr = to_expr(children.next().unwrap());

    let mut options = Vec::new();
    for option in children {
        options.push(to_case_option(option));
    }

    let case_struct = Case {
        expr: expr,
        options: options
    };

    CaseNode {
        val: case_struct,
        span: Span::from(case_span)
    }
}

fn to_stmt(stmt: Pair<Rule>) -> StmtNode {
    let stmt_span = stmt.as_span();

    let stmt_val = match stmt.as_rule() {
        Rule::assign => {
            let mut children = stmt.into_inner();
            let tgt = children.next().unwrap();
            let tgt_span = tgt.as_span();
            let target_val = match tgt.as_rule() {
                Rule::id => Target::Var(String::from(tgt.as_str())),
                Rule::mutable_tgt => Target::Mutable(String::from(tgt.into_inner().next().unwrap().as_str())),
                Rule::update_tgt => Target::Update(String::from(tgt.into_inner().next().unwrap().as_str())),
                _ => unreachable!()
            };
            let target = TargetNode {
                val: target_val,
                span: Span::from(tgt_span)
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
    };

    StmtNode {
        val: stmt_val,
        span: Span::from(stmt_span)
    }
}

fn to_func(mut p: Pair<Rule>) -> FuncNode {
    let func_span = p.as_span();
    let mut func = p.into_inner();

    let id = String::from(func.next().unwrap().as_str());

    let args = func.next().unwrap();
    let mut arg_vec = Vec::new();
    for arg in args.into_inner() {
        arg_vec.push(String::from(arg.as_str()));
    }

    let body = to_body(func.next().unwrap());

    let func = Func {
        name: id,
        args: arg_vec,
        body: body
    };

    FuncNode {
        val: func,
        span: Span::from(func_span)
    }
}

fn to_type_option(option: Pair<Rule>) -> TypeOptionNode {
    let option_span = option.as_span();

    let mut children = option.into_inner();

    let name = String::from(children.next().unwrap().as_str());

    let mut args = Vec::new();
    for arg in children {
        args.push(String::from(arg.as_str()));
    }
    let type_option_val = TypeOption {
        name: name,
        args: args
    };

    TypeOptionNode {
        val: type_option_val,
        span: Span::from(option_span)
    }
}

fn to_type(mut t: Pair<Rule>) -> TypeNode {
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
        options.push(to_type_option(option));
    }

    let type_val = Type {
        name: name,
        type_params: params,
        options: options
    };

    TypeNode {
        val: type_val,
        span: Span::from(type_span)
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
