use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

type Symbol = String;

type Number = f64;

#[derive(Clone, PartialEq)]
enum Atom {
    Symbol(Symbol),
    Number(Number),
}

enum AstNode {
    Symbol(Symbol),
    Number(Number),
    List(Vec<AstNode>),
    Add(Box<AstNode>, Box<AstNode>),
    Print(Box<AstNode>),
}

impl fmt::Display for AstNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AstNode::Symbol(s) => write!(f, "{}", s),
            AstNode::Number(fl) => write!(f, "{}", fl),
            AstNode::List(list) => {
                write!(f, "(")?;
                for (i, node) in list.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", node)?;
                }
                write!(f, ")")
            }
            AstNode::Add(a, b) => write!(f, "(+ {} {})", a, b),
            AstNode::Print(a) => write!(f, "(print {})", a),
        }
    }
}

impl From<String> for Atom {
    fn from(s: String) -> Self {
        if let Ok(fl) = s.parse::<f64>() {
            Atom::Number(fl)
        } else {
            Atom::Symbol(s)
        }
    }
}

#[derive(Clone, PartialEq)]
enum Expr {
    Atom(Atom),
    List(Vec<Expr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Atom(Atom::Symbol(s)) => write!(f, "{}", s),
            Expr::Atom(Atom::Number(fl)) => write!(f, "{}", fl),
            Expr::List(list) => {
                write!(f, "(")?;
                for (i, exp) in list.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", exp)?;
                }
                write!(f, ")")
            }
        }
    }
}

fn tokenize(input: &str) -> Vec<String> {
    input
        .replace("(", " ( ")
        .replace(")", " ) ")
        .split_whitespace()
        .map(|s| s.to_string())
        .collect()
}

fn read_from_tokens(tokens: &mut Vec<String>) -> Expr {
    if let Some(token) = tokens.pop() {
        match token.as_str() {
            "(" => {
                let mut list = Vec::new();
                while tokens.last().map_or(false, |t| t != ")") {
                    list.push(read_from_tokens(tokens));
                }
                tokens.pop();
                Expr::List(list)
            }
            ")" => panic!("unexpected ')'"),
            _ => Expr::Atom(Atom::from(token)),
        }
    } else {
        panic!("unexpected EOF while reading");
    }
}

fn expr_to_ast(expr: Expr) -> AstNode {
    match expr {
        Expr::Atom(Atom::Symbol(s)) => AstNode::Symbol(s),
        Expr::Atom(Atom::Number(fl)) => AstNode::Number(fl),
        Expr::List(list) => {
            if list.is_empty() {
                return AstNode::List(vec![]);
            }

            if let Expr::Atom(Atom::Symbol(op)) = &list[0] {
                match op.as_str() {
                    "+" => {
                        if list.len() != 3 {
                            panic!("+ operator expects exactly 2 arguments");
                        }
                        AstNode::Add(
                            Box::new(expr_to_ast(list[1].clone())),
                            Box::new(expr_to_ast(list[2].clone())),
                        )
                    }
                    "print" => {
                        if list.len() != 2 {
                            panic!("print expects exactly 1 argument");
                        }
                        AstNode::Print(Box::new(expr_to_ast(list[1].clone())))
                    }
                    _ => AstNode::List(list.into_iter().map(expr_to_ast).collect()),
                }
            } else {
                AstNode::List(list.into_iter().map(expr_to_ast).collect())
            }
        }
    }
}

fn parse(program: &str) -> AstNode {
    let tokens = tokenize(program);
    let expr = read_from_tokens(&mut tokens.into_iter().rev().collect());
    expr_to_ast(expr)
}

fn pretty_print(node: &AstNode) -> String {
    fn pp_helper(node: &AstNode, indent: usize) -> String {
        let indent_str = " ".repeat(indent * 2);
        match node {
            AstNode::Symbol(s) => format!("{}{}", indent_str, s),
            AstNode::Number(n) => format!("{}{}", indent_str, n),
            AstNode::List(list) => {
                let mut result = format!("{}(\n", indent_str);
                for (i, node) in list.iter().enumerate() {
                    if i > 0 {
                        result.push('\n');
                    }
                    result.push_str(&pp_helper(node, indent + 1));
                }
                result.push(')');
                result
            }
            AstNode::Add(a, b) => {
                let mut result = format!("{}(\n{}  +\n", indent_str, indent_str);
                result.push_str(&pp_helper(a, indent + 1));
                result.push('\n');
                result.push_str(&pp_helper(b, indent + 2));
                result.push(')');
                result
            }
            AstNode::Print(a) => {
                let mut result = format!("{}(\n{}  print\n", indent_str, indent_str);
                result.push_str(&pp_helper(a, indent + 1));
                result.push(')');
                result
            }
        }
    }

    pp_helper(node, 0)
}

type Env = HashMap<Symbol, Rc<Expr>>;

fn main() {
    let program = "(print (+ 1 (+ 2 3)))";
    println!("{}", program);
    println!("{:?}", tokenize(program));
    println!("{}", pretty_print(&parse(program)));
}
