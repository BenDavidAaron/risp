use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

type Symbol = String;

#[derive(Clone, PartialEq)]
enum Number {
    Integer(i64),
    Float(f64),
}

#[derive(Clone, PartialEq)]
enum Atom {
    Symbol(Symbol),
    Number(Number),
}

impl From<String> for Atom {
    fn from(s: String) -> Self {
        if let Ok(i) = s.parse::<i64>() {
            Atom::Number(Number::Integer(i))
        } else if let Ok(fl) = s.parse::<f64>() {
            Atom::Number(Number::Float(fl))
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

type Env = HashMap<Symbol, Rc<Expr>>;

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Atom(Atom::Symbol(s)) => write!(f, "{}", s),
            Expr::Atom(Atom::Number(Number::Integer(i))) => write!(f, "{}", i),
            Expr::Atom(Atom::Number(Number::Float(fl))) => write!(f, "{}", fl),
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

fn parse(program: &str) -> Expr {
    read_from_tokens(&mut tokenize(program).into_iter().rev().collect())
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
fn main() {
    let program = "(+ 1 2)";
    println!("{}", program);
    println!("{:?}", tokenize(program));
    println!("{}", parse(program));
}
