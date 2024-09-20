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

fn main() {
    println!("Hello, world!");
}
