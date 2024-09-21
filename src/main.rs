use std::cell::RefCell;
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

#[derive(Debug)]
enum AstNode {
    Symbol(Symbol),
    Number(Number),
    List(Vec<AstNode>),
    Add(Box<AstNode>, Box<AstNode>),
    Print(Box<AstNode>),
    Let(Vec<(String, Box<AstNode>)>, Box<AstNode>), // (bindings, body)
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
            AstNode::Let(a, b) => write!(f, "(let {:?} {})", a, b),
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

struct Environment {
    values: HashMap<String, Value>,
    parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    fn new() -> Self {
        Environment {
            values: HashMap::new(),
            parent: None,
        }
    }

    fn with_parent(parent: Rc<RefCell<Environment>>) -> Self {
        Environment {
            values: HashMap::new(),
            parent: Some(parent),
        }
    }

    fn set(&mut self, key: String, value: Value) {
        self.values.insert(key, value);
    }

    fn get(&self, key: &str) -> Option<Value> {
        match self.values.get(key) {
            Some(value) => Some(value.clone()),
            None => match &self.parent {
                Some(parent) => parent.borrow().get(key),
                None => None,
            },
        }
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
            AstNode::Let(bindings, body) => {
                let mut result = format!("{}(\n{}  let (\n", indent_str, indent_str);
                for (var, value) in bindings {
                    result.push_str(&format!("{}    ({}\n", indent_str, var));
                    result.push_str(&pp_helper(value, indent + 3));
                    result.push_str(&format!("{}    )\n", indent_str));
                }
                result.push_str(&format!("{}  )\n", indent_str));
                result.push_str(&pp_helper(body, indent + 1));
                result.push(')');
                result
            }
        }
    }

    pp_helper(node, 0)
}

#[derive(Debug, Clone)]
enum Value {
    Number(f64),
    Symbol(String),
    List(Vec<Value>),
}

fn eval(node: &AstNode, environment: Rc<RefCell<Environment>>) -> Result<Value, String> {
    match node {
        AstNode::Number(n) => Ok(Value::Number(*n)),
        AstNode::Symbol(s) => environment
            .borrow()
            .get(s)
            .ok_or_else(|| format!("Undefined symbol: {}", s)),
        AstNode::List(list) => {
            if list.is_empty() {
                return Ok(Value::List(vec![]));
            }
            if let AstNode::Symbol(s) = &list[0] {
                match s.as_str() {
                    "let" => {
                        if list.len() != 3 {
                            return Err(
                                "let requires exactly 2 arguments: bindings and body".to_string()
                            );
                        }
                        if let AstNode::List(bindings) = &list[1] {
                            let new_env = Rc::new(RefCell::new(Environment::with_parent(
                                Rc::clone(&environment),
                            )));
                            for binding in bindings {
                                if let AstNode::List(pair) = binding {
                                    if pair.len() != 2 {
                                        return Err(
                                            "Each binding in let must be a pair".to_string()
                                        );
                                    }
                                    if let AstNode::Symbol(var) = &pair[0] {
                                        let value = eval(&pair[1], Rc::clone(&new_env))?;
                                        new_env.borrow_mut().set(var.clone(), value);
                                    } else {
                                        return Err(
                                            "First element of a binding pair must be a symbol"
                                                .to_string(),
                                        );
                                    }
                                } else {
                                    return Err("Each binding in let must be a list".to_string());
                                }
                            }
                            eval(&list[2], new_env)
                        } else {
                            Err("Second argument to let must be a list of bindings".to_string())
                        }
                    }
                    "add" => {
                        if list.len() != 3 {
                            return Err("add requires exactly 2 arguments".to_string());
                        }
                        let a = eval(&list[1], Rc::clone(&environment))?;
                        let b = eval(&list[2], Rc::clone(&environment))?;
                        match (a, b) {
                            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x + y)),
                            _ => Err("add requires two numbers".to_string()),
                        }
                    }
                    _ => {
                        let mut evaluated = Vec::new();
                        for node in list {
                            evaluated.push(eval(node, Rc::clone(&environment))?);
                        }
                        Ok(Value::List(evaluated))
                    }
                }
            } else {
                let mut evaluated = Vec::new();
                for node in list {
                    evaluated.push(eval(node, Rc::clone(&environment))?);
                }
                Ok(Value::List(evaluated))
            }
        }
        AstNode::Add(a, b) => {
            let a_val = eval(a, Rc::clone(&environment))?;
            let b_val = eval(b, Rc::clone(&environment))?;
            match (a_val, b_val) {
                (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x + y)),
                _ => Err("Addition requires two numbers".to_string()),
            }
        }
        AstNode::Print(a) => {
            let value = eval(a, Rc::clone(&environment))?;
            println!("{:?}", value);
            Ok(value)
        }
        AstNode::Let(bindings, body_expr) => {
            let new_env = Rc::new(RefCell::new(Environment::with_parent(Rc::clone(
                &environment,
            ))));
            for (var, value_expr) in bindings {
                let value = eval(value_expr, Rc::clone(&new_env))?;
                new_env.borrow_mut().set(var.clone(), value);
            }
            eval(body_expr, new_env)
        }
    }
}

fn main() {
    let global_env = Rc::new(RefCell::new(Environment::new()));
    let program = r#"(let ((x 1) (y 2)) (add x y))"#;
    // println!("{}", program);
    // println!("{:?}", tokenize(program));
    println!("{}", pretty_print(&parse(program)));
    match eval(&parse(program), global_env) {
        Ok(value) => println!("{:?}", value),
        Err(e) => println!("Oh Shid: {}", e),
    };
}
