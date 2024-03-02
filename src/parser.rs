
// This is a specific parser for a simple expression language
// it can parse single line expressions with the exact two variables x and y
// it can also parse constants: pi, e
// it assumses all numbers are floating point numbers
// It returns a closeure that takes a tuple of two f64 (x, y) and returns a f64


use std::iter::Peekable;

type VectorFunctionType = dyn Fn((f64, f64, f64)) -> f64;

// vector or scalar function
enum Function {
    V(Box<Expression>, Box<Expression>),
    S(Box<Expression>)
}

enum Expression {
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mult(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    
    Brackets(Box<Expression>),
    Number(f64),
    Variable(Var),

    Neg(Box<Expression>),
    Sin(Box<Expression>),
    Cos(Box<Expression>),
    Tan(Box<Expression>),
    Abs(Box<Expression>),
    Floor(Box<Expression>),
    Ceil(Box<Expression>),
    Sqrt(Box<Expression>),
    Mod(Box<Expression>, Box<Expression>),
    Pow(Box<Expression>, Box<Expression>),
    Len(Box<Expression>, Box<Expression>, Box<Expression>, Box<Expression>),
}

#[derive(Clone)]
enum Token {
    Operator(char),
    Number(f64),
    Const(String),
    Variable(Var),
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
           Token::Operator(c) => {
                match c {
                    'S' => write!(f, "sin"),
                    'C' => write!(f, "cos"),
                    'T' => write!(f, "tan"),
                    'q' => write!(f, "sqrt"),
                    'a' => write!(f, "abs"),
                    // 'l' => write!(f, "len"),
                    c => write!(f, "{}", c),
                }
           },
           Token::Number(n) => write!(f, "{}", n),
           Token::Const(s) => write!(f, "{}", s),
           Token::Variable(v) => {
                match v {
                    Var::X => write!(f, "x"),
                    Var::Y => write!(f, "y"),
                    Var::T => write!(f, "t"),
                    Var::R => write!(f, "r"),
                }
           }
        }
    }
}

#[derive(Debug, Clone)]
enum Var {
    X,
    Y,
    T,
    R,
}


fn lexer(input: &String) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();
    let mut token_string = String::new();
    for c in input.chars() {
        match c {
            '+' | '-' | '*' | '/' | '(' | ')' | ',' | '^' | '%' => {
                if !token_string.is_empty() {
                    tokens.push(lex_token_string(token_string)?);
                    token_string = String::new();
                }
                tokens.push(Token::Operator(c));
            }
            ' ' | '\t' | '\n' => {}
            c => token_string.push(c),
        }
    }
    if !token_string.is_empty() {
        tokens.push(lex_token_string(token_string)?);
    }

    Ok(tokens)
}

fn lex_token_string(token_string: String) -> Result<Token, String> {
    match token_string.as_str() {
        "pi" | "e" => Ok(Token::Const(token_string)),
        "x" => Ok(Token::Variable(Var::X)),
        "y" => Ok(Token::Variable(Var::Y)),
        "t" => Ok(Token::Variable(Var::T)),
        "r" => Ok(Token::Variable(Var::R)),
        "sin" => Ok(Token::Operator('S')),
        "cos" => Ok(Token::Operator('C')),
        "tan" => Ok(Token::Operator('T')),
        "sqrt" => Ok(Token::Operator('q')),
        "abs" => Ok(Token::Operator('a')),
        "len" => Ok(Token::Operator('l')),
        "floor" => Ok(Token::Operator('f')),
        "ceil" => Ok(Token::Operator('c')),
        
        n => {
            let num = n.parse::<f64>();
            match num {
                Ok(n) => Ok(Token::Number(n)),
                Err(_) => Err(format!("could not parse '{}'",n)),
            }
        },
    }
}


fn field_function_parser(tokens_iter: impl Iterator<Item = Token>) -> Result<Function, String> {
    // parse a function of the form f(x, y) -> (x, y)
    let mut tokens_iter = tokens_iter.peekable();

    // remove opening bracket    
    match tokens_iter.next() {
        Some(Token::Operator('(')) => {}
        Some(e) => return Err(format!("Function must start with '(' not '{}'", e)),
        None => return Err(format!("Function is empty")),
    }

    // parse left hand side and then expect a comma
    let x = prase_add_sub(&mut tokens_iter)?;
    match tokens_iter.next() {
        Some(Token::Operator(',')) => {}
        Some(e) => return Err(format!("expected ',' not '{}'", e)),
        None => return Err(format!("Function must be two dimensional, seperated by ','")),
    }

    // parse right hand side and then expect a closing bracket
    let y = prase_add_sub(&mut tokens_iter)?;
    match tokens_iter.next() {
        Some(Token::Operator(')')) => {}
        Some(e) => return Err(format!("expected ')' not '{}'", e)),
        None => return Err(format!("Function must end with ')'")),
    }
    
    match tokens_iter.next() {
        None => {}
        Some(token) => return Err(format!("expected end of input, got {:}", token)),
    }

    Ok(Function::V(Box::new(x), Box::new(y)))
}

#[allow(dead_code)]
fn parser(tokens_iter: impl Iterator<Item = Token>) -> Result<Function, String> {
    let mut tokens_iter = tokens_iter.peekable();
    let f =  prase_add_sub(&mut tokens_iter)?;

    match tokens_iter.next() {
        None => {}
        Some(token) => return Err(format!("expected end of input, got {:}", token)),
    }

    Ok(Function::S(Box::new(f)))
}


fn prase_add_sub(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expression, String> {
    // first go as deep into the recursion as possible (parse_mult_div)
    let mut lhs = parse_mult_div(tokens)?;
    loop {
        match tokens.peek() {
            Some(Token::Operator('+')) => {
                tokens.next();
                let rhs = parse_mult_div(tokens)?;
                lhs = Expression::Add(Box::new(lhs), Box::new(rhs));
            }
            Some(Token::Operator('-')) => {
                tokens.next();
                let rhs = parse_mult_div(tokens)?;
                lhs = Expression::Sub(Box::new(lhs), Box::new(rhs));
            }
            _ => break,
        }
    }
    Ok(lhs)
}

fn parse_mult_div(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expression, String> {
    // first go as deep into the recursion as possible
    let mut lhs = parse_exponent(tokens)?;

    loop {
        // while we can still see a '*' or '/' operator (with numbers in between)
        match tokens.peek() {
            Some(Token::Operator('*')) => {
                tokens.next();
                let rhs = parse_exponent(tokens)?;
                lhs = Expression::Mult(Box::new(lhs), Box::new(rhs));
            }
            Some(Token::Operator('/')) => {
                tokens.next();
                let rhs = parse_exponent(tokens)?;
                lhs = Expression::Div(Box::new(lhs), Box::new(rhs));
            }
            Some(Token::Operator('%')) => {
                tokens.next();
                let rhs = parse_exponent(tokens)?;
                lhs = Expression::Mod(Box::new(lhs), Box::new(rhs));
            }
            // we see anything else we break and move up in recursion layers
            _ => break,
        }
    }
    Ok(lhs)
}


fn parse_exponent(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expression, String> {
    // first go as deep into the recursion as possible
    let mut lhs = parse_num_bracket(tokens)?;

    loop {
        // while we can still see a '^' operator (with numbers in between)
        match tokens.peek() {
            Some(Token::Operator('^')) => {
                tokens.next();
                let rhs = parse_exponent(tokens)?;
                lhs = Expression::Pow(Box::new(lhs), Box::new(rhs));
            },
            // we see anything else we break and move up in recursion layers
            _ => break,
        }
    }
    Ok(lhs)
}


macro_rules! parse_expression_variant {
    ($tokens:expr, $expr_variant:ident) => {{
        $tokens.next();
        let inside = parse_num_bracket($tokens)?;
        Ok(Expression::$expr_variant(Box::new(inside)))
    }};
}
fn parse_num_bracket(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expression, String> {
    // parse a number or opening bracket
    // if we see a number we start the bottom of an expression,
    // if we se a bracket we proceed back to the top without doing anything
    match tokens.peek() {
        Some(Token::Number(n)) => {
            let number = *n;
            tokens.next();
            Ok(Expression::Number(number))
        }
        Some(Token::Operator('(')) => {
            tokens.next();
            // now if we parse the next expression we will get the expression inside the brackets
            // and then we epect the next token to be the corresponding closing bracket
            let inside = prase_add_sub(tokens)?;
            match tokens.next() {
                Some(Token::Operator(')')) =>  Ok(Expression::Brackets(Box::new(inside))),
                Some(token) => Err(format!("expected ')' not '{}'" , token)),
                None => Err(format!("expected ')' not end of input")),
            }
        }
        Some(Token::Operator('l')) => {
            tokens.next();
            // expect a '('
            let x1: Expression;
            let y1: Expression;
            let x2: Expression;
            let y2: Expression;
            match tokens.next() {
                Some(Token::Operator('(')) =>  x1 = prase_add_sub(tokens)?,
                Some(token) => return Err(format!("in fn len argument 1: expected '(' not '{}'" , token)),
                None => return Err(format!("in fn len argument 1: '(' not end of input")),
            }
            // expect a ','
            match tokens.next() {
                Some(Token::Operator(',')) =>  y1 = prase_add_sub(tokens)?,
                Some(token) => return Err(format!("in fn len argument 2: expected ',' not '{}'" , token)),
                None => return Err(format!("in fn len argument 2: ',' not end of input")),
            }
            // expect a ','
            match tokens.next() {
                Some(Token::Operator(',')) =>  x2 = prase_add_sub(tokens)?,
                Some(token) => return Err(format!("in fn len argument 3: expected ',' not '{}'" , token)),
                None => return Err(format!("in fn len argument 3: ',' not end of input")),
            }
            // expect a ','
            match tokens.next() {
                Some(Token::Operator(',')) =>  y2 = prase_add_sub(tokens)?,
                Some(token) => return Err(format!("in fn len argument 4: expected ',' not '{}'" , token)),
                None => return Err(format!("in fn len argument 4: ',' not end of input")),
            }
            // expect a ')'
            match tokens.next() {
                Some(Token::Operator(')')) => Ok(Expression::Len(Box::new(x1), Box::new(y1), Box::new(x2), Box::new(y2))),
                Some(e) => Err(format!("in fn len expected ')' not '{}'", e)),
                None => Err(format!("in fn len expected ')' not end of input")),
            }
        }
        Some(Token::Operator('-')) => { // unary minus
            parse_expression_variant!(tokens, Neg)
        }
        Some(Token::Operator('S')) => {
            parse_expression_variant!(tokens, Sin)
        }
        Some(Token::Operator('C')) => {
            parse_expression_variant!(tokens, Cos)
        }
        Some(Token::Operator('T')) => {
            parse_expression_variant!(tokens, Tan)
        }
        Some(Token::Operator('a')) => {
            parse_expression_variant!(tokens, Abs)
        }
        Some(Token::Operator('q')) => {
            parse_expression_variant!(tokens, Sqrt)
        }
        Some(Token::Operator('f')) => {
            parse_expression_variant!(tokens, Floor)
        }
        Some(Token::Operator('c')) => {
            parse_expression_variant!(tokens, Ceil)
        }
        Some(Token::Const(s)) => {
            let num = match s.as_str() {
                "pi" => std::f64::consts::PI,
                "e" => std::f64::consts::E,
                s => Err(format!("unknown constant: {}", s))?,
            };
            tokens.next();
            Ok(Expression::Number(num))
        }
        Some(Token::Variable(v)) => {
            let var = v.clone();
            tokens.next();
            Ok(Expression::Variable(var))
        }
        Some(Token::Operator(c)) => Err(format!("expected expression, not '{}'", c)),

        None => Err(format!("expected expression, not end of input")),
    }
}



fn expression_to_str(exp: &Expression) -> String { 
    match exp {
        Expression::Add(a, b) => format!("{} + {}", expression_to_str(a), expression_to_str(b)),
        Expression::Sub(a, b) => format!("{} - {}", expression_to_str(a), expression_to_str(b)),
        Expression::Mult(a, b) => format!("{} * {}", expression_to_str(a), expression_to_str(b)),
        Expression::Div(a, b) => format!("{} / {}", expression_to_str(a), expression_to_str(b)),
        Expression::Number(n) => n.to_string(),
        Expression::Brackets(a) => format!("({})", expression_to_str(a)),
        Expression::Variable(s) => format!("{:?}", s),
        Expression::Sin(a) => format!("sin{}", expression_to_str(a)),
        Expression::Cos(a) => format!("cos{}", expression_to_str(a)),
        Expression::Tan(a) => format!("tan{}", expression_to_str(a)),
        Expression::Sqrt(a) => format!("sqrt{}", expression_to_str(a)),
        Expression::Abs(a) => format!("abs{}", expression_to_str(a)),
        Expression::Neg(a) => format!("-{}", expression_to_str(a)),
        Expression::Len(a, b, c, d) => format!(
            "len({}, {}, {}, {})", 
            expression_to_str(a), 
            expression_to_str(b), 
            expression_to_str(c), 
            expression_to_str(d)
        ),
        Expression::Pow(a, b) => format!("{}^{}", expression_to_str(a), expression_to_str(b)),
        Expression::Mod(a, b) => format!("{}%{}", expression_to_str(a), expression_to_str(b)),
        Expression::Floor(a) => format!("floor{}", expression_to_str(a)),
        Expression::Ceil(a) => format!("ceil{}", expression_to_str(a)),
    }
}

macro_rules! evaluate_unary_expression {
    ($expr:expr, $op:expr) => {{
        let eval_a = evaluate(*$expr);
        Box::new(move |(x, y, t)| $op(eval_a((x, y, t))))
    }};
}
macro_rules! evaluate_binary_expression {
    ($expr_a:expr, $expr_b:expr, $op:expr) => {{
        let eval_a = evaluate(*$expr_a);
        let eval_b = evaluate(*$expr_b);
        Box::new(move |(x, y, t)| $op(eval_a((x, y, t)), eval_b((x, y, t))))
    }};
}

// returns a closed form of the expression with (x, y) as arguments this should be macro
fn evaluate(exp: Expression) -> Box<VectorFunctionType> {
    match exp {
        // single argument
        Expression::Variable(s) => {
            Box::new(move |(x, y, t)| {
                match s {
                    Var::X => x,
                    Var::Y => y,
                    Var::T => t,
                    Var::R => (x*x + y*y).sqrt(),
                }
            })
        }
        Expression::Number(n) => {
            let number = n;
            Box::new(move |(_, _, _)| number)
        }
        Expression::Neg(a) => {
            evaluate_unary_expression!(a, |x: f64| -x)
        }
        Expression::Sin(a) => {
            evaluate_unary_expression!(a, |x: f64| f64::sin(x))
        }
        Expression::Abs(a) => {
            evaluate_unary_expression!(a, |x: f64| f64::abs(x))
        }
        Expression::Cos(a) => {
            evaluate_unary_expression!(a, |x: f64| f64::cos(x))
        }
        Expression::Tan(a) => {
            evaluate_unary_expression!(a, |x: f64| f64::tan(x))
        }
        Expression::Sqrt(a) => {
            evaluate_unary_expression!(a, |x: f64| f64::sqrt(x))
        }
        Expression::Ceil(a) => {
            evaluate_unary_expression!(a, |x: f64| f64::ceil(x))
        }
        Expression::Floor(a) => {
            evaluate_unary_expression!(a, |x: f64| f64::floor(x))
        }
        Expression::Brackets(a) => {
            let eval_a = evaluate(*a);
            Box::new(move |(x, y, t)| eval_a((x, y, t)))
        }
        // two arguments
        Expression::Add(a, b) => {
            evaluate_binary_expression!(a, b, |x, y| x + y)
        }
        Expression::Sub(a, b) => {
            evaluate_binary_expression!(a, b, |x, y| x - y)
        }
        Expression::Mult(a, b) => {
            evaluate_binary_expression!(a, b, |x, y| x * y)
        }
        Expression::Div(a, b) => {
            evaluate_binary_expression!(a, b, |x, y| x / y)
        }
        Expression::Pow(a, b) => {
            evaluate_binary_expression!(a, b, |x, y| f64::powf(x, y))
        }
        Expression::Mod(a, b) => {
            evaluate_binary_expression!(a, b, |x, y| ((x % y) + y) % y )
        }
        Expression::Len(x1, y1, x2, y2) => {
            let eval_x1 = evaluate(*x1);
            let eval_y1 = evaluate(*y1);
            let eval_x2 = evaluate(*x2);
            let eval_y2 = evaluate(*y2);
            Box::new(move |(x, y, t)| {
                let dx = eval_x2((x, y, t)) - eval_x1((x, y, t));
                let dy = eval_y2((x, y, t)) - eval_y1((x, y, t));
                f64::sqrt(dx * dx + dy * dy)
            })
        }
    }
}


pub fn pretty_print(input: String) -> String {
    let tokens = match lexer(&input) {
        Ok(tokens) => tokens,
        Err(e) => return e,
    };
    let fun = field_function_parser(tokens.into_iter());
    match fun.unwrap() {
        Function::V(a, b) => format!("(\n{}\n,\n{}\n)", expression_to_str(&a), expression_to_str(&b)),
        Function::S(a) => expression_to_str(&a),
    }
}

#[allow(dead_code)]
pub fn interpret(input: String) -> Result<Box<VectorFunctionType>, String> {
    let tokens = lexer(&input)?;
    let fun = parser(tokens.into_iter());
    match fun {
        Ok(Function::S(exp)) => Ok(evaluate(*exp)),
        Ok(Function::V(..)) => Err("expected scalar function".to_string()),
        Err(e) => Err(e),
    }
}

pub fn interpret_field_function(input: &String) -> Result<Box<dyn Fn((f64, f64, f64)) -> (f64, f64)>, String> {
    let tokens = lexer(&input)?;
    let fun = field_function_parser(tokens.into_iter());
    match fun {
        Ok(Function::V(exp_x, exp_y)) => {
            let eval_x = evaluate(*exp_x);
            let eval_y = evaluate(*exp_y);
            Ok(Box::new(move |(x, y, t)| (eval_x((x, y, t)), eval_y((x, y, t)))))
        }
        Ok(Function::S(..)) => Err("expected vector function".to_string()),
        Err(e) => Err(e),
    }
}

#[test]
fn test_all() {
    let x = 3.0;
    let y = 4.0;
    let t = 5.0;
    let arg = (x, y, t);
    assert_eq!(interpret("5".to_string()).unwrap()(arg), 5.0);
    assert_eq!(interpret("(5)".to_string()).unwrap()(arg), 5.0);
    assert_eq!(interpret("-5".to_string()).unwrap()(arg), -5.0);
    assert_eq!(interpret("x".to_string()).unwrap()(arg), x);
    assert_eq!(interpret("r".to_string()).unwrap()(arg), (x*x+y*y).sqrt());
    assert_eq!(interpret("x+y".to_string()).unwrap()(arg), x+y);
    assert_eq!(interpret("x*y".to_string()).unwrap()(arg), x*y);
    assert_eq!(interpret("x/y".to_string()).unwrap()(arg), x / y);
    assert_eq!(interpret("x/(y+x)".to_string()).unwrap()(arg), x / (y+x));
    assert_eq!(interpret("x^y".to_string()).unwrap()(arg), x.powf(y));
    assert_eq!(interpret("abs(x)".to_string()).unwrap()(arg), x.abs());
    assert_eq!(interpret("sin(x)".to_string()).unwrap()(arg), x.sin());
    assert_eq!(interpret("tan(x)".to_string()).unwrap()(arg), x.tan());
    assert_eq!(interpret("sqrt(x)".to_string()).unwrap()(arg), x.sqrt());
    assert_eq!(interpret("len(x, y, 0, 0)".to_string()).unwrap()(arg), (x*x+y*y).sqrt());
    assert_eq!(interpret("x%y".to_string()).unwrap()(arg), x % y);
    assert_eq!(interpret("floor(x)".to_string()).unwrap()(arg), x.floor());
    assert_eq!(interpret("ceil(x)".to_string()).unwrap()(arg), x.ceil());
    assert_eq!(interpret("t".to_string()).unwrap()(arg), t);
    assert_eq!(interpret("x+t".to_string()).unwrap()(arg), x+t);

    // Combinations of functions
    assert_eq!(interpret("abs(x) + sin(y)".to_string()).unwrap()(arg), x.abs() + y.sin());
    assert_eq!(interpret("x*y - sqrt(x)".to_string()).unwrap()(arg), x * y - x.sqrt());
    assert_eq!(interpret("tan(x) * cos(y)".to_string()).unwrap()(arg), x.tan() * y.cos());
    assert_eq!(interpret("x / (y * 2)".to_string()).unwrap()(arg), x / (y * 2.0));
    assert_eq!(interpret("sin(x) + cos(x)".to_string()).unwrap()(arg), x.sin() + x.cos());
    assert_eq!(interpret("x^2 + y^2".to_string()).unwrap()(arg), x.powf(2.0) + y.powf(2.0));
    assert_eq!(interpret("x + y * 2".to_string()).unwrap()(arg), x + y * 2.0);
    assert_eq!(interpret("sqrt(x + y)".to_string()).unwrap()(arg), (x + y).sqrt());
    assert_eq!(interpret("len(x, y, 7, 2) / 2".to_string()).unwrap()(arg), ((x-7.0)*(x-7.0) + (y-2.0)*(y-2.0)).sqrt() / 2.0);
    assert_eq!(interpret("tan(sin(r) + cos(t))".to_string()).unwrap()(arg), ((x*x+y*y).sqrt().sin() + t.cos()).tan());
}

#[allow(dead_code)]
fn main() {
    // read from stdin
    println!("X=");
    let mut input = String::new();
    std::io::stdin().read_line(&mut input).unwrap();
    let x = input.trim().parse::<f64>().unwrap();
    println!("Y=");
    let mut input = String::new();
    std::io::stdin().read_line(&mut input).unwrap();
    let y = input.trim().parse::<f64>().unwrap();
    println!("Enter expression (empty line to quit):");
    let t = 0.0;
    loop {    
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        let input = input.trim();
        if input.is_empty() {
            break;
        }
        // tokenize 
        let tokens = lexer(&input.to_string()).unwrap();
        // parse
        let fun = parser(tokens.into_iter());
        let expression = match fun.unwrap() {
            Function::S(exp) => *exp,
            _ => panic!("expected scalar function"),
        };
        // print
        println!("{}", expression_to_str(&expression));
        // evaluate
        let result = evaluate(expression);
        println!("= {}", result((x, y, t)));
    }
}
