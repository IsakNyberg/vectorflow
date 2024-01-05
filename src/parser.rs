
// This is a specific parser for a simple expression language
// it can parse single line expressions with the exact two variables x and y
// it can also parse constants: pi, e
// it assumses all numbers are floating point numbers
// It returns a closeure that takes a tuple of two f64 (x, y) and returns a f64


use std::iter::Peekable;

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
    Sqrt(Box<Expression>),
    Len(Box<Expression>, Box<Expression>),
    Pow(Box<Expression>, Box<Expression>),
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
                    's' => write!(f, "sin"),
                    'c' => write!(f, "cos"),
                    't' => write!(f, "tan"),
                    'q' => write!(f, "sqrt"),
                    'a' => write!(f, "abs"),
                    'l' => write!(f, "len"),
                    c => write!(f, "{}", c),
                }
           },
           Token::Number(n) => write!(f, "{}", n),
           Token::Const(s) => write!(f, "{}", s),
           Token::Variable(v) => {
                match v {
                     Var::X => write!(f, "x"),
                     Var::Y => write!(f, "y"),
                }
           }
        }
    }
}

#[derive(Debug, Clone)]
enum Var {
    X,
    Y,
}


fn lexer(input: &String) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();
    let mut token_string = String::new();
    for c in input.chars() {
        match c {
            '+' | '-' | '*' | '/' | '(' | ')' | ',' | '^' => {
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
        "sin" => Ok(Token::Operator('s')),
        "cos" => Ok(Token::Operator('c')),
        "tan" => Ok(Token::Operator('t')),
        "sqrt" => Ok(Token::Operator('q')),
        "abs" => Ok(Token::Operator('a')),
        "len" => Ok(Token::Operator('l')),
        
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
        Some(e) => return Err(format!("in fn len expected '(' not '{}'", e)),
        None => return Err(format!("in fn len expected '(' not end of input")),
    }

    // parse left hand side and then expect a comma
    let x = prase_add_sub(&mut tokens_iter)?;
    match tokens_iter.next() {
        Some(Token::Operator(',')) => {}
        Some(e) => return Err(format!("in fn len expected ',' not '{}'", e)),
        None => return Err(format!("in fn len expected ',' not end of input")),
    }

    // parse right hand side and then expect a closing bracket
    let y = prase_add_sub(&mut tokens_iter)?;
    match tokens_iter.next() {
        Some(Token::Operator(')')) => {}
        Some(e) => return Err(format!("expected ')' not '{}'", e)),
        None => return Err(format!("expected ')' not end of input")),
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
    let mut lhs = parse_function(tokens)?;

    loop {
        // while we can still see a '*' or '/' operator (with numbers in between)
        match tokens.peek() {
            Some(Token::Operator('*')) => {
                tokens.next();
                let rhs = parse_function(tokens)?;
                lhs = Expression::Mult(Box::new(lhs), Box::new(rhs));
            }
            Some(Token::Operator('/')) => {
                tokens.next();
                let rhs = parse_function(tokens)?;
                lhs = Expression::Div(Box::new(lhs), Box::new(rhs));
            }
            // we see anything else we break and move up in recursion layers
            _ => break,
        }
    }
    Ok(lhs)
}


fn parse_function(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expression, String> {
    // first go as deep into the recursion as possible
    let mut lhs = parse_num_bracket(tokens)?;

    loop {
        // while we can still see a '^' operator (with numbers in between)
        match tokens.peek() {
            Some(Token::Operator('^')) => {
                tokens.next();
                let rhs = parse_num_bracket(tokens)?;
                lhs = Expression::Pow(Box::new(lhs), Box::new(rhs));
            }
            // we see anything else we break and move up in recursion layers
            _ => break,
        }
    }
    Ok(lhs)
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
            let x: Expression;
            let y: Expression;
            match tokens.next() {
                Some(Token::Operator('(')) =>  x = prase_add_sub(tokens)?,
                Some(token) => return Err(format!("in fn len expected '(' not '{}'" , token)),
                None => return Err(format!("in fn len expected '(' not end of input")),
            }
            // expect a ','
            match tokens.next() {
                Some(Token::Operator(',')) =>  y = prase_add_sub(tokens)?,
                Some(token) => return Err(format!("in fn len expected ',' not '{}'" , token)),
                None => return Err(format!("in fn len expected ',' not end of input")),
            }
            // expect a ')'
            match tokens.next() {
                Some(Token::Operator(')')) => Ok(Expression::Len(Box::new(x), Box::new(y))),
                Some(e) => Err(format!("in fn len expected ')' not '{}'", e)),
                None => Err(format!("in fn len expected ')' not end of input")),
            }
        }
        Some(Token::Operator('-')) => {
            // unary minus
            tokens.next();
            let inside = parse_num_bracket(tokens)?;
            Ok(Expression::Neg(Box::new(inside)))

        }
        Some(Token::Operator('s')) => {
            tokens.next();
            let inside = parse_num_bracket(tokens)?;
            Ok(Expression::Sin(Box::new(inside)))
        }
        Some(Token::Operator('c')) => {
            tokens.next();
            let inside = parse_num_bracket(tokens)?;
            Ok(Expression::Cos(Box::new(inside)))
        }
        Some(Token::Operator('t')) => {
            tokens.next();
            let inside = parse_num_bracket(tokens)?;
            Ok(Expression::Tan(Box::new(inside)))
        }
        Some(Token::Operator('a')) => {
            tokens.next();
            let inside = parse_num_bracket(tokens)?;
            Ok(Expression::Abs(Box::new(inside)))
        }
        Some(Token::Operator('q')) => {
            tokens.next();
            let inside = parse_num_bracket(tokens)?;
            Ok(Expression::Sqrt(Box::new(inside)))
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
        Expression::Len(a, b) => format!("len({}, {})", expression_to_str(a), expression_to_str(b)),
        Expression::Pow(a, b) => format!("{}^{}", expression_to_str(a), expression_to_str(b)),
    }
}

// returns a closed form of the expression with (x, y) as arguments this should be macro
fn evaluate(exp: Expression) -> Box<dyn Fn((f64, f64)) -> f64> {
    match exp {
        Expression::Add(a, b) => {
            let eval_a = evaluate(*a);
            let eval_b = evaluate(*b);
            Box::new(move |(x, y)| eval_a((x, y)) + eval_b((x, y)))
        }
        Expression::Sub(a, b) => {
            let eval_a = evaluate(*a);
            let eval_b = evaluate(*b);
            Box::new(move |(x, y)| eval_a((x, y)) - eval_b((x, y)))
        }
        Expression::Mult(a, b) => {
            let eval_a = evaluate(*a);
            let eval_b = evaluate(*b);
            Box::new(move |(x, y)| eval_a((x, y)) * eval_b((x, y)))
        }
        Expression::Div(a, b) => {
            let eval_a = evaluate(*a);
            let eval_b = evaluate(*b);
            Box::new(move |(x, y)| eval_a((x, y)) / eval_b((x, y)))
        }
        Expression::Pow(a, b) => {
            let eval_a = evaluate(*a);
            let eval_b = evaluate(*b);
            Box::new(move |(x, y)| f64::powf(eval_a((x, y)), eval_b((x, y)))) 
        }
        Expression::Len(a, b) => {
            let eval_a = evaluate(*a);
            let eval_b = evaluate(*b);
            Box::new(move |(x, y)| {
                let dx = eval_a((x, y));
                let dy = eval_b((x, y));
                f64::sqrt(dx*dx + dy*dy)
            })
        }
        Expression::Neg(a) => {
            let eval_a = evaluate(*a);
            Box::new(move |(x, y)| -eval_a((x, y)))
        }
        Expression::Sin(a) => {
            let eval_a = evaluate(*a);
            Box::new(move |(x, y)| eval_a((x, y)).sin())
        }
        Expression::Abs(a) => {
            let eval_a = evaluate(*a);
            Box::new(move |(x, y)| f64::abs(eval_a((x, y))))
        }
        Expression::Cos(a) => {
            let eval_a = evaluate(*a);
            Box::new(move |(x, y)| eval_a((x, y)).cos())
        }
        Expression::Tan(a) => {
            let eval_a = evaluate(*a);
            Box::new(move |(x, y)| eval_a((x, y)).tan())
        }
        Expression::Sqrt(a) => {
            let eval_a = evaluate(*a);
            Box::new(move |(x, y)| eval_a((x, y)).sqrt())
        }
        Expression::Number(n) => {
            let number = n;
            Box::new(move |(_, _)| number)
        }
        Expression::Brackets(a) => {
            let eval_a = evaluate(*a);
            Box::new(move |(x, y)| eval_a((x, y)))
        }
        Expression::Variable(s) => {
            Box::new(move |(x, y)| {
                match s {
                    Var::X => x,
                    Var::Y => y,                    
                }
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
pub fn interpret(input: String) -> Result<Box<dyn Fn((f64, f64)) -> f64>, String> {
    let tokens = lexer(&input)?;
    let fun = parser(tokens.into_iter());
    match fun {
        Ok(Function::S(exp)) => Ok(evaluate(*exp)),
        Ok(Function::V(..)) => Err("expected scalar function".to_string()),
        Err(e) => Err(e),
    }
}

pub fn interpret_field_function(input: &String) -> Result<Box<dyn Fn((f64, f64)) -> (f64, f64)>, String> {
    let tokens = lexer(&input)?;
    let fun = field_function_parser(tokens.into_iter());
    match fun {
        Ok(Function::V(exp_x, exp_y)) => {
            let eval_x = evaluate(*exp_x);
            let eval_y = evaluate(*exp_y);
            Ok(Box::new(move |(x, y)| (eval_x((x, y)), eval_y((x, y)))))
        }
        Ok(Function::S(..)) => Err("expected vector function".to_string()),
        Err(e) => Err(e),
    }
}

#[test]
fn test_all() {
    let x = 3.0;
    let y = 4.0;
    let arg = (x, y);
    assert_eq!(interpret("5".to_string()).unwrap()(arg), 5.0);
    assert_eq!(interpret("-5".to_string()).unwrap()(arg), -5.0);
    assert_eq!(interpret("x+y".to_string()).unwrap()(arg), x+y);
    assert_eq!(interpret("x*y".to_string()).unwrap()(arg), x*y);
    assert_eq!(interpret("x/y".to_string()).unwrap()(arg), x / y);
    assert_eq!(interpret("x/(y+x)".to_string()).unwrap()(arg), x / (y+x));
    assert_eq!(interpret("x^y".to_string()).unwrap()(arg), x.powf(y));
    assert_eq!(interpret("abs(x)".to_string()).unwrap()(arg), x.abs());
    assert_eq!(interpret("sin(x)".to_string()).unwrap()(arg), x.sin());
    assert_eq!(interpret("tan(x)".to_string()).unwrap()(arg), x.tan());
    assert_eq!(interpret("sqrt(x)".to_string()).unwrap()(arg), x.sqrt());
    assert_eq!(interpret("len(x, y)".to_string()).unwrap()(arg), (x*x+y*y).sqrt());

    // Combinations of functions
    assert_eq!(interpret("abs(x) + sin(y)".to_string()).unwrap()(arg), x.abs() + y.sin());
    assert_eq!(interpret("x*y - sqrt(x)".to_string()).unwrap()(arg), x * y - x.sqrt());
    assert_eq!(interpret("tan(x) * cos(y)".to_string()).unwrap()(arg), x.tan() * y.cos());
    assert_eq!(interpret("x / (y * 2)".to_string()).unwrap()(arg), x / (y * 2.0));
    assert_eq!(interpret("sin(x) + cos(x)".to_string()).unwrap()(arg), x.sin() + x.cos());
    assert_eq!(interpret("x^2 + y^2".to_string()).unwrap()(arg), x.powf(2.0) + y.powf(2.0));
    assert_eq!(interpret("x + y * 2".to_string()).unwrap()(arg), x + y * 2.0);
    assert_eq!(interpret("sqrt(x + y)".to_string()).unwrap()(arg), (x + y).sqrt());
    assert_eq!(interpret("len(x, y) / 2".to_string()).unwrap()(arg), (x * x + y * y).sqrt() / 2.0);
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
        println!("= {}", result((x, y)));
    }
}
