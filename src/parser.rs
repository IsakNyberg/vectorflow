
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
    Sqrt(Box<Expression>),
}

#[derive(Debug, Clone)]
enum Token {
    Operator(char),
    Number(f64),
    Const(String),
    Variable(Var),
}

#[derive(Debug, Clone)]
enum Var {
    X,
    Y,
}


fn lexer(input: String) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut token_string = String::new();
    for c in input.chars() {
        match c {
            '+' | '-' | '*' | '/' | '(' | ')' | ',' => {
                if !token_string.is_empty() {
                    tokens.push(lex_token_string(token_string));
                    token_string = String::new();
                }
                tokens.push(Token::Operator(c));
            }
            ' ' | '\t' | '\n' => {}
            c => token_string.push(c),
        }
    }
    if !token_string.is_empty() {
        tokens.push(lex_token_string(token_string));
    }

    tokens
}

fn lex_token_string(token_string: String) -> Token {
    println!("token: {:?}",token_string);
    match token_string.as_str() {
        "pi" | "e" => Token::Const(token_string),
        "x" => Token::Variable(Var::X),
        "y" => Token::Variable(Var::Y),
        "sin" => Token::Operator('s'),
        "cos" => Token::Operator('c'),
        "tan" => Token::Operator('t'),
        "sqrt" => Token::Operator('q'),
        
        _ => Token::Number(token_string.parse().unwrap()),
    }
}


fn field_function_parser(tokens_iter: impl Iterator<Item = Token>) -> Function {
    // parse a function of the form f(x, y) -> (x, y)
    let mut tokens_iter = tokens_iter.peekable();

    // remove opening bracket    
    match tokens_iter.next() {
        Some(Token::Operator('(')) => {}
        _ => panic!("expected '('"),
    }

    // parse left hand side and then expect a comma
    let x = prase_add_sub(&mut tokens_iter);
    match tokens_iter.next() {
        Some(Token::Operator(',')) => {}
        _ => panic!("expected ','"),
    }

    // parse right hand side and then expect a closing bracket
    let y = prase_add_sub(&mut tokens_iter);
    match tokens_iter.next() {
        Some(Token::Operator(')')) => {}
        _ => panic!("expected ')'"),
    }

    // expect end of input
    match tokens_iter.next() {
        None => {}
        _ => panic!("expected end of input"),
    }

    Function::V(Box::new(x), Box::new(y))
}

fn parser(tokens_iter: impl Iterator<Item = Token>) -> Function {
    let mut tokens_iter = tokens_iter.peekable();
    Function::S(Box::new(prase_add_sub(&mut tokens_iter)))
}


fn prase_add_sub(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Expression {
    // first go as deep into the recursion as possible (parse_mult_div)
    let mut lhs = parse_mult_div(tokens);
    loop {
        match tokens.peek() {
            Some(Token::Operator('+')) => {
                tokens.next();
                let rhs = parse_mult_div(tokens);
                lhs = Expression::Add(Box::new(lhs), Box::new(rhs));
            }
            Some(Token::Operator('-')) => {
                tokens.next();
                let rhs = parse_mult_div(tokens);
                lhs = Expression::Sub(Box::new(lhs), Box::new(rhs));
            }
            _ => break,
        }
    }
    lhs
}

fn parse_mult_div(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Expression {
    // first go as deep into the recursion as possible
    let mut lhs = parse_num_bracket(tokens);

    loop {
        // while we can still see a '*' or '/' operator (with numbers in between)
        match tokens.peek() {
            Some(Token::Operator('*')) => {
                tokens.next();
                let rhs = parse_num_bracket(tokens);
                lhs = Expression::Mult(Box::new(lhs), Box::new(rhs));
            }
            Some(Token::Operator('/')) => {
                tokens.next();
                let rhs = parse_num_bracket(tokens);
                lhs = Expression::Div(Box::new(lhs), Box::new(rhs));
            }
            // we see anything else we break and move up in recursion layers
            _ => break,
        }
    }
    lhs
}


fn parse_num_bracket(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Expression {
    // parse a number or opening bracket
    // if we see a number we start the bottom of an expression,
    // if we se a bracket we proceed back to the top without doing anything
    match tokens.peek() {
        Some(Token::Number(n)) => {
            let number = *n;
            tokens.next();
            Expression::Number(number)
        }
        Some(Token::Operator('(')) => {
            tokens.next();
            // now if we parse the next expression we will get the expression inside the brackets
            // and then we epect the next token to be the corresponding closing bracket
            let inside = prase_add_sub(tokens);
            if let Some(Token::Operator(')')) = tokens.next() {
                Expression::Brackets(Box::new(inside))
            } else {
                panic!("expected closing bracket")
            }
        }
        Some(Token::Operator('-')) => {
            // unary minus
            tokens.next();
            let inside = parse_num_bracket(tokens);
            Expression::Neg(Box::new(inside))

        }
        Some(Token::Operator('s')) => {
            tokens.next();
            let inside = parse_num_bracket(tokens);
            Expression::Sin(Box::new(inside))
        }
        Some(Token::Operator('c')) => {
            tokens.next();
            let inside = parse_num_bracket(tokens);
            Expression::Cos(Box::new(inside))
        }
        Some(Token::Operator('t')) => {
            tokens.next();
            let inside = parse_num_bracket(tokens);
            Expression::Tan(Box::new(inside))
        }
        Some(Token::Operator('q')) => {
            tokens.next();
            let inside = parse_num_bracket(tokens);
            Expression::Sqrt(Box::new(inside))
        }
        Some(Token::Const(s)) => {
            let num = match s.as_str() {
                "pi" => std::f64::consts::PI,
                "e" => std::f64::consts::E,
                _ => panic!("unknown constant {}", s),
            };
            tokens.next();
            Expression::Number(num)
        }
        Some(Token::Variable(v)) => {
            let var = v.clone();
            tokens.next();
            Expression::Variable(var)
        }
        Some(Token::Operator(c)) => panic!("token {} not expected", c),

        e => panic!("expected number or '(' or function, not {:?}", e),
    }
}



fn expression_to_str(exp: &Expression) -> String { 
    match exp {
        Expression::Add(a, b) => format!("({} + {})", expression_to_str(a), expression_to_str(b)),
        Expression::Sub(a, b) => format!("({} - {})", expression_to_str(a), expression_to_str(b)),
        Expression::Mult(a, b) => format!("({} * {})", expression_to_str(a), expression_to_str(b)),
        Expression::Div(a, b) => format!("({} / {})", expression_to_str(a), expression_to_str(b)),
        Expression::Number(n) => n.to_string(),
        Expression::Brackets(a) => format!("({})", expression_to_str(a)),
        Expression::Variable(s) => format!("{:?}", s),
        Expression::Sin(a) => format!("sin{}", expression_to_str(a)),
        Expression::Cos(a) => format!("cos{}", expression_to_str(a)),
        Expression::Tan(a) => format!("tan{}", expression_to_str(a)),
        Expression::Sqrt(a) => format!("sqrt{}", expression_to_str(a)),
        Expression::Neg(a) => format!("-{}", expression_to_str(a)),
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
        Expression::Neg(a) => {
            let eval_a = evaluate(*a);
            Box::new(move |(x, y)| -eval_a((x, y)))
        }
        Expression::Sin(a) => {
            let eval_a = evaluate(*a);
            Box::new(move |(x, y)| eval_a((x, y)).sin())
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
    let tokens = lexer(input);
    let fun = field_function_parser(tokens.into_iter());
    match fun {
        Function::V(a, b) => format!("(\n{}\n,\n{}\n)", expression_to_str(&a), expression_to_str(&b)),
        Function::S(a) => expression_to_str(&a),
    }
}

pub fn interpret(input: String) -> Box<dyn Fn((f64, f64)) -> f64> {
    let tokens = lexer(input);
    let fun = parser(tokens.into_iter());
    match fun {
        Function::S(exp) => evaluate(*exp),
        _ => panic!("expected scalar function"),
    }
}

pub fn interpret_field_function(input: String) -> Box<dyn Fn((f64, f64)) -> (f64, f64)> {
    let tokens = lexer(input);
    let fun = field_function_parser(tokens.into_iter());
    match fun {
        Function::V(exp_x, exp_y) => {
            let eval_x = evaluate(*exp_x);
            let eval_y = evaluate(*exp_y);
            Box::new(move |(x, y)| (eval_x((x, y)), eval_y((x, y))))
        }
        _ => panic!("expected vector function"),
    }
}

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
        let tokens = lexer(input.to_string());
        // parse
        println!("{:?}", tokens);
        let fun = parser(tokens.into_iter());
        let expression = match fun {
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
