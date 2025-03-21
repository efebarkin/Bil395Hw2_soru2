use std::collections::HashMap;
use std::io::{self, Write};

#[derive(Debug)]
enum Token {
    Number(f64),
    Plus,
    Minus,
    Multiply,
    Divide,
    Power,
    LeftParen,
    RightParen,
    Identifier(String),
    Assign,
}

#[derive(Debug)]
enum ParseError {
    InvalidToken(String),
    UnexpectedToken(String),
    UnbalancedParentheses,
    DivisionByZero,
    UndefinedVariable(String),
}

struct Calculator {
    variables: HashMap<String, f64>,
}

impl Calculator {
    fn new() -> Self {
        Calculator {
            variables: HashMap::new(),
        }
    }

    fn tokenize(&self, input: &str) -> Result<Vec<Token>, ParseError> {
        let mut tokens = Vec::new();
        let mut chars = input.chars().peekable();

        while let Some(&c) = chars.peek() {
            match c {
                '0'..='9' | '.' => {
                    let mut number = String::new();
                    while let Some(&c) = chars.peek() {
                        if c.is_digit(10) || c == '.' {
                            number.push(c);
                            chars.next();
                        } else {
                            break;
                        }
                    }
                    match number.parse::<f64>() {
                        Ok(n) => tokens.push(Token::Number(n)),
                        Err(_) => return Err(ParseError::InvalidToken(number)),
                    }
                }
                '+' => {
                    tokens.push(Token::Plus);
                    chars.next();
                }
                '-' => {
                    tokens.push(Token::Minus);
                    chars.next();
                }
                '*' => {
                    tokens.push(Token::Multiply);
                    chars.next();
                }
                '/' => {
                    tokens.push(Token::Divide);
                    chars.next();
                }
                '^' => {
                    tokens.push(Token::Power);
                    chars.next();
                }
                '(' => {
                    tokens.push(Token::LeftParen);
                    chars.next();
                }
                ')' => {
                    tokens.push(Token::RightParen);
                    chars.next();
                }
                '=' => {
                    tokens.push(Token::Assign);
                    chars.next();
                }
                'a'..='z' | 'A'..='Z' => {
                    let mut identifier = String::new();
                    while let Some(&c) = chars.peek() {
                        if c.is_alphanumeric() || c == '_' {
                            identifier.push(c);
                            chars.next();
                        } else {
                            break;
                        }
                    }
                    tokens.push(Token::Identifier(identifier));
                }
                ' ' | '\t' => {
                    chars.next();
                }
                _ => {
                    return Err(ParseError::InvalidToken(c.to_string()));
                }
            }
        }

        Ok(tokens)
    }

    fn parse_expression(&mut self, tokens: &[Token]) -> Result<f64, ParseError> {
        if tokens.is_empty() {
            return Ok(0.0);
        }

        // Check for assignment
        if let Some(Token::Identifier(var_name)) = tokens.get(0) {
            if let Some(Token::Assign) = tokens.get(1) {
                let var_name = var_name.clone();
                let value = self.parse_expression(&tokens[2..])?;
                self.variables.insert(var_name, value);
                return Ok(value);
            }
        }

        self.parse_addition_subtraction(tokens)
    }

    fn parse_addition_subtraction(&mut self, tokens: &[Token]) -> Result<f64, ParseError> {
        let mut result = self.parse_multiplication_division(tokens)?;
        let mut i = 0;

        while i < tokens.len() {
            match &tokens[i] {
                Token::Plus => {
                    result += self.parse_multiplication_division(&tokens[i + 1..])?;
                    break;
                }
                Token::Minus => {
                    result -= self.parse_multiplication_division(&tokens[i + 1..])?;
                    break;
                }
                _ => i += 1,
            }
        }

        Ok(result)
    }

    fn parse_multiplication_division(&mut self, tokens: &[Token]) -> Result<f64, ParseError> {
        let mut result = self.parse_power(tokens)?;
        let mut i = 0;

        while i < tokens.len() {
            match &tokens[i] {
                Token::Multiply => {
                    result *= self.parse_power(&tokens[i + 1..])?;
                    break;
                }
                Token::Divide => {
                    let divisor = self.parse_power(&tokens[i + 1..])?;
                    if divisor == 0.0 {
                        return Err(ParseError::DivisionByZero);
                    }
                    result /= divisor;
                    break;
                }
                _ => i += 1,
            }
        }

        Ok(result)
    }

    fn parse_power(&mut self, tokens: &[Token]) -> Result<f64, ParseError> {
        let mut result = self.parse_primary(tokens)?;
        let mut i = 0;

        while i < tokens.len() {
            match &tokens[i] {
                Token::Power => {
                    let exponent = self.parse_primary(&tokens[i + 1..])?;
                    result = result.powf(exponent);
                    break;
                }
                _ => i += 1,
            }
        }

        Ok(result)
    }

    fn parse_primary(&mut self, tokens: &[Token]) -> Result<f64, ParseError> {
        if tokens.is_empty() {
            return Err(ParseError::UnexpectedToken("Beklenen ifade bulunamadı.".to_string()));
        }
    
        match &tokens[0] {
            Token::Number(n) => Ok(*n),
            Token::Identifier(name) => {
                match self.variables.get(name) {
                    Some(value) => Ok(*value),
                    None => Err(ParseError::UndefinedVariable(name.clone())),
                }
            }
            Token::LeftParen => {
                // Parantez içi ifadeleri ayrıştır
                let mut depth = 1;
                let mut i = 1;
                while i < tokens.len() && depth > 0 {
                    match tokens[i] {
                        Token::LeftParen => depth += 1,
                        Token::RightParen => depth -= 1,
                        _ => {}
                    }
                    i += 1;
                }
    
                if depth != 0 {
                    return Err(ParseError::UnbalancedParentheses);
                }
    
                self.parse_expression(&tokens[1..i-1])
            }
            Token::Minus => {
                // Negatif sayılar için destek
                let value = self.parse_primary(&tokens[1..])?;
                Ok(-value)
            }
            _ => Err(ParseError::UnexpectedToken(format!("Beklenmeyen token: {:?}", tokens[0]))),
        }
    }

    fn evaluate(&mut self, input: &str) -> Result<f64, ParseError> {
        let tokens = self.tokenize(input)?;
        let result = self.parse_expression(&tokens);
        
        if let Err(ref err) = result {
            match err {
                ParseError::InvalidToken(token) => println!("Hata: Geçersiz token '{}'", token),
                ParseError::UnexpectedToken(token) => println!("Hata: Beklenmeyen token '{}'", token),
                ParseError::UnbalancedParentheses => println!("Hata: Parantezler dengesiz!"),
                ParseError::DivisionByZero => println!("Hata: Sıfıra bölme hatası!"),
                ParseError::UndefinedVariable(var) => println!("Hata: Tanımsız değişken '{}'", var),
            }
        }
    
        result
    }
}

fn main() {
    let mut calculator = Calculator::new();
    
    println!("Rust Calculator");
    println!("Enter expressions with variables and assignments (e.g., 'x = 5', 'y = x + 3')");
    println!("Type 'exit' to quit");
    
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();
        
        let input = input.trim();
        if input == "exit" {
            break;
        }
        
        match calculator.evaluate(input) {
            Ok(result) => println!("{}", result),
            Err(e) => println!("Error: {:?}", e),
        }
    }
}