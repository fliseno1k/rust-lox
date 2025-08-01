use super::token::Token;
use crate::position::*;
use std::iter::Peekable;
use std::str;
use std::str::Chars;

struct Scanner<'a> {
    current_position: BytePos,
    it: Peekable<Chars<'a>>,
}

impl<'a> Scanner<'a> {
    fn new(buf: &str) -> Scanner {
        Scanner {
            current_position: BytePos::default(),
            it: buf.chars().peekable(),
        }
    }

    fn next(&mut self) -> Option<char> {
        let next = self.it.next();
        if let Some(c) = next {
            self.current_position = self.current_position.shift(c);
        }

        next
    }

    fn peek(&mut self) -> Option<&char> {
        self.it.peek()
    }

    fn consume_if<F>(&mut self, x: F) -> bool
    where
        F: Fn(char) -> bool,
    {
        if let Some(&ch) = self.peek() {
            if x(ch) {
                self.next().unwrap();
                return true;
            }
        }
        false
    }

    fn consume_if_next<F>(&mut self, x: F) -> bool
    where
        F: Fn(char) -> bool,
    {
        let mut it = self.it.clone();
        match it.next() {
            None => return false,
            _ => (),
        }

        if let Some(&ch) = it.peek() {
            if x(ch) {
                self.next().unwrap();
                return true;
            }
        }

        false
    }

    fn consume_while<F>(&mut self, x: F) -> Vec<char>
    where
        F: Fn(char) -> bool,
    {
        let mut chars: Vec<char> = Vec::new();
        while let Some(&ch) = self.peek() {
            if x(ch) {
                self.next().unwrap();
                chars.push(ch);
            } else {
                break;
            }
        }

        chars
    }
}

struct Lexer<'a> {
    it: Scanner<'a>,
}

impl<'a> Lexer<'a> {
    fn new(buf: &str) -> Lexer {
        Lexer {
            it: Scanner::new(buf),
        }
    }

    fn match_token(&mut self, ch: char) -> Option<Token> {
        match ch {
            '=' => Some(self.either('=', Token::EqualEqual, Token::Equal)),
            '!' => Some(self.either('=', Token::BangEqual, Token::Bang)),
            '<' => Some(self.either('=', Token::LessEqual, Token::Less)),
            '>' => Some(self.either('=', Token::GreaterEqual, Token::Greater)),
            ' ' => None,
            '/' => {
                if self.it.consume_if(|ch| ch == '/') {
                    self.it.consume_while(|ch| ch != '\n');
                    return None;
                }

                Some(Token::Slash)
            }
            '\n' => None,
            '\t' => None,
            '\r' => None,
            '"' => {
                let string: String = self.it.consume_while(|ch| ch != '"').into_iter().collect();
                match self.it.next() {
                    None => Some(Token::UnterminatedString),
                    _ => Some(Token::String(string)),
                }
            }
            x if x.is_numeric() => self.number(x),
            x if x.is_ascii_alphabetic() || x == '_' => self.identifier(x),
            '.' => Some(Token::Dot),
            '(' => Some(Token::LeftParenthesis),
            ')' => Some(Token::RightParenthesis),
            '{' => Some(Token::LeftBrace),
            '}' => Some(Token::RightBrace),
            '[' => Some(Token::LeftBracket),
            ']' => Some(Token::RightBracket),
            ',' => Some(Token::Comma),
            '-' => Some(Token::Minus),
            '+' => Some(Token::Plus),
            ';' => Some(Token::Semicolon),
            '*' => Some(Token::Semicolon),
            c => Some(Token::Unknown(c)),
        }
    }

    fn either(&mut self, to_match: char, matched: Token, unmatched: Token) -> Token {
        if self.it.consume_if(|ch| ch == to_match) {
            return matched;
        }

        unmatched
    }

    fn number(&mut self, x: char) -> Option<Token> {
        let mut number = String::new();
        number.push(x);
        let num: String = self
            .it
            .consume_while(|a| a.is_numeric())
            .into_iter()
            .collect();
        number.push_str(num.as_str());

        if self.it.peek() == Some(&'.') && self.it.consume_if_next(|ch| ch.is_numeric()) {
            let num2: String = self
                .it
                .consume_while(|a| a.is_numeric())
                .into_iter()
                .collect();
            number.push('.');
            number.push_str(num2.as_str());
        }

        Some(Token::Number(number.parse::<f64>().unwrap()))
    }

    fn identifier(&mut self, x: char) -> Option<Token> {
        let mut identifier = String::new();
        identifier.push(x);
        let rest: String = self
            .it
            .consume_while(|a| a.is_ascii_alphanumeric() || a == '_')
            .into_iter()
            .collect();
        identifier.push_str(rest.as_str());

        match self.keyword(&identifier) {
            None => Some(Token::Identifier(identifier)),
            Some(token) => Some(token),
        }
    }

    fn keyword(&self, identifier: &str) -> Option<Token> {
        use std::collections::HashMap;

        let mut keywords: HashMap<&str, Token> = HashMap::new();
        keywords.insert("and", Token::And);
        keywords.insert("class", Token::Class);
        keywords.insert("else", Token::Else);
        keywords.insert("false", Token::False);
        keywords.insert("for", Token::For);
        keywords.insert("fun", Token::Fun);
        keywords.insert("if", Token::If);
        keywords.insert("print", Token::Print);
        keywords.insert("return", Token::Return);
        keywords.insert("super", Token::Super);
        keywords.insert("this", Token::This);
        keywords.insert("true", Token::True);
        keywords.insert("var", Token::Var);
        keywords.insert("while", Token::While);
        keywords.insert("import", Token::Import);

        match keywords.get(identifier) {
            None => None,
            Some(token) => Some(token.clone()),
        }
    }

    fn tokenize_with_context(&mut self) -> Vec<WithSpan<Token>> {
        let mut tokens: Vec<WithSpan<Token>> = Vec::new();

        loop {
            let initial_position = self.it.current_position;
            let ch = match self.it.next() {
                None => break,
                Some(c) => c,
            };

            if let Some(token) = self.match_token(ch) {
                tokens.push(WithSpan::new(
                    token,
                    Span {
                        start: initial_position,
                        end: self.it.current_position,
                    },
                ));
            }
        }

        tokens
    }
}

pub fn tokenize_with_context(buf: &str) -> Vec<WithSpan<Token>> {
    Lexer::new(buf).tokenize_with_context()
}
