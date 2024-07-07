pub mod position;

#[macro_use]
mod parser;
mod common;
mod token;
mod tokenizer;

pub fn parse(code: &str) {
    use tokenizer::tokenize_with_context;

    let tokens = tokenize_with_context(code);
    let mut parser = crate::parser::Parser::new(&tokens);
}
