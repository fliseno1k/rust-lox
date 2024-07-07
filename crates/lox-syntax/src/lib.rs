pub mod ast;
pub mod position;

#[macro_use]
mod parser;
mod common;
mod expr_parser;
mod token;
mod tokenizer;

use ast::Ast;
use position::Diagnostic;

pub fn parse(code: &str) -> Result<Ast, Vec<Diagnostic>> {
    use tokenizer::tokenize_with_context;

    let tokens = tokenize_with_context(code);
    let mut parser = crate::parser::Parser::new(&tokens);
}
