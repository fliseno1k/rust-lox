use super::ast::*;
use super::token::*;
use crate::common::*;
use crate::parser::Parser;
use crate::position::{Span, WithSpan};

fn parse_program(it: &mut Parser) -> Result<Vec<WithSpan<Stmt>>, ()> {
    let mut statements = Vec::new();
    while !it.is_eof() {
        statements.push(parse_declaration(it)?);
    }

    Ok(statements)
}

fn parse_declaration(it: &mut Parser) -> Result<WithSpan<Stmt>, ()> {
    match it.peek() {
        TokenKind::Var => parse_var_declaration(it),
        _ => parse_expr_statement(it),
    }
}

fn parse_function_declaration(it: &mut Parser) -> Result<WithSpan<Stmt>, ()> {
    let begin_path = it.expect(TokenKind::Fun)?;
    let fun = parse_function(it)?;

    let span = Span::union(begin_path, &fun);
    Ok(WithSpan::new(fun.value, span))
}

fn parse_var_declaration(it: &mut Parser) -> Result<WithSpan<Stmt>, ()> {
    let begin_span = it.expect(TokenKind::Var)?;
    let name = expect_identifier(it)?;
    let mut initializer = None;

    if it.optionally(TokenKind::Equal)? {
        initializer = Some(parse_expr(it)?);
    }

    let end_span = it.expect(TokenKind::Semicolon)?;
    Ok(WithSpan::new(
        Stmt::Var(name, initializer.map(Box::new)),
        Span::union(begin_span, end_span),
    ))
}

fn parse_function(it: &mut Parser) -> Result<WithSpan<Stmt>, ()> {
    let name = expect_identifier(it)?;
    it.expect(TokenKind::LeftParenthesis)?;
    let params = if it.check(TokenKind::RightParenthesis) {
        parse_params(it)?
    } else {
        Vec::new()
    }
    it.expect(TokenKind::RightParenthesis)?;
    it.expect(TokenKind::LeftBrace)?;
    let mut body: Vec<WithSpan<Stmt>> = Vec::new();
    while !it.check(TokenKind::RightBrace) {
        body.push(parse_declaration(it)?);
    }
    let end_span = it.expect(TokenKind::RightBrace)?;
    Ok(WithSpan::new(Stmt::Function(name.clone(), params, body), Span::union(&name, end_span)))

}

fn parse_params(it: &mut Parser) -> Result<Vec<WithSpan<Identifier>>, ()> {
    let mut params: Vec<WithSpan<Identifier>> = Vec::new();
    params.push(expect_identifier(it)?);
    while it.check(TokenKind::Comma) {
        it.expect(TokenKind::Comma)?;
        params.push(expect_identifier(it)?);
    }

    Ok(params)
}

fn parse_expr_statement(it: &mut Parser) -> Result<WithSpan<Stmt>, ()> {
    let expr = parse_expr(it)?;
    let end_span = it.expect(TokenKind::Semicolon)?;

    let span = Span::union(&expr, end_span);
    Ok(WithSpan::new(Stmt::Expression(Box::new(expr)), span))
}

fn parse_expr(it: &mut Parser) -> Result<WithSpan<Expr>, ()> {
    super::expr_parser::parse(it)
}

pub fn parse(it: &mut Parser) -> Result<Vec<WithSpan<Stmt>>, ()> {
    parse_program(it)
}
