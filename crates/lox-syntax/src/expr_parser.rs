use super::ast::*;
use super::token::*;
use crate::common::*;
use crate::parser::Parser;
use crate::position::{Span, WithSpan};

#[allow(dead_code)]
#[derive(PartialEq, PartialOrd, Clone, Copy)]
enum Precedence {
    None,
    Assign,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    List,
    Primary,
}

impl<'a> From<TokenKind> for Precedence {
    fn from(token: TokenKind) -> Precedence {
        match token {
            TokenKind::Equal => Precedence::Assign,
            TokenKind::Or => Precedence::Or,
            TokenKind::And => Precedence::And,
            TokenKind::BangEqual | TokenKind::EqualEqual => Precedence::Equality,
            TokenKind::Less
            | TokenKind::LessEqual
            | TokenKind::Greater
            | TokenKind::GreaterEqual => Precedence::Comparison,
            TokenKind::Plus | TokenKind::Minus => Precedence::Term,
            TokenKind::Star | TokenKind::Slash => Precedence::Factor,
            TokenKind::Bang => Precedence::Unary,
            TokenKind::LeftParenthesis => Precedence::Call,
            TokenKind::Dot => Precedence::Call,
            TokenKind::LeftBracket => Precedence::List,
            _ => Precedence::None,
        }
    }
}

pub fn parse(it: &mut Parser) -> Result<WithSpan<Expr>, ()> {
    parse_expr(it, Precedence::None)
}

fn parse_expr(it: &mut Parser, precedence: Precedence) -> Result<WithSpan<Expr>, ()> {
    let mut expr = parse_prefix(it)?;
    while !it.is_eof() {
        let next_precedence = Precedence::from(it.peek());
        if precedence >= next_precedence {
            break;
        }
        expr = parse_infix(it, expr)?;
    }

    Ok(expr)
}

fn parse_prefix(it: &mut Parser) -> Result<WithSpan<Expr>, ()> {
    match it.peek() {
        TokenKind::Number
        | TokenKind::Nil
        | TokenKind::This
        | TokenKind::True
        | TokenKind::False
        | TokenKind::Identifier
        | TokenKind::Super
        | TokenKind::String => parse_primary(it),
        TokenKind::Bang | TokenKind::Minus => parse_unary(it),
        TokenKind::LeftParenthesis => parse_grouping(it),
        TokenKind::LeftBracket => parse_list(it),
        _ => {
            it.error(
                &format!("Unexpected {}", it.peek_token().value),
                it.peek_token().span,
            );
            Err(())
        }
    }
}

fn parse_infix(it: &mut Parser, left: WithSpan<Expr>) -> Result<WithSpan<Expr>, ()> {
    match it.peek() {
        TokenKind::BangEqual
        | TokenKind::EqualEqual
        | TokenKind::Less
        | TokenKind::LessEqual
        | TokenKind::Greater
        | TokenKind::GreaterEqual
        | TokenKind::Plus
        | TokenKind::Minus
        | TokenKind::Star
        | TokenKind::Slash => parse_binary(it, left),
        TokenKind::And | TokenKind::Or => parse_logical(it, left),
        TokenKind::Equal => parse_assign(it, left),
        TokenKind::LeftParenthesis => parse_call(it, left),
        TokenKind::LeftBracket => parse_list_get(it, left),
        TokenKind::Dot => parse_get(it, left),
        _ => {
            it.error(
                &format!("Unexpected {}", it.peek_token().value),
                it.peek_token().span,
            );
            Err(())
        }
    }
}

fn parse_primary(it: &mut Parser) -> Result<WithSpan<Expr>, ()> {
    let token = it.advance();
    match &token.value {
        &Token::Nil => Ok(WithSpan::new(Expr::Nil, token.span)),
        &Token::This => Ok(WithSpan::new(Expr::This, token.span)),
        &Token::Number(n) => Ok(WithSpan::new(Expr::Number(n), token.span)),
        &Token::True => Ok(WithSpan::new(Expr::Boolean(true), token.span)),
        &Token::False => Ok(WithSpan::new(Expr::Boolean(false), token.span)),
        &Token::String(ref s) => Ok(WithSpan::new(Expr::String(s.clone()), token.span)),
        &Token::Identifier(ref i) => Ok(WithSpan::new(
            Expr::Variable(WithSpan::new(i.clone(), token.span)),
            token.span,
        )),
        &Token::Super => parse_super(it, &token),
        _ => {
            it.error(&format!("Expected primary got {}", token.value), token.span);
            Err(())
        }
    }
}

fn parse_super(it: &mut Parser, keyword: &WithSpan<Token>) -> Result<WithSpan<Expr>, ()> {
    it.expect(TokenKind::Dot)?;
    let name = expect_identifier(it)?;

    let span = Span::union(keyword, &name);
    Ok(WithSpan::new(Expr::Super(name), span))
}

fn parse_unary(it: &mut Parser) -> Result<WithSpan<Expr>, ()> {
    let operator = parse_unary_op(it)?;
    let right = parse_expr(it, Precedence::Unary)?;

    let span = Span::union(&operator, &right);
    Ok(WithSpan::new(Expr::Unary(operator, Box::new(right)), span))
}

fn parse_binary(it: &mut Parser, left: WithSpan<Expr>) -> Result<WithSpan<Expr>, ()> {
    let precedence = Precedence::from(it.peek());
    let operator = parse_binary_op(it)?;
    let right = parse_expr(it, precedence)?;

    let span = Span::union(&left, &right);
    Ok(WithSpan::new(
        Expr::Binary(Box::new(left), operator, Box::new(right)),
        span,
    ))
}

fn parse_grouping(it: &mut Parser) -> Result<WithSpan<Expr>, ()> {
    let left_parent = it.expect(TokenKind::LeftParenthesis)?;
    let expr = parse_expr(it, Precedence::None)?;
    let right_paren = it.expect(TokenKind::RightParenthesis)?;

    let span = Span::union(left_parent, right_paren);
    Ok(WithSpan::new(Expr::Grouping(Box::new(expr)), span))
}

fn parse_list(it: &mut Parser) -> Result<WithSpan<Expr>, ()> {
    let left_bracket = it.expect(TokenKind::LeftBracket)?;
    let items = parse_list_items(it)?;
    let right_bracket = it.expect(TokenKind::RightBracket)?;

    let span = Span::union(left_bracket, right_bracket);
    Ok(WithSpan::new(Expr::List(items), span))
}

fn parse_logical(it: &mut Parser, left: WithSpan<Expr>) -> Result<WithSpan<Expr>, ()> {
    let precedence = Precedence::from(it.peek());
    let operator = parse_logical_op(it)?;
    let right = parse_expr(it, precedence)?;

    let span = Span::union(&left, &right);
    Ok(WithSpan::new(
        Expr::Logical(Box::new(left), operator, Box::new(right)),
        span,
    ))
}

fn parse_assign(it: &mut Parser, left: WithSpan<Expr>) -> Result<WithSpan<Expr>, ()> {
    it.expect(TokenKind::Equal)?;
    let right = parse_expr(it, Precedence::None)?;
    let span = Span::union(&left, &right);
    match &left.value {
        Expr::Variable(i) => Ok(WithSpan::new(
            Expr::Assign(i.clone(), Box::new(right)),
            span,
        )),
        Expr::Get(l, i) => Ok(WithSpan::new(
            Expr::Set(l.clone(), i.clone(), Box::new(right)),
            span,
        )),
        Expr::ListGet(l, i) => Ok(WithSpan::new(
            Expr::ListSet(l.clone(), i.clone(), Box::new(right)),
            span,
        )),
        _ => {
            it.error(&format!("Invalid left value"), left.span);
            Err(())
        }
    }
}

fn parse_call(it: &mut Parser, left: WithSpan<Expr>) -> Result<WithSpan<Expr>, ()> {
    it.expect(TokenKind::LeftParenthesis)?;
    let args = parse_arguments(it)?;
    let most_right = it.expect(TokenKind::RightParenthesis)?;

    let span = Span::union(&left, most_right);
    Ok(WithSpan::new(Expr::Call(Box::new(left), args), span))
}

fn parse_arguments(it: &mut Parser) -> Result<Vec<WithSpan<Expr>>, ()> {
    let mut args = Vec::new();
    if !it.check(TokenKind::RightParenthesis) {
        args.push(parse_expr(it, Precedence::None)?);
        while it.check(TokenKind::Comma) {
            it.expect(TokenKind::Comma)?;
            args.push(parse_expr(it, Precedence::None)?);
        }
    }
    Ok(args)
}

fn parse_get(it: &mut Parser, left: WithSpan<Expr>) -> Result<WithSpan<Expr>, ()> {
    it.expect(TokenKind::Dot)?;
    let tc = it.advance();
    match &tc.value {
        &Token::Identifier(ref i) => {
            let span = Span::union(&left, tc);
            Ok(WithSpan::new(
                Expr::Get(Box::new(left), WithSpan::new(i.clone(), tc.span)),
                span,
            ))
        }
        _ => {
            it.error(&format!("Expected identifier got {}", tc.value), tc.span);
            Err(())
        }
    }
}

fn parse_list_get(it: &mut Parser, left: WithSpan<Expr>) -> Result<WithSpan<Expr>, ()> {
    it.expect(TokenKind::LeftBracket)?;
    let right = parse_expr(it, Precedence::None)?;
    let end = it.expect(TokenKind::RightBracket)?;

    let span = Span::union(&left, end);
    Ok(WithSpan::new(
        Expr::ListGet(Box::new(left), Box::new(right)),
        span,
    ))
}

fn parse_list_items(it: &mut Parser) -> Result<Vec<WithSpan<Expr>>, ()> {
    let mut args = Vec::new();
    if !it.check(TokenKind::RightBracket) {
        args.push(parse_expr(it, Precedence::None)?);
        while it.check(TokenKind::Comma) {
            it.expect(TokenKind::Comma)?;
            args.push(parse_expr(it, Precedence::None)?)
        }
    }
    Ok(args)
}

fn parse_unary_op(it: &mut Parser) -> Result<WithSpan<UnaryOperator>, ()> {
    let token = it.advance();
    match &token.value {
        &Token::Bang => Ok(WithSpan::new(UnaryOperator::Bang, token.span)),
        &Token::Minus => Ok(WithSpan::new(UnaryOperator::Minus, token.span)),
        _ => {
            it.error(
                &format!("expected unary operator got {}", token.value),
                token.span,
            );
            Err(())
        }
    }
}

fn parse_binary_op(it: &mut Parser) -> Result<WithSpan<BinaryOperator>, ()> {
    let token = it.advance();
    let operator = match &token.value {
        &Token::BangEqual => BinaryOperator::BangEqual,
        &Token::EqualEqual => BinaryOperator::EqualEqual,
        &Token::Less => BinaryOperator::Less,
        &Token::LessEqual => BinaryOperator::LessEqual,
        &Token::Greater => BinaryOperator::Greater,
        &Token::GreaterEqual => BinaryOperator::GreaterEqual,
        &Token::Plus => BinaryOperator::Plus,
        &Token::Minus => BinaryOperator::Minus,
        &Token::Star => BinaryOperator::Star,
        &Token::Slash => BinaryOperator::Slash,
        _ => {
            it.error(
                &format!("Expected binary operator got {}", token.value),
                token.span,
            );
            return Err(());
        }
    };

    Ok(WithSpan::new(operator, token.span))
}

fn parse_logical_op(it: &mut Parser) -> Result<WithSpan<LogicalOperator>, ()> {
    let token = it.advance();
    let operator = match &token.value {
        &Token::And => LogicalOperator::And,
        &Token::Or => LogicalOperator::Or,
        _ => {
            it.error(
                &format!("Expected logical operator got {}", token.value),
                token.span,
            );
            return Err(());
        }
    };

    Ok(WithSpan::new(operator, token.span))
}
