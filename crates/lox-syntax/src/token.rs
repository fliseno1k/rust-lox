use std::fmt::Display;

#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    // Single-character tokens;
    LeftParenthesis,
    RightParenthesis,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Start,

    // One or Two character tokens;
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals;
    Identifier(String),
    String(String),
    Number(f64),

    // Keywords;
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Import,

    // Other;
    Eof,
    UnterminatedString,
    Unknown(char),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TokenKind {
    // Single-character tokens;
    LeftParenthesis,
    RightParenthesis,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Start,

    // One or Two character tokens;
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals;
    Identifier,
    String,
    Number,

    // Keywords;
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Import,

    // Other;
    Eof,
    UnterminatedString,
    Unknown,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let kind: TokenKind = self.into();
        write!(f, "{}", kind)
    }
}

impl From<&Token> for TokenKind {
    fn from(token: &Token) -> Self {
        match token {
            Token::LeftParenthesis => TokenKind::LeftParenthesis,
            Token::RightParenthesis => TokenKind::RightParenthesis,
            Token::LeftBrace => TokenKind::LeftBrace,
            Token::RightBrace => TokenKind::RightBrace,
            Token::LeftBracket => TokenKind::LeftBracket,
            Token::RightBracket => TokenKind::RightBracket,
            Token::Comma => TokenKind::Comma,
            Token::Dot => TokenKind::Dot,
            Token::Minus => TokenKind::Minus,
            Token::Plus => TokenKind::Plus,
            Token::Semicolon => TokenKind::Semicolon,
            Token::Slash => TokenKind::Slash,
            Token::Start => TokenKind::Start,
            Token::Bang => TokenKind::Bang,
            Token::BangEqual => TokenKind::BangEqual,
            Token::Equal => TokenKind::Equal,
            Token::EqualEqual => TokenKind::EqualEqual,
            Token::Greater => TokenKind::Greater,
            Token::GreaterEqual => TokenKind::GreaterEqual,
            Token::Less => TokenKind::Less,
            Token::LessEqual => TokenKind::LessEqual,
            Token::Identifier(_) => TokenKind::Identifier,
            Token::String(_) => TokenKind::String,
            Token::Number(_) => TokenKind::Number,
            Token::And => TokenKind::And,
            Token::Class => TokenKind::Class,
            Token::Else => TokenKind::Else,
            Token::False => TokenKind::False,
            Token::Fun => TokenKind::Fun,
            Token::For => TokenKind::For,
            Token::If => TokenKind::If,
            Token::Nil => TokenKind::Nil,
            Token::Or => TokenKind::Or,
            Token::Print => TokenKind::Print,
            Token::Return => TokenKind::Return,
            Token::Super => TokenKind::Super,
            Token::This => TokenKind::This,
            Token::True => TokenKind::True,
            Token::Var => TokenKind::Var,
            Token::While => TokenKind::While,
            Token::Import => TokenKind::Import,
            Token::Eof => TokenKind::Eof,
            Token::UnterminatedString => TokenKind::UnterminatedString,
            Token::Unknown(_) => TokenKind::Unknown,
        }
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                LeftParenthesis => "')'",
                RightParenthesis => "'('",
                LeftBrace => "'{'",
                RightBrace => "'}'",
                LeftBracket => "'['",
                RightBracket => "']'",
                Comma => "','",
                Dot => "'.'",
                Minus => "'-'",
                Plus => "'+'",
                Semicolon => "';'",
                Slash => "'/'",
                Start => "'*'",
                Bang => "'!'",
                BangEqual => "'!='",
                Equal => "'='",
                EqualEqual => "'=='",
                Greater => "'>'",
                GreaterEqual => "'>='",
                Less => "'<'",
                LessEqual => "'<='",
                Identifier => "identifier",
                String => "string",
                Number => "number",
                And => "'and'",
                Class => "'class'",
                Else => "'else'",
                False => "'false'",
                Fun => "'fun'",
                For => "'for'",
                If => "'if'",
                Nil => "nil",
                Or => "'or'",
                Print => "'print'",
                Return => "'return'",
                Super => "'super'",
                This => "'this'",
                True => "'true'",
                Var => "'var'",
                While => "'while'",
                Import => "'import'",
                Eof => "<EOF>",
                UnterminatedString => "<Unterminated String>",
                Unknown => "<Unknown>",
            }
        )
    }
}
