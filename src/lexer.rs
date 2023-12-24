use std::collections::HashMap;
use std::fmt;
use std::iter::Iterator;
use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum TokenKind {
    Literal,
    Identifier,
    StringLiteral,

    Percent,
    Semicolon,
    Comma,

    Assignment,
    Let,

    Import,
    Function,
    Struct,
    Colon,
    Extern,
    Const,

    If,
    Else,

    Add,
    Sub,
    Div,
    Mul,
    Dot,
    Ampersand,

    Greater,
    GreaterEquals,
    Less,
    LessEquals,
    NotEquals,
    Equals,
    Not,

    And,
    Or,

    While,
    Arrow,
    Return,

    True,
    False,

    LeftParen,
    RightParen,

    LeftBracket,
    RightBracket,

    LeftCurly,
    RightCurly,

    HashTag,

    Eof,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Span {
    pub row: usize,
    pub col: usize,
    pub file_name: String,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub value: String,

    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Lexer {
    pub source: String,
    pub start: usize,
    pub current: usize,
    pub keywords: HashMap<String, TokenKind>,

    pub row: usize,
    pub col: usize,
    pub file_name: String,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name: String = match *self {
            TokenKind::Literal => "Literal".into(),
            TokenKind::Identifier => "Identifier".into(),
            TokenKind::StringLiteral => "StringLiteral".into(),

            TokenKind::Percent => "%".into(),
            TokenKind::Semicolon => ";".into(),
            TokenKind::Comma => ",".into(),
            TokenKind::Extern => ",".into(),

            TokenKind::Import => "import".into(),
            TokenKind::Assignment => "=".into(),
            TokenKind::Let => "let".into(),
            TokenKind::Function => "fn".into(),
            TokenKind::Struct => "struct".into(),
            TokenKind::Colon => ":".into(),
            TokenKind::Const => "const".into(),

            TokenKind::If => "if".into(),
            TokenKind::Else => "else".into(),

            TokenKind::Add => "+".into(),
            TokenKind::Sub => "-".into(),
            TokenKind::Div => "/".into(),
            TokenKind::Mul => "*".into(),
            TokenKind::Dot => ".".into(),
            TokenKind::Ampersand => "&".into(),

            TokenKind::Greater => ">".into(),
            TokenKind::GreaterEquals => ">=".into(),
            TokenKind::Less => "<".into(),
            TokenKind::LessEquals => "<=".into(),
            TokenKind::NotEquals => "!=".into(),
            TokenKind::Equals => "==".into(),
            TokenKind::Not => "!".into(),

            TokenKind::And => "and".into(),
            TokenKind::Or => "or".into(),

            TokenKind::While => "while".into(),
            TokenKind::Arrow => "->".into(),
            TokenKind::Return => "return".into(),

            TokenKind::True => "true".into(),
            TokenKind::False => "false".into(),

            TokenKind::RightParen => ")".into(),
            TokenKind::LeftParen => "(".into(),

            TokenKind::RightBracket => "]".into(),
            TokenKind::LeftBracket => "[".into(),

            TokenKind::RightCurly => "}".into(),
            TokenKind::LeftCurly => "{".into(),

            TokenKind::HashTag => "#".into(),

            TokenKind::Eof => "eof".into(),
        };

        write!(f, "{name}")
    }
}

impl Token {
    pub fn kind_loc(kind: TokenKind, row: usize, col: usize) -> Token {
        let span = Span {
            file_name: "".into(),
            row,
            col,
        };

        Token {
            kind,
            value: kind.to_string(),
            span,
        }
    }

    pub fn kind_span(kind: TokenKind, span: Span) -> Token {
        Token {
            kind,
            value: kind.to_string(),
            span,
        }
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        /* For now this is good enough but we might shoot ourselfs in the foot with it later. */
        self.kind == other.kind && self.value == other.value
    }
}

impl Lexer {
    pub fn new(source: String, file_name: String) -> Self {
        let mut keywords = HashMap::new();

        keywords.insert("let".into(), TokenKind::Let);
        keywords.insert("and".into(), TokenKind::And);
        keywords.insert("or".into(), TokenKind::Or);
        keywords.insert("true".into(), TokenKind::True);
        keywords.insert("false".into(), TokenKind::False);
        keywords.insert("while".into(), TokenKind::While);
        keywords.insert("if".into(), TokenKind::If);
        keywords.insert("else".into(), TokenKind::Else);
        keywords.insert("struct".into(), TokenKind::Struct);
        keywords.insert("fn".into(), TokenKind::Function);
        keywords.insert("return".into(), TokenKind::Return);
        keywords.insert("extern".into(), TokenKind::Extern);
        keywords.insert("import".into(), TokenKind::Import);
        keywords.insert("const".into(), TokenKind::Const);

        Self {
            source,
            start: 0,
            current: 0,
            keywords,

            col: 1,
            row: 1,
            file_name: file_name.clone(),
        }
    }

    pub fn span(&self) -> Span {
        Span {
            file_name: self.file_name.clone(),
            row: self.row,
            col: self.col,
        }
    }

    pub fn numeric(&mut self) -> Token {
        let span = self.span();
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            self.advance();
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        let value = &self.source[self.start..self.current];

        Token {
            kind: TokenKind::Literal,
            value: value.into(),
            span,
        }
    }

    pub fn identifier(&mut self) -> Token {
        let span = self.span();
        while self.peek().is_alphabetic() || self.peek().is_ascii_digit() || self.peek() == '_' {
            self.advance();
        }

        let value = &self.source[self.start..self.current];

        let mut token = Token {
            kind: TokenKind::Identifier,
            value: value.into(),
            span,
        };

        if let Some(kind) = self.keywords.get(value) {
            token.kind = *kind;
        }

        token
    }

    pub fn string(&mut self) -> Token {
        let span = self.span();

        /* Advance to consume the first " in the literal. */
        self.advance();

        while self.peek() != '"' && !self.is_at_end() {
            self.advance();
        }

        /* Advance to consume the last " in the literal */

        self.advance();

        let value = &self.source[(self.start + 1)..(self.current - 1)];

        Token {
            kind: TokenKind::StringLiteral,
            value: value.into(),
            span,
        }
    }

    pub fn advance(&mut self) -> char {
        if self.is_at_end() {
            return '\0';
        }

        self.current += 1;
        self.col += 1;

        let val = self.source.chars().nth(self.current - 1);
        val.unwrap()
    }

    pub fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    pub fn peek_next(&self) -> char {
        self.source.chars().nth(self.current + 1).unwrap()
    }

    pub fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source.chars().nth(self.current).unwrap()
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        if self.current >= self.source.len() {
            return Some(Token::kind_loc(TokenKind::Eof, self.row, self.col));
        }

        let c = self.peek();
        self.start = self.current;

        match c {
            '\n' => {
                self.row += 1;
                self.col = 0;
                self.advance();
                self.next()
            }
            '&' => {
                let span = self.span();
                let token = Token::kind_span(TokenKind::Ampersand, span);
                self.advance();
                Some(token)
            }
            '.' => {
                let span = self.span();
                let token = Token::kind_span(TokenKind::Dot, span);
                self.advance();
                Some(token)
            }
            ',' => {
                let span = self.span();
                let token = Token::kind_span(TokenKind::Comma, span);
                self.advance();
                Some(token)
            }
            ':' => {
                let span = self.span();
                let token = Token::kind_span(TokenKind::Colon, span);
                self.advance();
                Some(token)
            }
            ';' => {
                let span = self.span();
                let token = Token::kind_span(TokenKind::Semicolon, span);
                self.advance();
                Some(token)
            }
            '%' => {
                let span = self.span();
                let token = Token::kind_span(TokenKind::Percent, span);
                self.advance();
                Some(token)
            }
            ')' => {
                let span = self.span();
                let token = Token::kind_span(TokenKind::RightParen, span);
                self.advance();
                Some(token)
            }
            '(' => {
                let span = self.span();
                let token = Token::kind_span(TokenKind::LeftParen, span);
                self.advance();
                Some(token)
            }
            ']' => {
                let span = self.span();
                let token = Token::kind_span(TokenKind::RightBracket, span);
                self.advance();
                Some(token)
            }
            '[' => {
                let span = self.span();
                let token = Token::kind_span(TokenKind::LeftBracket, span);
                self.advance();
                Some(token)
            }
            '}' => {
                let span = self.span();
                let token = Token::kind_span(TokenKind::RightCurly, span);
                self.advance();
                Some(token)
            }
            '{' => {
                let span = self.span();
                let token = Token::kind_span(TokenKind::LeftCurly, span);
                self.advance();
                Some(token)
            }
            '=' => {
                let span = self.span();
                self.advance();
                if self.peek() == '=' {
                    self.advance();
                    Some(Token::kind_span(TokenKind::Equals, span))
                } else {
                    Some(Token::kind_span(TokenKind::Assignment, span))
                }
            }
            '+' => {
                let span = self.span();
                let token = Token::kind_span(TokenKind::Add, span);
                self.advance();
                Some(token)
            }
            '>' => {
                let span = self.span();
                self.advance();
                if self.peek() == '=' {
                    self.advance();
                    Some(Token::kind_span(TokenKind::GreaterEquals, span))
                } else {
                    Some(Token::kind_span(TokenKind::Greater, span))
                }
            }
            '<' => {
                let span = self.span();
                self.advance();
                if self.peek() == '=' {
                    self.advance();
                    Some(Token::kind_span(TokenKind::LessEquals, span))
                } else {
                    Some(Token::kind_span(TokenKind::Less, span))
                }
            }
            '!' => {
                let span = self.span();
                self.advance();
                if self.peek() == '"' {
                    Some(Token::kind_span(TokenKind::NotEquals, span))
                } else {
                    Some(Token::kind_span(TokenKind::Not, span))
                }
            }
            '-' => {
                let span = self.span();
                self.advance();
                if self.peek() == '>' {
                    self.advance();
                    let token = Token::kind_span(TokenKind::Arrow, span);
                    Some(token)
                } else {
                    let token = Token::kind_span(TokenKind::Sub, span);
                    Some(token)
                }
            }
            '/' => {
                let span = self.span();
                let token = Token::kind_span(TokenKind::Div, span);
                self.advance();
                Some(token)
            }
            '*' => {
                let span = self.span();
                let token = Token::kind_span(TokenKind::Mul, span);
                self.advance();
                Some(token)
            }
            '#' => {
                let span = self.span();
                let token = Token::kind_span(TokenKind::HashTag, span);
                self.advance();
                Some(token)
            }
            ' ' => {
                self.advance();
                self.next()
            }
            '\t' => {
                self.advance();
                self.next()
            }
            '"' => Some(self.string()),
            _ => {
                if c.is_ascii_digit() {
                    Some(self.numeric())
                } else {
                    Some(self.identifier())
                }
            }
        }
    }
}
