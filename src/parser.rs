use crate::ast::{
    BinaryOp, 
    expressions::{
        Expression,
        NamedArgument,
    }, 
    statements::{
        Statement,
        NamedParameter,
        IfStatement,
    }, 
    definitions::{
        ExplicitType
    }
};
use crate::lexer::{Lexer, Token, TokenKind, Span};
use crate::errors::{OceanError, Level, Step};
use crate::unescape::{unescape};
use std::iter::Peekable;
use std::process::id;
use crate::ast::expressions::{BinaryExpression, BooleanExpression, CallExpression, EmptyExpression, LiteralExpression, StringLiteralExpression, UnaryExpression, IdentifierExpression, LookupExpression};
use crate::ast::statements::{BlockStatement, ExpressionStatement, FunctionStatement, ImportStatement, LetStatement, ReturnStatement, WhileStatement, WithStatement};

type SyntaxResult<T> = Result<T, OceanError>;
type ParseResult<T> = Result<T, OceanError>;

pub struct Parser {
    lexer: Peekable<Lexer>,
    file_name: String,
    input: String,

    in_function: bool,

    pub ended: bool,
}

/* Credit to https://domenicquirl.github.io/blog/parsing-basics/#binary-operators */
trait Operator {
    /// Prefix operators bind their operand to the right
    fn prefix_binding_power(&self) -> Option<((), u8)>;

    /// Infix operators bind two operands, lhs and rhs
    fn infix_binding_power(&self) -> Option<(u8, u8)>;

    /// Postfix operators bind their operand to the left
    fn postfix_binding_power(&self) -> Option<(u8, ())>;
}

impl Operator for TokenKind {
    fn prefix_binding_power(&self) -> Option<((), u8)> {
        Some(match self {
            TokenKind::Sub => ((), 51),
            TokenKind::Not => ((), 101),
            _ => {
                return None;
            }
        })
    }

    fn infix_binding_power(&self) -> Option<(u8, u8)> {
        Some(match self {
            TokenKind::Or => (1, 2),
            TokenKind::And => (3, 4),
            TokenKind::Equals | TokenKind::NotEquals => (5, 6),
            TokenKind::Less | TokenKind::Greater | TokenKind::LessEquals | TokenKind::GreaterEquals => (7, 8),
            TokenKind::Add | TokenKind::Sub => (9, 10),
            TokenKind::Mul | TokenKind::Div => (11, 12),
            _ => return None,
        })
    }

    fn postfix_binding_power(&self) -> Option<(u8, ())> {
        None
    }
}

impl Parser {
    pub fn new(input: String, file_name: String) -> Self {
        Self {
            lexer: Lexer::new(input.clone(), file_name.clone()).peekable(),
            file_name: file_name.clone(),
            input: input.clone(),

            in_function: false,

            ended: false,
        } 
    }

    pub fn next_token(&mut self) -> SyntaxResult<Token> {
        self.lexer.next().ok_or_else(|| {
            let span = Span {
                file_name: self.file_name.clone(),
                row: self.input.len(),
                col: self.input.len(),
            };

            OceanError::new(
                Level::Error, 
                Step::Parsing, 
                span, 
                "Unexpected end of input.".into()
            )
        })
    }

    #[inline(always)]
    pub fn peek(&mut self) -> TokenKind {
        self.lexer
            .peek()
            .map(|token| token.kind)
            .unwrap_or(TokenKind::Eof)
    }

    #[inline(always)]
    pub fn at(&mut self, kind: TokenKind) -> bool {
        self.peek() == kind
    }

    #[inline(always)]
    pub fn multi_at(&mut self, kinds: &'static [TokenKind]) -> bool {
        kinds.contains(&self.peek())
    }

    pub fn consume(&mut self, expected: TokenKind) -> SyntaxResult<()> {
        let token = self.next_token()?;
        if token.kind != expected {
            Err(OceanError::new(
                Level::Error,
                Step::Parsing,
                token.span,
                format!(
                    "Unexpected token: \x1b[1m{}\x1b[0m. Expected token: \x1b[1m{}\x1b[0m", 
                    token.kind.to_string(), 
                    expected.to_string()
                ),
            ))
        } else {
            Ok(())
        }
    }

    pub fn consume_next(&mut self, expected: TokenKind) -> SyntaxResult<Token> {
        let token = self.next_token()?;
        if token.kind != expected {
            Err(OceanError::new(
                Level::Error,
                Step::Parsing,
                token.span,
                format!(
                    "Unexpected token: \x1b[1m{}\x1b[0m. Expected token: \x1b[1m{}\x1b[0m", 
                    token.kind.to_string(), 
                    expected.to_string()
                ),
            ))

        } else {
            Ok(token)
        }
    }

    pub fn parse_identifier(&mut self, lhs: Token) -> ParseResult<Expression> {
        if self.peek() == TokenKind::Dot {
            self.consume(TokenKind::Dot)?;
            let identifier = self.consume_next(TokenKind::Identifier)?;

            let mut lookup = LookupExpression {
                span: identifier.span.clone(),
                name: lhs.value.clone(),
                child: Some(Box::new(self.parse_identifier(identifier)?.as_lookup().unwrap())),
            };

            if self.peek() == TokenKind::Dot {
                self.consume(TokenKind::Dot)?;
                let lhs = self.consume_next(TokenKind::Identifier)?;
                lookup.child = Some(Box::new(self.parse_identifier(lhs)?.as_lookup().unwrap()));
            }

            Ok(Expression::Lookup(lookup))
        } else {
            Ok(Expression::Lookup(LookupExpression { span: lhs.span, name: lhs.value.clone(), child: None}))
        }
    }

    pub fn parse_literal(&mut self, lit: TokenKind) -> ParseResult<Expression> {
        let token = self.next_token()?;
        
        let literal = match lit {
            TokenKind::Literal => Expression::Literal(LiteralExpression { span: token.span, val: token.value.parse::<u64>().unwrap()}),
            TokenKind::True => Expression::Bool(BooleanExpression { span: token.span, val: true }),
            TokenKind::False => Expression::Bool(BooleanExpression { span: token.span, val: false}),
            TokenKind::Identifier => self.parse_identifier(token.clone())?,
            TokenKind::StringLiteral => { 
                let value = token.value.clone();
                let unescaped = unescape(&value).unwrap();
                Expression::StringLiteral(StringLiteralExpression { span: token.span, val: unescaped})
            },
            token => unimplemented!("Error handeling or token: {:?}", token)
        };

        Ok(literal)
    }

    pub fn parse_expression(&mut self, binding_power: u8, provided_lhs: Option<Expression>) -> ParseResult<Expression> {
        let mut lhs = match provided_lhs {
            Some(e) => e,
            None => {
                match self.peek() {
                    lit @ TokenKind::Literal
                        | lit @ TokenKind::StringLiteral
                        | lit @ TokenKind::Identifier
                        | lit @ TokenKind::True
                        | lit @ TokenKind::False => self.parse_literal(lit)?,
                    _ => {
                        let token = self.next_token()?;
                        return Err(
                            OceanError::new(Level::Error, Step::Parsing, token.span, format!("Unexpected token: \x1b[1m{}\x1b[0m. Expected \x1b[1mExpression\x1b[0m.", token.kind.to_string()))
                        );
                    }
                }
            }
        };

        if self.at(TokenKind::LeftParen) {
            return self.parse_function_call(lhs);
        }

        loop {
             let op = match self.peek() {
                op @ TokenKind::Add
                | op @ TokenKind::Sub
                | op @ TokenKind::Mul
                | op @ TokenKind::Div
                | op @ TokenKind::And
                | op @ TokenKind::Or
                | op @ TokenKind::Less
                | op @ TokenKind::Greater
                | op @ TokenKind::Not
                | op @ TokenKind::LessEquals
                | op @ TokenKind::GreaterEquals
                | op @ TokenKind::NotEquals
                | op @ TokenKind::Equals => op,

                TokenKind::RightParen
                | TokenKind::Let
                | TokenKind::Comma
                | TokenKind::RightBracket
                | TokenKind::RightCurly
                | TokenKind::LeftCurly
                | TokenKind::Semicolon
                | TokenKind::Eof => break,

                _ => {
                    let token = self.next_token()?;
                    return Err(
                        OceanError::new(
                            Level::Error, 
                            Step::Parsing, 
                            token.span, 
                            format!(
                                "Expected \x1b[1moperator\x1b[0m or \x1b[1mexpression\x1b[0m terminater but found: \x1b[1m{}\x1b[0m.", 
                                token.kind.to_string()
                            )
                        )
                    )
                }
            };

              if let Some((left_binding_power, ())) = op.postfix_binding_power() {
                if left_binding_power < binding_power {
                    // previous operator has higher binding power then new one
                    // --> end of expression
                    break;
                }

                let op_token = self.consume_next(op)?;
                lhs = Expression::Unary(UnaryExpression { span: op_token.span.clone(), op: BinaryOp::from_token_kind(&op), expr: Box::new(lhs )});
                // parsed an operator --> go round the loop again
                continue;
            }

           if let Some((left_binding_power, right_binding_power)) = op.infix_binding_power() {
                if left_binding_power < binding_power {
                    // previous operator has higher binding power then new one
                    // --> end of expression
                    break;
                }

                self.consume(op)?;

                let rhs = self.parse_expression(right_binding_power, None)?;
                lhs = Expression::Binary(BinaryExpression { span: lhs.span(), op: BinaryOp::from_token_kind(&op), lhs: Box::new(lhs), rhs: Box::new(rhs)});

                // parsed an operator --> go round the loop again
                continue;
            }
            break;
        }

        Ok(lhs)
    }

    pub fn parse_function_call(&mut self, lhs: Expression) -> ParseResult<Expression> {
        let start = self.consume_next(TokenKind::LeftParen)?; 
        let mut arguments = Vec::<NamedArgument>::new();
        while !self.at(TokenKind::RightParen) {
            let name_token = self.consume_next(TokenKind::Identifier)?;
            let name = name_token.value.clone();
            self.consume(TokenKind::Colon)?;

            let expr = self.parse_expression(0, None)?; 
            let argument = NamedArgument::new(name, expr, name_token.span);

            arguments.push(argument);

            if !self.at(TokenKind::Comma) {
                break;
            } else {
                self.consume(TokenKind::Comma)?;
            }
        }
        self.consume(TokenKind::RightParen)?;

        let name = lhs.as_lookup().expect("not lookup");

        Ok(Expression::Call(CallExpression { span: start.span, name, arguments }))
    }

    pub fn parse_block_body(&mut self) -> ParseResult<(Span, Vec<Statement>)> {
        let start = self.consume_next(TokenKind::LeftCurly)?;
        let mut body = Vec::<Statement>::new();

        while !self.at(TokenKind::RightCurly) {
            if self.multi_at(&[
                TokenKind::If, 
                TokenKind::Let, 
                TokenKind::Function, 
                TokenKind::While,
                TokenKind::LeftCurly,
                TokenKind::Return,
                TokenKind::Extern,
            ]) {
                body.push(self.parse_statement()?);  
            } else {
                let expr = self.parse_expression(0, None)?;
                let stmt = Statement::Expression(ExpressionStatement { span: expr.span(), expr });
                body.push(stmt);
            }

            if !self.at(TokenKind::Semicolon) {
                break;
            }

            self.consume(TokenKind::Semicolon)?;
        }

        self.consume(TokenKind::RightCurly)?;

        Ok((start.span.clone(), body))
    }
    
    pub fn parse_block(&mut self) -> ParseResult<Statement> {
        let (span, body) = self.parse_block_body()?;    
        Ok(Statement::Block(BlockStatement { span, statements: body }))
    }

    pub fn parse_let_statement(&mut self) -> ParseResult<Statement> {
        let start = self.consume_next(TokenKind::Let)?;
        let ident_token = self.consume_next(TokenKind::Identifier)?;
        let ident = ident_token.value.clone();
        let mut expr = Expression::Empty(EmptyExpression { span: ident_token.span, });
        let mut typ = ExplicitType::Empty;

        if self.at(TokenKind::Colon) {
            self.consume(TokenKind::Colon)?;
            typ = self.parse_defined_type()?;
        }

        if self.at(TokenKind::Assignment) {
            self.consume(TokenKind::Assignment)?;
            expr = self.parse_expression(0, None)?;
        }

        if !self.in_function && self.at(TokenKind::Semicolon){
            self.consume(TokenKind::Semicolon)?;
        }

        Ok(Statement::Let(LetStatement { span: start.span, name: ident, expr, explicit_type: typ }))
    }

    pub fn parse_if_statement(&mut self) -> ParseResult<Statement> {
        let start = self.consume_next(TokenKind::If)?; 
        let cond = self.parse_expression(0, None)?;
        let (if_span, if_block) = self.parse_block_body()?;

        let mut else_block: Option<Vec<Statement>> = None;
        let mut else_span: Option<Span> = None;

        if self.peek() == TokenKind::Else {
            self.consume(TokenKind::Else)?;
            let (span, block) = self.parse_block_body()?;
            else_block = Some(block);
            else_span = Some(span); 
        }

        let x = IfStatement::new(
            start.span,
            cond, 
            if_block, 
            if_span, 
            else_block, 
            else_span
        );

       let stmt = Statement::If(x);

        return Ok(stmt);
    }

    pub fn parse_while_loop(&mut self) -> ParseResult<Statement> {
        let start = self.consume_next(TokenKind::While)?;
        
        let expr = self.parse_expression(0, None)?;
        
        let (span, body) = self.parse_block_body()?;
        
        let stmt = Statement::While(WhileStatement { span: start.span, condition: expr, body: BlockStatement { span, statements: body }});
        Ok(stmt)
    }

    pub fn parse_defined_type(&mut self) -> ParseResult<ExplicitType> {
        match self.peek() {
            TokenKind::Identifier => {
                let name = self.consume_next(TokenKind::Identifier)?.value.clone();         
                Ok(ExplicitType::Name(name))
            }
            TokenKind::Mul => {
                self.consume_next(TokenKind::Mul)?;
                Ok(ExplicitType::Pointer(Box::new(self.parse_defined_type()?)))
            }
            o => todo!("Parse defined type for: {:?}", o)
        } 
    }

    pub fn parse_return(&mut self) -> ParseResult<Statement> {
        self.consume(TokenKind::Return)?; 
        let expr = self.parse_expression(0, None)?;

        Ok(Statement::Return(ReturnStatement { span: expr.span(), expr }))
    }

    pub fn parse_function(&mut self, external: bool) -> ParseResult<Statement> {
        self.in_function =true;

        let start = self.consume_next(TokenKind::Function)?; 
        let name = self.consume_next(TokenKind::Identifier)?.value.clone();
        let mut parameters = Vec::<NamedParameter>::new();
        self.consume(TokenKind::LeftParen)?;

        while !self.at(TokenKind::RightParen) {
            let name_token = self.consume_next(TokenKind::Identifier)?;
            let name = name_token.value.clone();
            self.consume(TokenKind::Colon)?;

            let defined_type = self.parse_defined_type()?; 
            let parameter = NamedParameter::new(name, defined_type, name_token.span);

            parameters.push(parameter);

            if !self.at(TokenKind::Comma) {
                break;
            } else {
                self.consume(TokenKind::Comma)?;
            }
        }

        let r_paren = self.consume_next(TokenKind::RightParen)?;

        let mut returning = ExplicitType::Empty;

        if self.peek() == TokenKind::Arrow {
            self.consume(TokenKind::Arrow)?;  
            returning = self.parse_defined_type()?;
        }

        let mut span = Span::default();
        let mut body: Vec<Statement> = vec![];

        if self.peek() == TokenKind::Semicolon {
            self.consume(TokenKind::Semicolon)?;   
        } else if self.peek() == TokenKind::LeftCurly {
            (span, body) = self.parse_block_body()?;
        } else {
            return Err(
                OceanError::new(
                    Level::Error,
                    Step::Parsing,
                    r_paren.span,
                    format!(
                        "Unexpected token: \x1b[1m{}\x1b[0m. Expected either token: \"\x1b[1m{}\x1b[0m\" or: \"\x1b[1m{}\x1b[0m\"", 
                        self.peek().to_string(), 
                        TokenKind::Semicolon.to_string(),
                        TokenKind::LeftCurly.to_string(),
                    ),
                )   
            );           
        }

        let function = Statement::Function(FunctionStatement {
            span: start.span,
            name,
            parameters,
            block: BlockStatement {
                span: span.clone(),
                statements: body.clone(),
            },
            return_type: returning,
            external,
        });
        self.in_function = false;

        Ok(function)
    }

    pub fn parse_import_statement(&mut self) -> ParseResult<Statement> {
        let start = self.consume_next(TokenKind::Import)?;

        let name = self.consume_next(TokenKind::StringLiteral)?.value.clone();
        self.consume(TokenKind::Semicolon)?;

        Ok(Statement::Import(ImportStatement {
            span: start.span,
            name,
        }))
    }

    pub fn parse_statement(&mut self) -> ParseResult<Statement> {
        match self.peek() {
            TokenKind::Let => self.parse_let_statement(),
            TokenKind::LeftCurly => self.parse_block(),
            TokenKind::Function => self.parse_function(false),
            TokenKind::If => self.parse_if_statement(),
            TokenKind::While => self.parse_while_loop(),
            TokenKind::Return => self.parse_return(),
            TokenKind::Extern => {
                self.consume(TokenKind::Extern)?;
                self.parse_function(true)
            }
            TokenKind::Literal => {
                let expr = self.parse_expression(0, None)?;
                let stmt = Statement::Expression(ExpressionStatement { span: expr.span(), expr });
                Ok(stmt)
            }
            TokenKind::Import => self.parse_import_statement(),
            TokenKind::Eof => {
                let token = self.consume_next(TokenKind::Eof)?;
                self.ended = true;
                Err(OceanError::new(Level::Ignore, Step::Parsing, token.span, "EofENDEof".into()))
            }
            token => todo!("Token: {:?} not implemented yet.", token)
        }
    }
}