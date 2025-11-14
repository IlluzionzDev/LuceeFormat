use crate::ast::Expression::Literal as ExpLiteral;
use crate::ast::{
    AccessModifier, ArrayExpression, BinaryExpression, BinaryOperator, BreakStatement,
    CaseStatement, ComponentDefinition, ContinueStatement, Expression, ExpressionStatement,
    ForControl, ForStatement, FunctionCall, FunctionDefinition, GroupExpression, IfStatement,
    IndexAccess, LambdaExpression, Literal, LiteralValue, LuceeFunction, MemberAccess,
    ObjectCreation, Parameter, ReturnStatement, Statement, StaticAccess, StringValue,
    StructExpression, SwitchStatement, TernaryExpression, TryCatchStatement, UnaryExpression,
    UnaryOperator, VariableAssignment, VariableDeclaration, WhileStatement, AST,
};
use crate::lexer::{Lexer, SourceSpan, Token, TokenType};
use miette::{miette, LabeledSpan, NamedSource, Report};
use std::rc::Rc;

/// Parse a source string into an AST
/// Lexs tokens one at a time as needed
pub struct Parser<'ast> {
    lexer: Lexer<'ast>,
    behind: Token<'ast>,
    current: Token<'ast>,
    ahead: Token<'ast>,

    named_source: NamedSource<String>,

    // DEBUG: Total time spent lexing while parsing, as micros
    pub lex_time: u128,
}

/**
* Parser generates a literal AST, as in translates text to syntax. Performs
* basic syntax checking as it parses, but does not validate syntax with context. For example
* in Lucee we can't define a function within a function. This Parser will parse that, but not validate
* that it is correct
*/
impl<'ast> Parser<'ast> {
    pub fn new(source: &'ast str, file_name: &'ast str) -> miette::Result<Parser<'ast>> {
        let mut lexer = Lexer::new(source, file_name);
        let current = lexer.scan_token()?;
        let ahead = lexer.scan_token()?;
        Ok(Parser {
            lexer,
            current,
            ahead,
            behind: Token {
                token_type: TokenType::EOF,
                line: 0,
                column: 0,
                end_column: 0,
                lexeme: "",
                span: SourceSpan { start: 0, end: 0 },
                comments: None,
                trailing_comments: None,
                lines_before: 0,
            },
            named_source: NamedSource::new(file_name, source.to_string()),
            lex_time: 0,
        })
    }

    fn check(&self, token_type: TokenType) -> bool {
        !self.is_at_end() && self.peek().token_type == token_type
    }

    fn check_next(&self, token_type: TokenType) -> bool {
        !self.is_at_end() && self.peek_next().token_type == token_type
    }

    fn advance(&mut self) -> miette::Result<&Token<'ast>> {
        self.behind = std::mem::replace(&mut self.current, self.ahead.clone());
        if !self.is_at_end() {
            let start = std::time::Instant::now();
            self.ahead = self.lexer.scan_token()?;
            self.lex_time += start.elapsed().as_micros();
        }

        // Assign trailing comments from ahead to behind
        if self.ahead.trailing_comments.is_some() {
            self.current.trailing_comments = self.ahead.trailing_comments.clone();
            self.ahead.trailing_comments = None;
        }

        Ok(&self.behind)
    }

    // Check next token is token_type, and if so advance.
    // If not match, safe exit
    fn advance_check(&mut self, token_type: TokenType) -> bool {
        // println!("Advance Check: {0:?}, {1:?}", self.peek(), token_type);
        if self.check(token_type) {
            self.advance();
            true
        } else {
            false
        }
    }

    // Expect token and consume, otherwise error
    fn consume(
        &mut self,
        token_type: TokenType,
        report: impl FnOnce(&Token) -> Report,
    ) -> miette::Result<&Token<'ast>> {
        if self.check(token_type) {
            return self.advance();
        }

        Err(report(self.peek()).with_source_code(self.named_source.clone()))
    }

    fn error(&self, report: Report) -> miette::Result<()> {
        Err(report.with_source_code(self.named_source.clone()))
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::EOF
    }

    fn peek(&self) -> &Token<'ast> {
        &self.current
    }

    fn peek_next(&self) -> &Token<'ast> {
        &self.ahead
    }

    pub fn parse(&mut self) -> Result<AST<'ast>, Vec<Report>> {
        let mut statements = vec![];
        let mut reports = vec![];

        while !self.is_at_end() {
            match self.statement() {
                Ok(stmt) => statements.push(stmt),
                Err(err) => {
                    reports.push(err);
                    self.synchronize();
                }
            }
        }

        if !reports.is_empty() {
            return Err(reports);
        }

        Ok(AST {
            source: Rc::new(String::from(self.lexer.source)),
            statements,
        })
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            // TODO: Properly synchronize
            // if self.behind.token_type == TokenType::Semicolon {
            //     return;
            // }
            //
            // match self.peek().token_type {
            //     TokenType::Function
            //     | TokenType::Var
            //     | TokenType::For
            //     | TokenType::If
            //     | TokenType::While
            //     | TokenType::Return => return,
            //     _ => {}
            // }

            self.advance();
        }
    }

    fn statement(&mut self) -> miette::Result<Statement<'ast>> {
        // Variable Declaration
        if self.check(TokenType::Var) {
            return self.variable_declaration();
        }

        if self.check(TokenType::Return) {
            let return_token = self
                .consume(TokenType::Return, |token| {
                    let labels = vec![LabeledSpan::at(token.span(), "Expected 'return' here")];

                    miette!(labels = labels, "Expected 'return' keyword")
                })?
                .clone();
            let expression = self.expression()?;
            let semicolon_token = if self.check(TokenType::Semicolon) {
                Some(self.advance()?.clone())
            } else {
                None
            };
            return Ok(Statement::ReturnStatement(Rc::new(ReturnStatement {
                return_token,
                value: Some(expression),
                semicolon_token,
            })));
        }

        if self.check(TokenType::Break) {
            let break_token = self
                .consume(TokenType::Break, |token| {
                    let labels = vec![LabeledSpan::at(token.span(), "Expected 'break' here")];

                    miette!(labels = labels, "Expected 'break' keyword")
                })?
                .clone();
            let semicolon_token = if self.check(TokenType::Semicolon) {
                Some(self.advance()?.clone())
            } else {
                None
            };
            return Ok(Statement::BreakStatement(Rc::new(BreakStatement {
                break_token,
                semicolon_token,
            })));
        }

        if self.check(TokenType::Continue) {
            let continue_token = self
                .consume(TokenType::Continue, |token| {
                    let labels = vec![LabeledSpan::at(token.span(), "Expected 'continue' here")];

                    miette!(labels = labels, "Expected 'continue' keyword")
                })?
                .clone();
            let semicolon_token = if self.check(TokenType::Semicolon) {
                Some(self.advance()?.clone())
            } else {
                None
            };
            return Ok(Statement::ContinueStatement(Rc::new(ContinueStatement {
                continue_token,
                semicolon_token,
            })));
        }

        // Function Definition
        if self.check(TokenType::Public)
            || self.check(TokenType::Private)
            || self.check(TokenType::Protected)
            || self.check(TokenType::Function)
            || (self.check(TokenType::Identifier)
                && !self.check_next(TokenType::LeftParen)
                && self.check_next(TokenType::Function))
        {
            return self.function_definition();
        }

        // Custom lucee function
        if self.check(TokenType::Identifier)
            && (self.check_next(TokenType::LeftBrace)
                || self.check_next(TokenType::Identifier)
                || self.check_next(TokenType::Semicolon))
        {
            return self.lucee_function();
        }

        // Component Definition
        if self.check(TokenType::Component) {
            return self.component_definition();
        }

        // Control structure
        if self.check(TokenType::If) {
            return self.if_statement();
        }

        if self.check(TokenType::For) {
            return self.for_statement();
        }

        if self.check(TokenType::While) || self.check(TokenType::Do) {
            return self.while_statement();
        }

        if self.check(TokenType::Switch) {
            return self.switch_statement();
        }

        if self.check(TokenType::Try) {
            return self.try_catch_statement();
        }

        // Expression Statement
        let expression = self.expression()?;

        match expression {
            Expression::None => {
                // If followed by semicolon, is usually just an extra semicolon
                if self.check(TokenType::Semicolon) {
                    return Ok(Statement::ExpressionStmt(Rc::new(ExpressionStatement {
                        expression,
                        semicolon_token: Some(self.advance()?.clone()),
                    })));
                }

                let labels = vec![LabeledSpan::at(self.current.span(), "Here")];
                self.error(miette!(labels = labels, "Invalid expression"))?;
            }
            _ => {}
        }

        let semicolon_token = if self.check(TokenType::Semicolon) {
            Some(self.advance()?.clone())
        } else {
            None
        };

        Ok(Statement::ExpressionStmt(Rc::new(ExpressionStatement {
            expression,
            semicolon_token,
        })))
    }

    fn variable_declaration(&mut self) -> miette::Result<Statement<'ast>> {
        let var_token = self
            .consume(TokenType::Var, |token| {
                let labels = vec![LabeledSpan::at(token.span(), "Expected 'var' here")];
                miette!(labels = labels, "Expected 'var' keyword")
            })?
            .clone();
        let name = self
            .consume(TokenType::Identifier, |token| {
                let labels = vec![LabeledSpan::at(token.span(), "Expected variable name here")];
                miette!(labels = labels, "Expected variable name")
            })?
            .clone();
        let equals_token = self
            .consume(TokenType::Equal, |token| {
                let labels = vec![LabeledSpan::at(token.span(), "Expected '=' here")];
                miette!(labels = labels, "Expected assignment operator '='")
            })?
            .clone();
        let value = self.expression()?;
        let semicolon_token = if self.check(TokenType::Semicolon) {
            Some(self.advance()?.clone())
        } else {
            None
        };
        return Ok(Statement::VariableDeclaration(Rc::new(
            VariableDeclaration {
                var_token,
                name,
                value,
                equals_token,
                semicolon_token,
            },
        )));
    }

    /// Consume a statement block. Takes param if braces are optional for the block, and if they are it means
    /// can consume a single statement without braces
    /// Return type is a tuple of the statements, and the optional opening and closing brace tokens
    fn consume_statement_block(
        &mut self,
        optional_braces: bool,
    ) -> miette::Result<(
        Vec<Statement<'ast>>,
        Option<Token<'ast>>,
        Option<Token<'ast>>,
    )> {
        // TODO: If consuming no statements but with {}, return {} tokens
        let mut has_braces = false;
        let mut left_brace = None;
        let mut right_brace = None;

        // If { exists, consume multiple statements, otherwise only consume one
        if optional_braces {
            has_braces = self.check(TokenType::LeftBrace);
            if has_braces {
                left_brace = Some(self.advance()?.clone());
            }
        } else {
            has_braces = true;
            left_brace = Some(
                self.consume(TokenType::LeftBrace, |token| {
                    let labels = vec![LabeledSpan::at(token.span(), "Expected '{' here")];
                    miette!(labels = labels, "Expected '{{'")
                })?
                .clone(),
            );
        }

        let mut body = Vec::new();
        while !self.check(TokenType::RightBrace) {
            body.push(self.statement()?);
            // Only consume one statement if no braces
            if self.check(TokenType::RightBrace) || !has_braces {
                break;
            }
        }

        if optional_braces {
            // Only consume right brace if had left brace
            if self.check(TokenType::RightBrace) && has_braces {
                right_brace = Some(self.advance()?.clone());
            }
        } else {
            right_brace = Some(
                self.consume(TokenType::RightBrace, |token| {
                    let labels = vec![LabeledSpan::at(token.span(), "Expected '}' here")];
                    miette!(labels = labels, "Expected '}}''")
                })?
                .clone(),
            );
        }

        Ok((body, left_brace, right_brace))
    }

    fn function_definition(&mut self) -> miette::Result<Statement<'ast>> {
        let access_modifier = match self.peek().token_type {
            TokenType::Public => Some(AccessModifier::Public),
            TokenType::Private => Some(AccessModifier::Private),
            TokenType::Protected => Some(AccessModifier::Protected),
            _ => None,
        };
        let mut access_modifier_token = None;
        if access_modifier.is_some() {
            access_modifier_token = Some(self.advance()?.clone());
        }

        let mut return_type = None;
        if self.check(TokenType::Identifier) {
            return_type = Some(self.advance()?.clone());
        }

        let function_token = self
            .consume(TokenType::Function, |token| {
                let labels = vec![LabeledSpan::at(token.span(), "Expected 'function' here")];
                miette!(labels = labels, "Expected 'function' keyword")
            })?
            .clone();

        let name = self
            .consume(TokenType::Identifier, |token| {
                let labels = vec![LabeledSpan::at(token.span(), "Expected function name here")];
                miette!(
                    labels = labels,
                    "Expected function name, found '{}'",
                    token.lexeme
                )
            })?
            .clone();

        let left_paren = self
            .consume(TokenType::LeftParen, |token| {
                let labels = vec![LabeledSpan::at(token.span(), "Expected '(' here")];
                miette!(labels = labels, "Expected '('")
            })?
            .clone();
        let mut parameters = Vec::new();
        if !self.check(TokenType::RightParen) {
            parameters = self.parameters()?;
        }
        let right_paren = self
            .consume(TokenType::RightParen, |token| {
                let labels = vec![LabeledSpan::at(token.span(), "Expected ')' here")];
                miette!(labels = labels, "Expected ')'")
            })?
            .clone();

        // Try consume attributes
        let attributes = self.attribute_definitions()?;

        let (body, left_brace, right_brace) = self.consume_statement_block(false)?;

        let function_definition: FunctionDefinition = FunctionDefinition {
            access_modifier,
            access_modifier_token,
            return_type,
            function_token,
            name,
            left_paren,
            parameters,
            right_paren,
            attributes,
            body,
            left_brace: left_brace.unwrap(),
            right_brace: right_brace.unwrap(),
        };

        Ok(Statement::FunctionDefinition(Rc::new(function_definition)))
    }

    // Consume parameter list of <required>? <type> <identifier> ("=" <expression>)?
    fn parameters(&mut self) -> miette::Result<Vec<(Parameter<'ast>, Option<Token<'ast>>)>> {
        let mut parameters = Vec::new();

        let first_param = self.parameter()?;
        let comma_token = if self.check(TokenType::Comma) {
            Some(self.advance()?.clone())
        } else {
            None
        };
        let has_comma = comma_token.is_some();
        parameters.push((first_param, comma_token));

        while has_comma && !self.check(TokenType::RightParen) {
            let param = self.parameter()?;
            let comma_token = if self.check(TokenType::Comma) {
                Some(self.advance()?.clone())
            } else {
                None
            };
            let has_more_comma = comma_token.is_some();
            parameters.push((param, comma_token));

            if !has_more_comma {
                break;
            }
        }

        Ok(parameters)
    }

    fn parameter(&mut self) -> miette::Result<Parameter<'ast>> {
        let mut required = None;
        if self.check(TokenType::Required) {
            required = Some(self.advance()?.clone());
        }

        // Defining return type
        let mut param_type = None;
        if self.check(TokenType::Identifier) && self.check_next(TokenType::Identifier) {
            param_type = Some(
                self.consume(TokenType::Identifier, |token| {
                    let labels = vec![LabeledSpan::at(
                        token.span(),
                        "Expected parameter type here",
                    )];
                    miette!(labels = labels, "Expected parameter type")
                })?
                .clone(),
            );
        }

        let name = self
            .consume(TokenType::Identifier, |token| {
                let labels = vec![LabeledSpan::at(
                    token.span(),
                    "Expected parameter name here",
                )];
                miette!(labels = labels, "Expected parameter name")
            })?
            .clone();

        let mut default_value = None;
        let mut default_value_token = None;
        if self.check(TokenType::Equal) {
            default_value_token = Some(self.advance()?.clone());
            default_value = Some(self.expression()?);
            if default_value.is_none() {
                self.error(miette!("Expected default value"))?;
            }
        }

        Ok(Parameter {
            required,
            param_type,
            name,
            default_value,
            equals_token: default_value_token,
        })
    }

    /**
     * Lucee function refers to statements that look like the following
     *
     * lock name="myLock" type="exclusive" timeout="10" {
     *
     * }
     *
     * "Calling" a function like a statement with an attribute list
     */
    fn lucee_function(&mut self) -> miette::Result<Statement<'ast>> {
        let name = self
            .consume(TokenType::Identifier, |token| {
                let labels = vec![LabeledSpan::at(token.span(), "Expected identifier here")];
                miette!(labels = labels, "Expected identifier")
            })?
            .clone();

        let attributes = self.attribute_definitions()?;

        let mut body = None;
        let mut left_brace = None;
        let mut right_brace = None;
        if !self.check(TokenType::Semicolon) {
            let statement_block = self.consume_statement_block(true)?;
            body = Some(statement_block.0);
            left_brace = statement_block.1;
            right_brace = statement_block.2;
        }

        let semicolon_token = if self.check(TokenType::Semicolon) {
            Some(self.advance()?.clone())
        } else {
            None
        };

        Ok(Statement::LuceeFunction(Rc::new(LuceeFunction {
            name,
            attributes,
            body,
            left_brace,
            right_brace,
            semicolon_token,
        })))
    }

    fn component_definition(&mut self) -> miette::Result<Statement<'ast>> {
        let component_token = self
            .consume(TokenType::Component, |token| {
                let labels = vec![LabeledSpan::at(
                    token.span(),
                    "Expected 'component' keyword here",
                )];
                miette!(labels = labels, "Expected 'component' keyword")
            })?
            .clone();

        let attributes = self.attribute_definitions()?;

        let (body, left_brace, right_brace) = self.consume_statement_block(false)?;

        Ok(Statement::ComponentDefinition(Rc::new(
            ComponentDefinition {
                component_token,
                attributes,
                body,
                left_brace: left_brace.unwrap(),
                right_brace: right_brace.unwrap(),
            },
        )))
    }

    fn attribute_definitions(&mut self) -> miette::Result<Vec<(Token<'ast>, Expression<'ast>)>> {
        let mut attributes = Vec::new();

        while self.check(TokenType::Identifier) {
            let name = self.advance()?.clone();
            self.consume(TokenType::Equal, |token| {
                let labels = vec![LabeledSpan::at(token.span(), "Expected '=' here")];
                miette!(labels = labels, "Expected assignment operator '='")
            })?;
            let value = self.expression()?;
            attributes.push((name, value));
        }

        Ok(attributes)
    }

    fn if_statement(&mut self) -> miette::Result<Statement<'ast>> {
        let if_token = self
            .consume(TokenType::If, |token| {
                let labels = vec![LabeledSpan::at(token.span(), "Expected 'if' here")];
                miette!(labels = labels, "Expected 'if' keyword")
            })?
            .clone();
        let left_paren = self
            .consume(TokenType::LeftParen, |token| {
                let labels = vec![LabeledSpan::at(token.span(), "Expected '(' here")];
                miette!(labels = labels, "Expected '(' keyword")
            })?
            .clone();

        let condition = self.expression()?;

        let right_paren = self
            .consume(TokenType::RightParen, |token| {
                let labels = vec![LabeledSpan::at(token.span(), "Expected ')' here")];
                miette!(labels = labels, "Expected ')' keyword")
            })?
            .clone();

        let (body, left_brace, right_brace) = self.consume_statement_block(true)?;

        let mut else_body = None;
        let mut else_token = None;
        let mut else_left_brace = None;
        let mut else_right_brace = None;
        if self.check(TokenType::Else) {
            else_token = Some(self.advance()?.clone());
            if self.check(TokenType::If) {
                // Else body is another IF statement
                else_body = Some(vec![self.if_statement()?]);
            } else {
                // TODO: Probably a cleaner way to do this
                // Else body is consumed statements
                let (_else_body, _else_left_brace, _else_right_brace) =
                    self.consume_statement_block(true)?;
                else_body = Some(_else_body);
                else_left_brace = _else_left_brace;
                else_right_brace = _else_right_brace;
            }
        }

        Ok(Statement::IfStatement(Rc::new(IfStatement {
            if_token,
            left_paren,
            right_paren,
            condition,
            body,
            left_brace,
            right_brace,
            else_body,
            else_left_brace,
            else_right_brace,
            else_token,
        })))
    }

    fn for_statement(&mut self) -> miette::Result<Statement<'ast>> {
        let for_token = self
            .consume(TokenType::For, |token| {
                let labels = vec![LabeledSpan::at(token.span(), "Expected 'for' here")];
                miette!(labels = labels, "Expected 'for' keyword")
            })?
            .clone();

        let left_paren = self
            .consume(TokenType::LeftParen, |token| {
                let labels = vec![LabeledSpan::at(token.span(), "Expected '(' here")];
                miette!(labels = labels, "Expected '(' keyword")
            })?
            .clone();

        // Check optional "var" keyword
        let mut var_token = None;
        if self.check(TokenType::Var) {
            var_token = Some(self.advance()?.clone());
        }

        // Consume identifier, if next keyword is in, is a for in loop
        let name = self
            .consume(TokenType::Identifier, |token| {
                let labels = vec![LabeledSpan::at(token.span(), "Expected identifier here")];
                miette!(labels = labels, "Expected identifier")
            })?
            .clone();

        if self.check(TokenType::In) {
            let in_token = self
                .consume(TokenType::In, |token| {
                    let labels = vec![LabeledSpan::at(token.span(), "Expected 'in' here")];
                    miette!(labels = labels, "Expected 'in' keyword")
                })?
                .clone();
            let expression = self.expression()?;
            let right_paren = self
                .consume(TokenType::RightParen, |token| {
                    let labels = vec![LabeledSpan::at(token.span(), "Expected ')' here")];
                    miette!(labels = labels, "Expected ')' keyword")
                })?
                .clone();
            let (body, left_brace, right_brace) = self.consume_statement_block(true)?;
            Ok(Statement::ForStatement(Rc::new(ForStatement {
                for_token,
                left_paren,
                right_paren,
                control: ForControl::LoopOver {
                    var_token,
                    variable: name,
                    in_token,
                    array: expression,
                },
                body,
                left_brace,
                right_brace,
            })))
        } else {
            let equals_token = self
                .consume(TokenType::Equal, |token| {
                    let labels = vec![LabeledSpan::at(token.span(), "Expected '=' here")];
                    miette!(labels = labels, "Expected assignment operator")
                })?
                .clone();
            let init = self.expression()?;
            self.consume(TokenType::Semicolon, |token| {
                let labels = vec![LabeledSpan::at(token.span(), "Expected ';' here")];
                miette!(labels = labels, "Expected ';'")
            })?;
            let condition = self.expression()?;
            self.consume(TokenType::Semicolon, |token| {
                let labels = vec![LabeledSpan::at(token.span(), "Expected ';' here")];
                miette!(labels = labels, "Expected ';'")
            })?;
            let increment = self.expression()?;
            let right_paren = self
                .consume(TokenType::RightParen, |token| {
                    let labels = vec![LabeledSpan::at(token.span(), "Expected ')' here")];
                    miette!(labels = labels, "Expected ')' keyword")
                })?
                .clone();
            let (body, left_brace, right_brace) = self.consume_statement_block(true)?;
            Ok(Statement::ForStatement(Rc::new(ForStatement {
                for_token,
                left_paren,
                right_paren,
                control: ForControl::Increment {
                    var_token,
                    variable: name,
                    init,
                    equals_token,
                    condition,
                    increment,
                },
                body,
                left_brace,
                right_brace,
            })))
        }
    }

    fn while_statement(&mut self) -> miette::Result<Statement<'ast>> {
        if self.check(TokenType::While) {
            let while_token = self.advance()?.clone();
            let left_paren = self
                .consume(TokenType::LeftParen, |token| {
                    let labels = vec![LabeledSpan::at(token.span(), "Expected '(' here")];
                    miette!(labels = labels, "Expected '(' keyword")
                })?
                .clone();

            let condition = self.expression()?;

            let right_paren = self
                .consume(TokenType::RightParen, |token| {
                    let labels = vec![LabeledSpan::at(token.span(), "Expected ')' here")];
                    miette!(labels = labels, "Expected ')' keyword")
                })?
                .clone();

            // Check if this is a bodyless while statement (just a semicolon)
            if self.check(TokenType::Semicolon) {
                let semicolon_token = Some(self.advance()?.clone());
                return Ok(Statement::WhileStatement(Rc::new(WhileStatement {
                    do_while: false,
                    do_token: None,
                    while_token,
                    condition,
                    left_paren,
                    right_paren,
                    body: None,
                    left_brace: None,
                    right_brace: None,
                    semicolon_token,
                })));
            }

            // Otherwise, consume a statement block (braces optional for single statement)
            let (body, left_brace, right_brace) = self.consume_statement_block(true)?;

            return Ok(Statement::WhileStatement(Rc::new(WhileStatement {
                do_while: false,
                do_token: None,
                while_token,
                condition,
                left_paren,
                right_paren,
                body: Some(body),
                left_brace,
                right_brace,
                semicolon_token: None,
            })));
        } else if self.check(TokenType::Do) {
            let do_token = Some(self.advance()?.clone());
            let (body, left_brace, right_brace) = self.consume_statement_block(false)?;

            let while_token = self
                .consume(TokenType::While, |token| {
                    let labels = vec![LabeledSpan::at(token.span(), "Expected 'while' here")];
                    miette!(labels = labels, "Expected 'while' keyword")
                })?
                .clone();

            let left_paren = self
                .consume(TokenType::LeftParen, |token| {
                    let labels = vec![LabeledSpan::at(token.span(), "Expected '(' here")];
                    miette!(labels = labels, "Expected '(' keyword")
                })?
                .clone();

            let condition = self.expression()?;

            let right_paren = self
                .consume(TokenType::RightParen, |token| {
                    let labels = vec![LabeledSpan::at(token.span(), "Expected ')' here")];
                    miette!(labels = labels, "Expected ')' keyword")
                })?
                .clone();

            // Consume optional semicolon after do-while
            let semicolon_token = if self.check(TokenType::Semicolon) {
                Some(self.advance()?.clone())
            } else {
                None
            };

            return Ok(Statement::WhileStatement(Rc::new(WhileStatement {
                do_while: true,
                do_token,
                while_token,
                condition,
                left_paren,
                right_paren,
                body: Some(body),
                left_brace,
                right_brace,
                semicolon_token,
            })));
        }

        Err(miette!("Expected 'while' or 'do' keyword"))
    }

    fn switch_statement(&mut self) -> miette::Result<Statement<'ast>> {
        let switch_token = self
            .consume(TokenType::Switch, |token| {
                let labels = vec![LabeledSpan::at(token.span(), "Expected 'switch' here")];
                miette!(labels = labels, "Expected 'switch' keyword")
            })?
            .clone();
        let left_paren = self
            .consume(TokenType::LeftParen, |token| {
                let labels = vec![LabeledSpan::at(token.span(), "Expected '(' here")];
                miette!(labels = labels, "Expected '(' keyword")
            })?
            .clone();
        let expression = self.expression()?;

        let right_paren = self
            .consume(TokenType::RightParen, |token| {
                let labels = vec![LabeledSpan::at(token.span(), "Expected ')' here")];
                miette!(labels = labels, "Expected ')' keyword")
            })?
            .clone();

        let left_brace = self
            .consume(TokenType::LeftBrace, |token| {
                let labels = vec![LabeledSpan::at(token.span(), "Expected '{' here")];
                miette!(labels = labels, "Expected '{{'")
            })?
            .clone();

        let mut cases = Vec::new();

        // Handle case statements
        while !self.check(TokenType::RightBrace) {
            let mut condition = Vec::new();

            // Consume multiple case statements
            while self.check(TokenType::Case) {
                let case_token = self.advance()?.clone();
                let cond_expr = self.expression()?;
                let colon = self
                    .consume(TokenType::Colon, |token| {
                        let labels = vec![LabeledSpan::at(token.span(), "Expected ':' here")];
                        miette!(labels = labels, "Expected ':' keyword")
                    })?
                    .clone();
                condition.push((case_token, cond_expr, colon));
            }

            let mut is_default = false;

            if self.check(TokenType::Default) {
                if !condition.is_empty() {
                    let labels = vec![LabeledSpan::at(
                        self.current.span(),
                        "Got default when case already declared",
                    )];
                    self.error(miette!(
                        labels = labels,
                        "Cannot mix 'case' and 'default' in a switch statement"
                    ))?;
                }
                let default_token = self.advance()?.clone();

                is_default = true;
                let colon = self
                    .consume(TokenType::Colon, |token| {
                        let labels = vec![LabeledSpan::at(token.span(), "Expected ':' here")];
                        miette!(labels = labels, "Expected ':' keyword")
                    })?
                    .clone();
                condition.push((default_token, Expression::None, colon));
            }

            if condition.is_empty() && !is_default {
                let labels = vec![LabeledSpan::at(self.current.span(), "Here")];
                self.error(miette!(
                    labels = labels,
                    "Expected 'case' or 'default' keyword"
                ))?;
            }

            // Case body: We consume until break or return expression, since don't need {} to declare
            // statement body
            let mut body = Vec::new();
            while !self.check(TokenType::Break)
                && !self.check(TokenType::Return)
                && !self.check(TokenType::RightBrace)
                && !self.check(TokenType::Case)
            {
                body.push(self.statement()?);
            }

            // Parse last break / return statement, optional as can be empty case
            if self.check(TokenType::Break) || self.check(TokenType::Return) {
                body.push(self.statement()?);
            }

            cases.push(CaseStatement {
                is_default,
                condition,
                body,
            });
        }

        let right_brace = self
            .consume(TokenType::RightBrace, |token| {
                let labels = vec![LabeledSpan::at(token.span(), "Expected '}' here")];
                miette!(labels = labels, "Expected '}}'")
            })?
            .clone();

        Ok(Statement::SwitchStatement(Rc::new(SwitchStatement {
            switch_token,
            left_paren,
            right_paren,
            expression,
            left_brace,
            right_brace,
            cases,
        })))
    }

    fn try_catch_statement(&mut self) -> miette::Result<Statement<'ast>> {
        let try_token = self
            .consume(TokenType::Try, |token| {
                let labels = vec![LabeledSpan::at(token.span(), "Expected 'try' here")];
                miette!(labels = labels, "Expected 'try' keyword")
            })?
            .clone();
        let (try_body, try_left_brace, try_right_brace) = self.consume_statement_block(false)?;
        let catch_token = self
            .consume(TokenType::Catch, |token| {
                let labels = vec![LabeledSpan::at(token.span(), "Expected 'catch' here")];
                miette!(labels = labels, "Expected 'catch' keyword")
            })?
            .clone();
        let left_paren = self
            .consume(TokenType::LeftParen, |token| {
                let labels = vec![LabeledSpan::at(token.span(), "Expected '(' here")];
                miette!(labels = labels, "Expected '(' keyword")
            })?
            .clone();
        let mut catch_var_token = None;
        if self.check(TokenType::Var) {
            catch_var_token = Some(self.advance()?.clone());
        }
        let mut catch_var_type = None;
        if self.check(TokenType::Identifier)
            && (self.check_next(TokenType::Identifier) || self.check_next(TokenType::Dot))
        {
            catch_var_type = Some(self.expression()?);
        }
        let catch_var = self
            .consume(TokenType::Identifier, |token| {
                let labels = vec![LabeledSpan::at(token.span(), "Expected identifier here")];
                miette!(labels = labels, "Expected identifier")
            })?
            .clone();
        let right_paren = self
            .consume(TokenType::RightParen, |token| {
                let labels = vec![LabeledSpan::at(token.span(), "Expected ')' here")];
                miette!(labels = labels, "Expected ')' keyword")
            })?
            .clone();
        let (catch_body, catch_left_brace, catch_right_brace) =
            self.consume_statement_block(false)?;

        let finally_token = if self.check(TokenType::Finally) {
            Some(self.advance()?.clone())
        } else {
            None
        };
        let (finally_body, finally_left_brace, finally_right_brace) = if finally_token.is_some() {
            self.consume_statement_block(false)?
        } else {
            (vec![], None, None)
        };

        Ok(Statement::TryCatchStatement(Rc::new(TryCatchStatement {
            try_token,
            try_body,
            try_left_brace: try_left_brace.unwrap(),
            try_right_brace: try_right_brace.unwrap(),
            catch_token,
            left_paren,
            right_paren,
            catch_var_token,
            catch_var,
            catch_var_type,
            catch_body,
            catch_left_brace: catch_left_brace.unwrap(),
            catch_right_brace: catch_right_brace.unwrap(),
            finally_token,
            finally_body: Some(finally_body),
            finally_left_brace,
            finally_right_brace,
        })))
    }

    fn expression(&mut self) -> miette::Result<Expression<'ast>> {
        let expression = self.ternary()?;

        // Attempt post expression logic
        // Separate from post expression in statement, as these can appear freely after expressions not
        // only in statement lines
        match &expression {
            // Sometimes index access is accessing a function definition, and wants to be called straight after access
            Expression::IndexAccess(_) => {
                if self.check(TokenType::LeftParen) {
                    return self.function_call(expression);
                }
            }
            _ => {}
        }

        // Shorthand increments, ++ or --
        // For our AST we will store as normally binary expression of x += 1. Later optimized when formatting/parsing
        if self.check(TokenType::PlusPlus) || self.check(TokenType::MinusMinus) {
            let op = self.advance()?.clone();
            return Ok(Expression::BinaryExpression(Rc::new(BinaryExpression {
                left: expression,
                op: op.clone(),
                right: Expression::Literal(Rc::new(Literal {
                    value: LiteralValue::Number(1.0),
                    token: op.clone(),
                })),
            })));
        }

        // If assigning expression to something else
        if self.check(TokenType::Equal) {
            let equals_token = self
                .consume(TokenType::Equal, |token| {
                    let labels = vec![LabeledSpan::at(token.span(), "Expected '=' here")];

                    miette!(labels = labels, "Expected '=' keyword")
                })?
                .clone();
            let value = self.expression()?;
            let semicolon_token = if self.check(TokenType::Semicolon) {
                Some(self.advance()?.clone())
            } else {
                None
            };
            return Ok(Expression::VariableAssignment(Rc::new(
                VariableAssignment {
                    equals_token,
                    name: expression,
                    value,
                    semicolon_token,
                },
            )));
        }

        Ok(expression)
    }

    fn ternary(&mut self) -> miette::Result<Expression<'ast>> {
        let mut expression = self.equality()?;

        if self.check(TokenType::Question) {
            let question_token = self.advance()?.clone();
            let true_expr = self.expression()?;
            let colon_token = self
                .consume(TokenType::Colon, |token| {
                    let labels = vec![LabeledSpan::at(token.span(), "Expected ':' here")];
                    miette!(labels = labels, "Expected ':' keyword")
                })?
                .clone();
            let false_expr = self.expression()?;
            expression = Expression::TernaryExpression(Rc::new(TernaryExpression {
                condition: expression,
                question_token,
                true_expr,
                colon_token,
                false_expr,
            }));
        }

        Ok(expression)
    }

    fn equality(&mut self) -> miette::Result<Expression<'ast>> {
        let mut expression = self.comparison()?;

        while self.check(TokenType::EqualEqual)
            || self.check(TokenType::BangEqual)
            || self.check(TokenType::Eq)
            || self.check(TokenType::Neq)
        {
            let op = self.advance()?.clone();
            let right = self.comparison()?;
            expression = Expression::BinaryExpression(Rc::new(BinaryExpression {
                left: expression,
                op,
                right,
            }));
        }

        Ok(expression)
    }

    fn comparison(&mut self) -> miette::Result<Expression<'ast>> {
        let mut expression = self.term()?;

        while self.check(TokenType::Less)
            || self.check(TokenType::Greater)
            || self.check(TokenType::LessEqual)
            || self.check(TokenType::GreaterEqual)
            || self.check(TokenType::Lt)
            || self.check(TokenType::Gt)
            || self.check(TokenType::AmpersandAmpersand)
            || self.check(TokenType::PipePipe)
            || self.check(TokenType::And)
            || self.check(TokenType::Or)
            || self.check(TokenType::Contains)
            || self.check(TokenType::Xor)
        {
            let op = self.advance()?.clone();
            let right = self.term()?;
            expression = Expression::BinaryExpression(Rc::new(BinaryExpression {
                left: expression,
                op,
                right,
            }));
        }

        Ok(expression)
    }

    fn term(&mut self) -> miette::Result<Expression<'ast>> {
        let mut expression = self.factor()?;

        while self.check(TokenType::Plus)
            || self.check(TokenType::Minus)
            || self.check(TokenType::Ampersand)
            || self.check(TokenType::PlusEqual)
            || self.check(TokenType::MinusEqual)
            || self.check(TokenType::AmpersandEqual)
        {
            let op = self.advance()?.clone();
            let right = self.factor()?;
            expression = Expression::BinaryExpression(Rc::new(BinaryExpression {
                left: expression,
                op,
                right,
            }));
        }

        Ok(expression)
    }

    fn factor(&mut self) -> miette::Result<Expression<'ast>> {
        let mut expression = self.unary()?;

        while self.check(TokenType::Star)
            || self.check(TokenType::Slash)
            || self.check(TokenType::StarEqual)
            || self.check(TokenType::SlashEqual)
        {
            let op = self.advance()?.clone();
            let right = self.unary()?;
            expression = Expression::BinaryExpression(Rc::new(BinaryExpression {
                left: expression,
                op,
                right,
            }));
        }

        Ok(expression)
    }

    fn unary(&mut self) -> miette::Result<Expression<'ast>> {
        // Unary operator - or !
        if self.check(TokenType::Minus) || self.check(TokenType::Bang) {
            let op = self.advance()?.clone();
            let right = self.dot_access()?;
            return Ok(Expression::UnaryExpression(Rc::new(UnaryExpression {
                op,
                expr: right,
            })));
        }

        self.dot_access()
    }

    fn dot_access(&mut self) -> miette::Result<Expression<'ast>> {
        let mut expression = self.index_access()?;

        while self.check(TokenType::Dot) {
            let dot_token = self.advance()?.clone();
            let property = self.index_access()?;
            expression = Expression::MemberAccess(Rc::new(MemberAccess {
                object: expression,
                dot_token,
                property,
            }));
        }

        Ok(expression)
    }

    fn index_access(&mut self) -> miette::Result<Expression<'ast>> {
        let mut expression = self.primary()?;

        while self.check(TokenType::LeftBracket) {
            let left_bracket = self.advance()?.clone();
            let index = self.expression()?;
            let right_bracket = self
                .consume(TokenType::RightBracket, |token| {
                    let labels = vec![LabeledSpan::at(token.span(), "Expected ']' here")];
                    miette!(labels = labels, "Expected ']' keyword")
                })?
                .clone();
            expression = Expression::IndexAccess(Rc::new(IndexAccess {
                object: expression,
                index,
                left_bracket,
                right_bracket,
            }));
        }

        Ok(expression)
    }

    fn primary(&mut self) -> miette::Result<Expression<'ast>> {
        // Literals
        if self.check(TokenType::String) {
            let token = self.advance()?.clone();
            let value: String = String::from(token.lexeme);

            // Detect if is double quote based on first char of value
            let is_double_quote = token.lexeme.starts_with('"') && token.lexeme.ends_with('"');
            // Take raw value without quotes
            let raw_value = &value[1..value.len() - 1];

            return Ok(ExpLiteral(Rc::new(Literal {
                token,
                value: LiteralValue::String(StringValue {
                    value: String::from(raw_value),
                    is_double_quote,
                }),
            })));
        }

        if self.check(TokenType::Number) {
            let token = self.advance()?.clone();
            let value: f64 = token.lexeme.parse().unwrap();
            return Ok(ExpLiteral(Rc::new(Literal {
                token,
                value: LiteralValue::Number(value),
            })));
        }

        if self.check(TokenType::True) {
            return Ok(ExpLiteral(Rc::new(Literal {
                token: self.advance()?.clone(),
                value: LiteralValue::Boolean(true),
            })));
        }

        if self.check(TokenType::False) {
            return Ok(ExpLiteral(Rc::new(Literal {
                token: self.advance()?.clone(),
                value: LiteralValue::Boolean(false),
            })));
        }

        if self.check(TokenType::Null) {
            return Ok(ExpLiteral(Rc::new(Literal {
                token: self.advance()?.clone(),
                value: LiteralValue::Null,
            })));
        }

        // Identifier for function call
        // Edge-case: contains is both a keyword and a in-build function
        if self.check(TokenType::Identifier) || self.check(TokenType::Contains) {
            // If followed by ::, it's a static access
            if self.check_next(TokenType::ColonColon) {
                let class_name = self.advance()?.clone();
                let colon_colon_token = self.advance()?.clone();

                // Expect function call after ::
                if self.check(TokenType::Identifier) && self.check_next(TokenType::LeftParen) {
                    let function_name = self.advance()?.clone();
                    let name = Expression::Identifier(Rc::new(function_name));
                    let function_call = self.function_call(name)?;

                    if let Expression::FunctionCall(fc) = function_call {
                        return Ok(Expression::StaticAccess(Rc::new(StaticAccess {
                            class_name,
                            colon_colon_token,
                            function_call: (*fc).clone(),
                        })));
                    }
                }

                let labels = vec![LabeledSpan::at(
                    colon_colon_token.span(),
                    "Expected function call here",
                )];
                self.error(miette!(
                    labels = labels,
                    "Expected function call after '::'"
                ))?;
            }

            // If followed by (, it's a function call
            if self.check_next(TokenType::LeftParen) {
                let function = self.advance()?.clone();
                let name = Expression::Identifier(Rc::new(function));

                return self.function_call(name);
            }

            // If followed by =>, single argument lambda
            if self.check_next(TokenType::Lambda) {
                return self.lambda_expression(None);
            }

            return Ok(Expression::Identifier(Rc::new(self.advance()?.clone())));
        }

        // Object creation
        if self.check(TokenType::New) {
            let new_token = self.advance()?.clone();
            let expression = self.expression()?;
            match expression {
                Expression::ObjectCreation(_) => {
                    self.error(miette!(
                        "Invalid object creation; 'new' appeared more than once"
                    ))?;
                }
                _ => {}
            }
            return Ok(Expression::ObjectCreation(Rc::new(ObjectCreation {
                new_token,
                expr: expression,
            })));
        }

        // Function lambda: function(args) => {}
        if self.check(TokenType::Function) && self.check_next(TokenType::LeftParen) {
            return self.lambda_expression(None);
        }

        // Array literal
        if self.check(TokenType::LeftBracket) {
            let left_bracket = self.advance()?.clone();

            // Is actually an empty struct expression in CF, "[:]" means empty struct
            if self.check(TokenType::Colon) {
                let colon_token = self.advance()?.clone();
                let right_bracket = self
                    .consume(TokenType::RightBracket, |token| {
                        let labels = vec![
                            LabeledSpan::at(left_bracket.span(), "For '[' here"),
                            LabeledSpan::at(token.span(), "Expected closing ']' here"),
                        ];
                        miette!(
                            labels = labels,
                            help = "Empty struct expressions are defined like '[:]'",
                            "Expected closing bracket ']'"
                        )
                    })?
                    .clone();
                return Ok(Expression::StructExpression(Rc::new(StructExpression {
                    is_empty: true,
                    left_brace: left_bracket,
                    right_brace: right_bracket,
                    elements: Vec::new(),
                })));
            }

            let mut elements = Vec::new();
            while !self.check(TokenType::RightBracket) {
                let element_expr = self.expression()?;
                let comma_token = if self.check(TokenType::Comma) {
                    Some(self.advance()?.clone())
                } else {
                    None
                };
                let has_comma = comma_token.is_some();
                elements.push((element_expr, comma_token));

                if self.check(TokenType::RightBracket) {
                    break;
                }

                // If we didn't consume a comma above, we expect one now
                if !has_comma {
                    self.consume(TokenType::Comma, |token| {
                        let labels = vec![LabeledSpan::at(token.span(), "Expected ',' here")];
                        miette!(labels = labels, "Expected ',' keyword")
                    })?;
                }
            }
            let right_bracket = self
                .consume(TokenType::RightBracket, |token| {
                    let labels = vec![LabeledSpan::at(token.span(), "Expected ']' here")];
                    miette!(labels = labels, "Expected ']' keyword")
                })?
                .clone();
            return Ok(Expression::ArrayExpression(Rc::new(ArrayExpression {
                left_bracket,
                right_bracket,
                elements,
            })));
        }

        // Struct literal
        if self.check(TokenType::LeftBrace) {
            let left_brace = self.advance()?.clone();
            let mut elements = Vec::new();
            while !self.check(TokenType::RightBrace) {
                // Key is identifier or String
                let mut key = None;
                if self.check(TokenType::Identifier)
                    || self.check(TokenType::String)
                    || self.check(TokenType::Number)
                {
                    key = Some(self.advance()?.clone());
                } else {
                    let labels = vec![LabeledSpan::at(self.current.span(), "Here")];
                    self.error(miette!(labels = labels, "Expected struct key"))?;
                }
                if !self.advance_check(TokenType::Colon) && !self.advance_check(TokenType::Equal) {
                    if let Some(k) = &key {
                        let labels = vec![LabeledSpan::at(
                            k.span(),
                            "Expected ':' or '=' after struct key",
                        )];
                        self.error(miette!(
                            labels = labels,
                            "Struct keys can be assigned with ':' or '='"
                        ))?;
                    }
                }
                let value = self.expression()?;
                let comma_token = if self.check(TokenType::Comma) {
                    Some(self.advance()?.clone())
                } else {
                    None
                };
                let has_comma = comma_token.is_some();
                elements.push((key.unwrap(), value, comma_token));

                if self.check(TokenType::RightBrace) {
                    break;
                }

                // If we didn't consume a comma above, we expect one now
                if !has_comma {
                    self.consume(TokenType::Comma, |token| {
                        let labels = vec![LabeledSpan::at(token.span(), "Expected ',' here")];
                        miette!(labels = labels, "Expected ',' keyword")
                    })?;
                }
            }
            let right_brace = self.advance()?.clone();
            return Ok(Expression::StructExpression(Rc::new(StructExpression {
                is_empty: false,
                left_brace,
                right_brace,
                elements,
            })));
        }

        // Lambda expression
        if self.check(TokenType::LeftParen) {
            let left_paren = self.advance()?.clone();
            if (self.check(TokenType::Identifier)
                && (self.check_next(TokenType::Comma) || self.check_next(TokenType::RightParen)))
                || self.check(TokenType::RightParen)
            {
                let expression = self.lambda_expression(Some(left_paren));
                return expression;
            }

            // Finally, group back to expression
            let expression = self.expression()?;
            let right_paren = self
                .consume(TokenType::RightParen, |token| {
                    let labels = vec![LabeledSpan::at(token.span(), "Expected ')' here")];
                    miette!(labels = labels, "Expected ')' keyword")
                })?
                .clone();
            return Ok(Expression::GroupExpression(Rc::new(GroupExpression {
                expr: expression,
                left_paren,
                right_paren,
            })));
        }

        Ok(Expression::None)
    }

    fn lambda_expression(
        &mut self,
        left_paren: Option<Token<'ast>>,
    ) -> miette::Result<Expression<'ast>> {
        // Check if we're parsing a function lambda
        let function_token = if self.check(TokenType::Function) {
            Some(self.advance()?.clone())
        } else {
            None
        };

        // If function lambda, left paren is required and must be consumed
        let left_paren = if function_token.is_some() {
            Some(
                self.consume(TokenType::LeftParen, |token| {
                    let labels = vec![LabeledSpan::at(token.span(), "Expected '(' here")];
                    miette!(labels = labels, "Expected '(' after function")
                })?
                .clone(),
            )
        } else {
            left_paren // Use the passed-in value (could be None for simple lambdas)
        };

        // Consume params as comma separated identifiers
        let mut parameters = Vec::new();

        while !self.check(TokenType::Lambda) && !self.check(TokenType::RightParen) {
            let param = self
                .consume(TokenType::Identifier, |token| {
                    let labels = vec![LabeledSpan::at(token.span(), "Expected identifier here")];
                    miette!(labels = labels, "Expected identifier")
                })?
                .clone();
            let comma_token = if self.check(TokenType::Comma) {
                Some(self.advance()?.clone())
            } else {
                None
            };
            let has_comma = comma_token.is_some();
            parameters.push((param, comma_token));

            if self.check(TokenType::RightParen) || self.check(TokenType::Lambda) {
                break;
            }

            // If we didn't consume a comma above, we expect one now
            if !has_comma {
                self.consume(TokenType::Comma, |token| {
                    let labels = vec![LabeledSpan::at(token.span(), "Expected ',' here")];
                    miette!(labels = labels, "Expected ',' keyword")
                })?;
            }
        }

        let mut right_paren = None;
        if self.check(TokenType::RightParen) {
            right_paren = Some(self.advance()?.clone());
        }

        // Function lambdas don't use => token, regular lambdas do
        let lambda_token = if function_token.is_some() {
            // Function lambdas should NOT have => token
            if self.check(TokenType::Lambda) {
                let labels = vec![LabeledSpan::at(
                    self.peek().span(),
                    "Function lambdas do not use '=>'",
                )];
                self.error(miette!(labels = labels, "Remove '=>' from function lambda"))?;
            }
            // Create a dummy token for consistency (won't be rendered)
            Token {
                token_type: crate::lexer::TokenType::Lambda,
                lexeme: "",
                line: 0,
                column: 0,
                end_column: 0,
                span: crate::lexer::SourceSpan { start: 0, end: 0 },
                comments: None,
                trailing_comments: None,
                lines_before: 0,
            }
        } else {
            // Regular lambdas require => token
            self.consume(TokenType::Lambda, |token| {
                let labels = vec![LabeledSpan::at(token.span(), "Expected '=>' here")];
                miette!(labels = labels, "Expected '=>'")
            })?
            .clone()
        };

        let (body, left_brace, right_brace) = self.consume_statement_block(true)?;
        Ok(Expression::LambdaExpression(Rc::new(LambdaExpression {
            function_token,
            left_paren,
            right_paren,
            parameters,
            lambda_token,
            body,
            left_brace,
            right_brace,
        })))
    }

    // Pass in function calling as expression, either identifier or array access usually
    // Processing function arguments
    fn function_call(&mut self, function: Expression<'ast>) -> miette::Result<Expression<'ast>> {
        let left_paren = self
            .consume(TokenType::LeftParen, |token| {
                let labels = vec![LabeledSpan::at(token.span(), "Expected '(' here")];
                miette!(labels = labels, "Expected '(' keyword")
            })?
            .clone();

        // Function arguments, can be named, eg arg1 = "value"
        // Can be separate by commas or not,
        // cannot mix named and unnamed arguments
        if !self.check(TokenType::RightParen) {
            let mut arguments = Vec::new();

            loop {
                // Named argument, can use either = or : to express
                if self.check_next(TokenType::Equal) || self.check_next(TokenType::Colon) {
                    let name = self.advance()?.clone();

                    // Consume either = or :
                    self.advance_check(TokenType::Colon);
                    self.advance_check(TokenType::Equal);

                    let value = self.expression()?;

                    // Check for comma token
                    let comma_token = if self.check(TokenType::Comma) {
                        Some(self.advance()?.clone())
                    } else {
                        None
                    };

                    arguments.push((Some(name), value, comma_token));
                } else {
                    // Unnamed argument
                    let expr = self.expression()?;

                    // Check for comma token
                    let comma_token = if self.check(TokenType::Comma) {
                        Some(self.advance()?.clone())
                    } else {
                        None
                    };

                    arguments.push((None, expr, comma_token));
                }

                // If we didn't consume a comma above, or we hit the right paren, break
                if arguments.last().unwrap().2.is_none() || self.check(TokenType::RightParen) {
                    break;
                }
            }

            let right_paren = self
                .consume(TokenType::RightParen, |token| {
                    let labels = vec![LabeledSpan::at(token.span(), "Expected ')' here")];
                    miette!(labels = labels, "Expected ')' keyword")
                })?
                .clone();

            return Ok(Expression::FunctionCall(Rc::new(FunctionCall {
                name: function,
                args: arguments,
                left_paren,
                right_paren,
            })));
        }

        let right_paren = self
            .consume(TokenType::RightParen, |token| {
                let labels = vec![LabeledSpan::at(token.span(), "Expected ')' here")];
                miette!(labels = labels, "Expected ')' keyword")
            })?
            .clone();

        return Ok(Expression::FunctionCall(Rc::new(FunctionCall {
            name: function,
            args: Vec::new(),
            left_paren,
            right_paren,
        })));
    }
}
