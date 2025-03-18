use crate::ast::Expression::Literal as ExpLiteral;
use crate::ast::{
    AccessModifier, BinaryOperator, CaseStatement, ComponentDefinition, ControlStructure,
    Expression, ForControl, FunctionDefinition, Literal, LoopStatement, Parameter, Statement,
    UnaryOperator,
};
use crate::lexer::{Token, TokenType};
use std::collections::HashMap;

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

/**
* Parser generates a literal AST, as in translates text to syntax. Performs
* basic syntax checking as it parses, but does not validate syntax with context. For example
* in Lucee we can't define a function within a function. This Parser will parse that, but not validate
* that it is correct
*/
impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, current: 0 }
    }

    fn check(&self, token_type: TokenType) -> bool {
        if self.is_at_end() {
            false
        } else {
            self.peek().token_type == token_type
        }
    }

    fn check_next(&self, token_type: TokenType) -> bool {
        if self.is_at_end() {
            false
        } else {
            self.peek_next().token_type == token_type
        }
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }

        self.previous()
    }

    // Check next token is token_type, and if so advance.
    // If not match, safe exit
    fn advance_check(&mut self, token_type: TokenType) -> bool {
        println!("Advance Check: {0:?}, {1:?}", self.peek(), token_type);
        if self.check(token_type) {
            self.advance();
            true
        } else {
            false
        }
    }

    // Expect token and consume, otherwise error
    fn consume(&mut self, token_type: TokenType, error: &str) -> &Token {
        println!("Consume: {0:?}, {1:?}", self.peek(), token_type);
        if self.check(token_type) {
            return self.advance();
        }

        // TODO: Properly handle parsing errors
        panic!("Parser error at {0:?}: {1}", self.peek(), error);
    }

    fn error(&self, message: &str) {
        panic!("Parser error at {0:?}: {1}", self.peek(), message);
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::EOF
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn peek_next(&self) -> &Token {
        &self.tokens[self.current + 1]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    pub fn parse(&mut self) -> Vec<Statement> {
        let mut statements = Vec::new();

        while !self.is_at_end() {
            statements.push(self.statement());
        }

        statements
    }

    fn statement(&mut self) -> Statement {
        // Variable Declaration
        if self.check(TokenType::Var) {
            return self.variable_declaration();
        }

        if self.check(TokenType::Return) {
            self.advance();
            // TODO: Return doesn't need an expression
            let expression = self.expression();
            self.advance_check(TokenType::Semicolon);
            return Statement::ReturnStatement(Some(expression));
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
            && (self.check_next(TokenType::LeftBrace) || self.check_next(TokenType::Identifier))
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

        // CfmlTag
        if self.check(TokenType::Less) && self.check_next(TokenType::Identifier) {
            return self.cfml_tag();
        }

        // Expression Statement
        let expression = self.expression();

        // If assigning expression to something else
        if self.advance_check(TokenType::Equal) {
            let value = self.expression();
            self.advance_check(TokenType::Semicolon);
            return Statement::VariableAssignment {
                name: expression,
                value,
            };
        }

        // Shorthand increments, ++ or --
        // For our AST we will store as normally binary expression of x += 1. Later optimized when formatting/parsing
        if self.advance_check(TokenType::PlusPlus) || self.advance_check(TokenType::MinusMinus) {
            return Statement::ExpressionStmt(Expression::BinaryExpression {
                left: Box::from(expression),
                op: match self.previous().token_type {
                    TokenType::PlusPlus => BinaryOperator::PlusEqual,
                    TokenType::MinusMinus => BinaryOperator::MinusEqual,
                    _ => BinaryOperator::PlusEqual,
                },
                right: Box::new(Expression::Literal(Literal::Number(1.0))),
            });
        }

        Statement::ExpressionStmt(expression)
    }

    fn variable_declaration(&mut self) -> Statement {
        if self.advance_check(TokenType::Var) {
            let identifier = self.consume(TokenType::Identifier, "Expected variable name");
            let name = identifier.lexeme.clone();
            self.consume(TokenType::Equal, "Expected assignment operator '='");
            let value = self.expression();
            self.advance_check(TokenType::Semicolon);
            return Statement::VariableDeclaration { name, value };
        }

        panic!("Invalid variable declaration");
    }

    fn consume_statement_block(&mut self, optional_braces: bool) -> Vec<Statement> {
        let mut has_braces = false;

        // If { exists, consume multiple statements, otherwise only consume one
        if optional_braces {
            has_braces = self.advance_check(TokenType::LeftBrace);
        } else {
            has_braces = true;
            self.consume(TokenType::LeftBrace, "Expected '{'");
        }

        let mut body = Vec::new();
        while !self.advance_check(TokenType::RightBrace) {
            body.push(self.statement());
            // Only consume one statement if no braces
            if self.check(TokenType::RightBrace) || !has_braces {
                break;
            }
        }

        if optional_braces {
            self.advance_check(TokenType::RightBrace);
        } else {
            self.consume(TokenType::RightBrace, "Expected '}'");
        }

        body
    }

    fn function_definition(&mut self) -> Statement {
        let mut access_modifier = None;
        if self.check(TokenType::Public) {
            access_modifier = Some(AccessModifier::Public);
            self.advance();
        } else if self.check(TokenType::Private) {
            access_modifier = Some(AccessModifier::Private);
            self.advance();
        } else if self.check(TokenType::Protected) {
            access_modifier = Some(AccessModifier::Protected);
            self.advance();
        }

        let mut return_type = None;
        if self.check(TokenType::Identifier) {
            return_type = Some(self.advance().lexeme.clone());
        }

        self.consume(TokenType::Function, "Expected 'function' keyword");

        let name = self
            .consume(TokenType::Identifier, "Expected function name")
            .lexeme
            .clone();

        self.consume(TokenType::LeftParen, "Expected '('");
        let mut parameters = Vec::new();
        if !self.check(TokenType::RightParen) {
            parameters = self.parameters();
        }
        self.consume(TokenType::RightParen, "Expected ')'");
        let body = self.consume_statement_block(false);

        let function_definition: FunctionDefinition = FunctionDefinition {
            access_modifier,
            return_type,
            name,
            parameters,
            body,
        };

        Statement::FunctionDefinition(function_definition)
    }

    // Consume parameter list of <required>? <type> <identifier> ("=" <expression>)?
    fn parameters(&mut self) -> Vec<Parameter> {
        let mut parameters = Vec::new();

        parameters.push(self.parameter());

        while self.advance_check(TokenType::Comma) {
            parameters.push(self.parameter());
        }

        parameters
    }

    fn parameter(&mut self) -> Parameter {
        let mut required = false;
        if self.advance_check(TokenType::Required) {
            required = true;
        }

        // Defining return type
        let mut param_type = None;
        if self.check(TokenType::Identifier) && self.check_next(TokenType::Identifier) {
            param_type = Some(
                self.consume(TokenType::Identifier, "Expected parameter type")
                    .lexeme
                    .clone(),
            );
        }

        let name = self
            .consume(TokenType::Identifier, "Expected parameter name")
            .lexeme
            .clone();

        let mut default_value = None;
        if self.advance_check(TokenType::Equal) {
            default_value = Some(self.expression());
            if default_value.is_none() {
                self.error("Expected default value");
            }
        }

        Parameter {
            required,
            param_type,
            name,
            default_value,
        }
    }

    /**
     * Lucee function refers to statements that look like the following
     *
     * lock name="myLock" type="exclusive" timeout="10" {
     *
     * {
     *
     * "Calling" a function like a statement with an attribute list
     */
    fn lucee_function(&mut self) -> Statement {
        let identifier = self.consume(TokenType::Identifier, "Expected identifier");

        let attributes = self.attribute_definitions();

        let body = self.consume_statement_block(true);

        self.advance_check(TokenType::Semicolon);

        Statement::LuceeFunction {
            attributes,
            body: Some(body),
        }
    }

    fn component_definition(&mut self) -> Statement {
        self.consume(TokenType::Component, "Expected 'component' keyword");

        let attributes = self.attribute_definitions();

        let body = self.consume_statement_block(false);

        Statement::ComponentDefinition(ComponentDefinition { attributes, body })
    }

    fn attribute_definitions(&mut self) -> HashMap<String, Expression> {
        println!("Trying component attributes");
        let mut attributes = HashMap::new();

        while self.check(TokenType::Identifier) {
            let identifier = self.advance();
            let name = identifier.lexeme.clone();
            println!("Consumed Identifier: {:?}", name);
            self.consume(TokenType::Equal, "Expected assignment operator '='");
            let value = self.expression();
            println!("Consumed Value: {:?}", value);
            attributes.insert(name, value);
        }

        attributes
    }

    fn if_statement(&mut self) -> Statement {
        self.consume(TokenType::If, "Expected 'if' keyword");
        self.consume(TokenType::LeftParen, "Expected '('");

        let condition = self.expression();

        self.consume(TokenType::RightParen, "Expected ')'");

        let body = self.consume_statement_block(true);

        let mut else_body = None;
        if self.advance_check(TokenType::Else) {
            if self.check(TokenType::If) {
                // Else body is another IF statement
                else_body = Some(vec![self.if_statement()]);
            } else {
                // Else body is consumed statements
                else_body = Some(self.consume_statement_block(true));
            }
        }

        Statement::ControlStructure(ControlStructure::IfStatement {
            condition,
            body,
            else_body,
        })
    }

    fn for_statement(&mut self) -> Statement {
        self.consume(TokenType::For, "Expected 'for' keyword");

        self.consume(TokenType::LeftParen, "Expected '('");

        // Check optional "var" keyword
        self.advance_check(TokenType::Var);

        // Consume identifier, if next keyword is in, is a for in loop
        let name = self
            .consume(TokenType::Identifier, "Expected identifier")
            .lexeme
            .clone();

        if self.check(TokenType::In) {
            self.consume(TokenType::In, "Expected 'in' keyword");
            let expression = self.expression();
            self.consume(TokenType::RightParen, "Expected ')'");
            let body = self.consume_statement_block(false);
            Statement::ControlStructure(ControlStructure::LoopStatement(LoopStatement::For {
                control: ForControl::LoopOver {
                    variable: name,
                    array: expression,
                },
                body,
            }))
        } else {
            self.consume(TokenType::Equal, "Expected assignment operator '='");
            let init = self.expression();
            self.consume(TokenType::Semicolon, "Expected ';'");
            let condition = self.expression();
            self.consume(TokenType::Semicolon, "Expected ';'");
            let increment = self.expression();
            self.consume(TokenType::RightParen, "Expected ')'");
            let body = self.consume_statement_block(false);
            Statement::ControlStructure(ControlStructure::LoopStatement(LoopStatement::For {
                control: ForControl::Increment {
                    init,
                    condition,
                    increment,
                },
                body,
            }))
        }
    }

    fn while_statement(&mut self) -> Statement {
        if self.advance_check(TokenType::While) {
            self.consume(TokenType::LeftParen, "Expected '('");

            let condition = self.expression();

            self.consume(TokenType::RightParen, "Expected ')'");

            let body = self.consume_statement_block(false);

            return Statement::ControlStructure(ControlStructure::LoopStatement(
                LoopStatement::While { condition, body },
            ));
        } else if self.advance_check(TokenType::Do) {
            let body = self.consume_statement_block(false);

            self.consume(TokenType::While, "Expected 'while' keyword");

            self.consume(TokenType::LeftParen, "Expected '('");

            let condition = self.expression();

            self.consume(TokenType::RightParen, "Expected ')'");

            return Statement::ControlStructure(ControlStructure::LoopStatement(
                LoopStatement::DoWhile { condition, body },
            ));
        }

        self.error("Expected 'while' or 'do' keyword");
        panic!("Expected 'while' or 'do' keyword")
    }

    fn switch_statement(&mut self) -> Statement {
        self.consume(TokenType::Switch, "Expected 'switch' keyword");
        self.consume(TokenType::LeftParen, "Expected '('");
        let expression = self.expression();
        self.consume(TokenType::RightParen, "Expected ')'");
        self.consume(TokenType::LeftBrace, "Expected '{'");

        let mut cases = Vec::new();

        // Handle case statements
        while !self.advance_check(TokenType::RightBrace) {
            let mut condition = Vec::new();

            // Consume multiple case statements
            while self.advance_check(TokenType::Case) {
                condition.push(self.expression());
                self.consume(TokenType::Colon, "Expected ':'");
            }

            let mut is_default = false;

            if self.advance_check(TokenType::Default) {
                if !condition.is_empty() {
                    self.error("Cannot mix 'case' and 'default' in a switch statement");
                }

                is_default = true;
                self.consume(TokenType::Colon, "Expected ':'");
            }

            if condition.is_empty() && !is_default {
                self.error("Expected 'case' or 'default' keyword");
                return Statement::Invalid {
                    error: "Expected 'case' or 'default' keyword".to_string(),
                    token: self.peek().clone(),
                };
            }

            // Case body: We consume until break or return expression, since don't need {} to declare
            // statement body
            let mut body = Vec::new();
            while !self.check(TokenType::Break)
                && !self.check(TokenType::Return)
                && !self.check(TokenType::RightBrace)
            {
                println!("Trying to consume case statements");
                body.push(self.statement());
            }

            // Parse last break / return statement, optional as can be empty case
            if self.advance_check(TokenType::Break) {
                self.consume(TokenType::Semicolon, "Expected ';'");
            } else if self.check(TokenType::Return) {
                // Push return statement
                body.push(self.statement());
            }

            cases.push(CaseStatement {
                is_default,
                condition: if is_default { None } else { Some(condition) },
                body,
            });
        }

        Statement::ControlStructure(ControlStructure::SwitchStatement { expression, cases })
    }

    fn cfml_tag(&mut self) -> Statement {
        // TODO
        Statement::VariableDeclaration {
            name: String::from(""),
            value: Expression::Literal(Literal::Null),
        }
    }

    fn expression(&mut self) -> Expression {
        self.ternary()
    }

    fn ternary(&mut self) -> Expression {
        let mut expression = self.equality();

        if self.advance_check(TokenType::Question) {
            let true_expr = self.expression();
            self.consume(TokenType::Colon, "Expected ':'");
            let false_expr = self.expression();
            expression = Expression::TernaryExpression {
                condition: Box::new(expression),
                true_expr: Box::new(true_expr),
                false_expr: Box::new(false_expr),
            };
        }

        expression
    }

    fn equality(&mut self) -> Expression {
        let mut expression = self.comparison();

        while self.advance_check(TokenType::EqualEqual)
            || self.advance_check(TokenType::BangEqual)
            || self.advance_check(TokenType::Eq)
            || self.advance_check(TokenType::Neq)
        {
            let operator = self.previous().clone().token_type;
            let right = self.comparison();
            expression = Expression::BinaryExpression {
                left: Box::new(expression),
                op: match operator {
                    TokenType::EqualEqual => BinaryOperator::Equal,
                    TokenType::BangEqual => BinaryOperator::NotEqual,
                    TokenType::Eq => BinaryOperator::Eq,
                    TokenType::Neq => BinaryOperator::Neq,
                    _ => BinaryOperator::Equal,
                },
                right: Box::new(right),
            };
        }

        expression
    }

    fn comparison(&mut self) -> Expression {
        let mut expression = self.term();

        while self.advance_check(TokenType::Less)
            || self.advance_check(TokenType::Greater)
            || self.advance_check(TokenType::LessEqual)
            || self.advance_check(TokenType::GreaterEqual)
            || self.advance_check(TokenType::Lt)
            || self.advance_check(TokenType::Gt)
            || self.advance_check(TokenType::AmpersandAmpersand)
            || self.advance_check(TokenType::PipePipe)
            || self.advance_check(TokenType::And)
            || self.advance_check(TokenType::Or)
            || self.advance_check(TokenType::Contains)
            || self.advance_check(TokenType::Xor)
        {
            let operator = self.previous().clone().token_type;
            let right = self.term();
            expression = Expression::BinaryExpression {
                left: Box::new(expression),
                op: match operator {
                    TokenType::Less => BinaryOperator::Less,
                    TokenType::Greater => BinaryOperator::Greater,
                    TokenType::LessEqual => BinaryOperator::LessEqual,
                    TokenType::GreaterEqual => BinaryOperator::GreaterEqual,
                    TokenType::Lt => BinaryOperator::Lt,
                    TokenType::Gt => BinaryOperator::Gt,
                    TokenType::AmpersandAmpersand => BinaryOperator::And,
                    TokenType::PipePipe => BinaryOperator::Or,
                    TokenType::And => BinaryOperator::LogicalAnd,
                    TokenType::Or => BinaryOperator::LogicalOr,
                    TokenType::Contains => BinaryOperator::Contains,
                    TokenType::Xor => BinaryOperator::Xor,
                    _ => BinaryOperator::Less,
                },
                right: Box::new(right),
            };
        }

        expression
    }

    fn term(&mut self) -> Expression {
        let mut expression = self.factor();

        while self.advance_check(TokenType::Plus)
            || self.advance_check(TokenType::Minus)
            || self.advance_check(TokenType::Ampersand)
            || self.advance_check(TokenType::PlusEqual)
            || self.advance_check(TokenType::MinusEqual)
            || self.advance_check(TokenType::AmpersandEqual)
        {
            let operator = self.previous().clone().token_type;
            let right = self.factor();
            expression = Expression::BinaryExpression {
                left: Box::new(expression),
                op: match operator {
                    TokenType::Plus => BinaryOperator::Add,
                    TokenType::Minus => BinaryOperator::Subtract,
                    TokenType::Ampersand => BinaryOperator::StringConcat,
                    TokenType::PlusEqual => BinaryOperator::PlusEqual,
                    TokenType::MinusEqual => BinaryOperator::MinusEqual,
                    TokenType::AmpersandEqual => BinaryOperator::ConcatEqual,
                    _ => BinaryOperator::Add,
                },
                right: Box::new(right),
            };
        }

        expression
    }

    fn factor(&mut self) -> Expression {
        let mut expression = self.unary();

        while self.advance_check(TokenType::Star)
            || self.advance_check(TokenType::Slash)
            || self.advance_check(TokenType::StarEqual)
            || self.advance_check(TokenType::SlashEqual)
        {
            let operator = self.previous().clone().token_type;
            let right = self.unary();
            expression = Expression::BinaryExpression {
                left: Box::new(expression),
                op: match operator {
                    TokenType::Star => BinaryOperator::Multiply,
                    TokenType::Slash => BinaryOperator::Divide,
                    TokenType::StarEqual => BinaryOperator::MultiplyEqual,
                    TokenType::SlashEqual => BinaryOperator::DivideEqual,
                    _ => BinaryOperator::Multiply,
                },
                right: Box::new(right),
            };
        }

        expression
    }

    fn unary(&mut self) -> Expression {
        // Unary operator - or !
        if self.check(TokenType::Minus) || self.check(TokenType::Bang) {
            let operator = self.advance().clone().token_type;
            let right = self.dot_access();
            return Expression::UnaryExpression {
                op: match operator {
                    TokenType::Minus => UnaryOperator::Negate,
                    TokenType::Bang => UnaryOperator::Not,
                    _ => UnaryOperator::Negate,
                },
                expr: Box::new(right),
            };
        }

        self.dot_access()
    }

    fn dot_access(&mut self) -> Expression {
        let mut expression = self.index_access();
        println!("Dot access {:?}", expression);

        while self.advance_check(TokenType::Dot) {
            let property = self.index_access();
            expression = Expression::MemberAccess {
                object: Box::new(expression),
                property: Box::new(property),
            };
        }

        expression
    }

    fn index_access(&mut self) -> Expression {
        let mut expression = self.primary();

        while self.advance_check(TokenType::LeftBracket) {
            let index = self.expression();
            self.consume(TokenType::RightBracket, "Expected ']'");
            expression = Expression::IndexAccess {
                object: Box::new(expression),
                index: Box::new(index),
            };
        }

        expression
    }

    fn primary(&mut self) -> Expression {
        // Literals
        if self.check(TokenType::String) {
            return ExpLiteral(Literal::String(self.advance().lexeme.clone()));
        }

        if self.check(TokenType::Number) {
            return ExpLiteral(Literal::Number(self.advance().lexeme.parse().unwrap()));
        }

        if self.check(TokenType::True) {
            self.advance();
            return ExpLiteral(Literal::Boolean(true));
        }

        if self.check(TokenType::False) {
            self.advance();
            return ExpLiteral(Literal::Boolean(false));
        }

        if self.check(TokenType::Null) {
            self.advance();
            return ExpLiteral(Literal::Null);
        }

        // Identifier for function call
        // Edge-case: contains is both a keyword and a in-build function
        if self.check(TokenType::Identifier) || self.check(TokenType::Contains) {
            // If followed by (, it's a function call
            if self.check_next(TokenType::LeftParen) {
                let function = self.advance().lexeme.clone();

                self.consume(TokenType::LeftParen, "Expected '('");

                // Function arguments, can be named, eg arg1 = "value"
                // Can be separate by commas or not,
                // cannot mix named and unnamed arguments
                if !self.advance_check(TokenType::RightParen) {
                    let mut arguments = Vec::new();

                    loop {
                        // Named argument
                        if self.check_next(TokenType::Equal) {
                            let name = self.advance().lexeme.clone();

                            self.consume(TokenType::Equal, "Expected '='");

                            let value = self.expression();

                            arguments.push((Some(name), value));
                        } else {
                            // Unnamed argument
                            arguments.push((None, self.expression()));
                        }

                        // If comma, keep parsing, or stop if encountered ')'
                        if !self.advance_check(TokenType::Comma)
                            || self.advance_check(TokenType::RightParen)
                        {
                            break;
                        }
                    }

                    self.consume(TokenType::RightParen, "Expected ')'");

                    self.advance_check(TokenType::Semicolon);

                    return Expression::FunctionCall {
                        name: function,
                        args: arguments,
                    };
                }

                self.advance_check(TokenType::Semicolon);

                return Expression::FunctionCall {
                    name: function,
                    args: Vec::new(),
                };
            }

            // If followed by =>, single argument lambda
            if self.check_next(TokenType::Lambda) {
                return self.lambda_expression();
            }

            return Expression::Identifier(self.advance().lexeme.clone());
        }

        // Object creation
        if self.advance_check(TokenType::New) {
            let expression = self.expression();
            match expression {
                Expression::ObjectCreation(_) => {
                    self.error("Invalid object creation; 'new' appeared more than once");
                }
                _ => {}
            }
            return Expression::ObjectCreation(Box::new(expression));
        }

        // Array literal
        // println!("Start array expression");
        if self.advance_check(TokenType::LeftBracket) {
            println!("Start array expression {:?}", self.peek());
            let mut elements = Vec::new();
            while !self.advance_check(TokenType::RightBracket) {
                println!("Try Consume {:?}", self.peek());
                elements.push(self.expression());
                println!("Post Consume {:?}", self.peek());
                if self.advance_check(TokenType::RightBracket) {
                    break;
                }
                self.consume(TokenType::Comma, "Expected ','");
            }
            return Expression::ArrayExpression(elements);
        }

        // Struct literal
        if self.advance_check(TokenType::LeftBrace) {
            let mut elements = HashMap::new();
            while !self.advance_check(TokenType::RightBrace) {
                // Key is identifier or String
                let mut key = String::from("");
                if self.check(TokenType::Identifier) || self.check(TokenType::String) {
                    key = self.advance().lexeme.clone();
                } else {
                    self.error("Expected struct key");
                }
                if !self.advance_check(TokenType::Colon) && !self.advance_check(TokenType::Equal) {
                    self.error("Struct keys can be assigned with ':' or '='");
                }
                let value = self.expression();
                elements.insert(key, value);
                if self.advance_check(TokenType::RightBrace) {
                    break;
                }
                self.consume(TokenType::Comma, "Expected ','");
            }
            return Expression::StructExpression(elements);
        }

        // Lambda expression
        if self.advance_check(TokenType::LeftParen) {
            println!("Trying lambda expression");
            if (self.check(TokenType::Identifier)
                && (self.check_next(TokenType::Comma) || self.check_next(TokenType::RightParen)))
                || self.check(TokenType::RightParen)
            {
                let expression = self.lambda_expression();
                return expression;
            }

            // Finally, group back to expression
            let expression = self.expression();
            self.consume(TokenType::RightParen, "Expected ')'");
            return Expression::GroupExpression(Box::new(expression));
        }

        Expression::None
    }

    fn lambda_expression(&mut self) -> Expression {
        // Consume params as comma separated identifiers
        let mut parameters = Vec::new();

        while !self.check(TokenType::Lambda) && !self.check(TokenType::RightParen) {
            parameters.push(
                self.consume(TokenType::Identifier, "Expected identifier")
                    .lexeme
                    .clone(),
            );
            if self.advance_check(TokenType::RightParen) || self.advance_check(TokenType::Lambda) {
                break;
            }
            self.consume(TokenType::Comma, "Expected ','");
        }

        self.consume(TokenType::Lambda, "Expected '=>'");

        let body = self.consume_statement_block(true);
        Expression::LambdaExpression { parameters, body }
    }
}
