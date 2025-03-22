use crate::ast::Expression::Literal as ExpLiteral;
use crate::ast::{
    AccessModifier, ArrayExpression, BinaryExpression, BinaryOperator, CaseStatement,
    ComponentDefinition, Expression, ForControl, ForStatement, FunctionCall, FunctionDefinition,
    GroupExpression, IfStatement, IndexAccess, LambdaExpression, Literal, LiteralValue,
    LuceeFunction, MemberAccess, ObjectCreation, Parameter, ReturnStatement, Statement,
    StructExpression, SwitchStatement, TernaryExpression, TryCatchStatement, UnaryExpression,
    UnaryOperator, VariableAssignment, VariableDeclaration, WhileStatement, AST,
};
use crate::lexer::{Lexer, SourceSpan, Token, TokenType};
use std::rc::Rc;

/// Parse a source string into an AST
/// Lexs tokens one at a time as needed
pub struct Parser<'ast> {
    lexer: Lexer<'ast>,
    behind: Token<'ast>,
    current: Token<'ast>,
    ahead: Token<'ast>,

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
    pub fn new(source: &'ast str) -> Parser<'ast> {
        let mut lexer = Lexer::new(source);
        let current = lexer.scan_token();
        let ahead = lexer.scan_token();
        Parser {
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
            },
            lex_time: 0,
        }
    }

    fn check(&self, token_type: TokenType) -> bool {
        !self.is_at_end() && self.peek().token_type == token_type
    }

    fn check_next(&self, token_type: TokenType) -> bool {
        !self.is_at_end() && self.peek_next().token_type == token_type
    }

    fn advance(&mut self) -> &Token<'ast> {
        self.behind = std::mem::replace(&mut self.current, self.ahead.clone());
        if !self.is_at_end() {
            let start = std::time::Instant::now();
            self.ahead = self.lexer.scan_token();
            self.lex_time += start.elapsed().as_micros();
            // println!("Advance took: {0}ns for {1:?}", start.elapsed().as_nanos(), self.ahead);
        }

        &self.behind
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
    fn consume(&mut self, token_type: TokenType, error: &str) -> &Token<'ast> {
        // println!("Consume: {0:?}, {1:?}", self.peek(), token_type);
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

    fn peek(&self) -> &Token<'ast> {
        &self.current
    }

    fn peek_next(&self) -> &Token<'ast> {
        &self.ahead
    }

    pub fn parse(&mut self) -> AST<'ast> {
        let mut statements = Vec::new();

        while !self.is_at_end() {
            statements.push(self.statement());
        }

        AST {
            source: Rc::new(String::from(self.lexer.source)),
            statements,
        }
    }

    fn statement(&mut self) -> Statement<'ast> {
        // Variable Declaration
        if self.check(TokenType::Var) {
            return self.variable_declaration();
        }

        if self.check(TokenType::Return) {
            self.advance();
            let expression = self.expression();
            self.advance_check(TokenType::Semicolon);
            return Statement::ReturnStatement(Rc::new(ReturnStatement {
                value: Some(expression),
            }));
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
        let expression = self.expression();

        // If assigning expression to something else
        if self.advance_check(TokenType::Equal) {
            let value = self.expression();
            self.advance_check(TokenType::Semicolon);
            return Statement::VariableAssignment(Rc::new(VariableAssignment {
                name: expression,
                value,
            }));
        }

        // Shorthand increments, ++ or --
        // For our AST we will store as normally binary expression of x += 1. Later optimized when formatting/parsing
        if self.check(TokenType::PlusPlus) || self.check(TokenType::MinusMinus) {
            let op = self.advance();
            return Statement::ExpressionStmt(Rc::new(Expression::BinaryExpression(Rc::new(
                BinaryExpression {
                    left: expression,
                    op: match op.token_type {
                        TokenType::PlusPlus => BinaryOperator::PlusPlus,
                        TokenType::MinusMinus => BinaryOperator::MinusMinus,
                        _ => BinaryOperator::PlusEqual,
                    },
                    right: Expression::Literal(Rc::new(Literal {
                        value: LiteralValue::Number(1.0),
                        token: op.clone(),
                    })),
                },
            ))));
        }

        match expression {
            Expression::None => {
                self.error("Invalid expression");
            }
            _ => {}
        }

        Statement::ExpressionStmt(Rc::new(expression))
    }

    fn variable_declaration(&mut self) -> Statement<'ast> {
        if self.advance_check(TokenType::Var) {
            let name = self
                .consume(TokenType::Identifier, "Expected variable name")
                .clone();
            self.consume(TokenType::Equal, "Expected assignment operator '='");
            let value = self.expression();
            self.advance_check(TokenType::Semicolon);
            return Statement::VariableDeclaration(Rc::new(VariableDeclaration { name, value }));
        }

        panic!("Invalid variable declaration");
    }

    fn consume_statement_block(&mut self, optional_braces: bool) -> Vec<Statement<'ast>> {
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

    fn function_definition(&mut self) -> Statement<'ast> {
        let access_modifier = match self.peek().token_type {
            TokenType::Public => {
                self.advance();
                Some(AccessModifier::Public)
            }
            TokenType::Private => {
                self.advance();
                Some(AccessModifier::Private)
            }
            TokenType::Protected => {
                self.advance();
                Some(AccessModifier::Protected)
            }
            _ => None,
        };

        let mut return_type = None;
        if self.check(TokenType::Identifier) {
            return_type = Some(self.advance().clone());
        }

        self.consume(TokenType::Function, "Expected 'function' keyword");

        let name = self
            .consume(TokenType::Identifier, "Expected function name")
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

        Statement::FunctionDefinition(Rc::new(function_definition))
    }

    // Consume parameter list of <required>? <type> <identifier> ("=" <expression>)?
    fn parameters(&mut self) -> Vec<Parameter<'ast>> {
        let mut parameters = Vec::new();

        parameters.push(self.parameter());

        while self.advance_check(TokenType::Comma) {
            parameters.push(self.parameter());
        }

        parameters
    }

    fn parameter(&mut self) -> Parameter<'ast> {
        let mut required = false;
        if self.advance_check(TokenType::Required) {
            required = true;
        }

        // Defining return type
        let mut param_type = None;
        if self.check(TokenType::Identifier) && self.check_next(TokenType::Identifier) {
            param_type = Some(
                self.consume(TokenType::Identifier, "Expected parameter type")
                    .clone(),
            );
        }

        let name = self
            .consume(TokenType::Identifier, "Expected parameter name")
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
    fn lucee_function(&mut self) -> Statement<'ast> {
        let name = self
            .consume(TokenType::Identifier, "Expected identifier")
            .clone();

        let attributes = self.attribute_definitions();

        let mut body = None;
        if !self.check(TokenType::Semicolon) {
            body = Some(self.consume_statement_block(true));
        }

        self.advance_check(TokenType::Semicolon);

        Statement::LuceeFunction(Rc::new(LuceeFunction {
            name,
            attributes,
            body,
        }))
    }

    fn component_definition(&mut self) -> Statement<'ast> {
        self.consume(TokenType::Component, "Expected 'component' keyword");

        let attributes = self.attribute_definitions();

        let body = self.consume_statement_block(false);

        Statement::ComponentDefinition(Rc::new(ComponentDefinition { attributes, body }))
    }

    fn attribute_definitions(&mut self) -> Vec<(Token<'ast>, Expression<'ast>)> {
        let mut attributes = Vec::new();

        while self.check(TokenType::Identifier) {
            let name = self.advance().clone();
            self.consume(TokenType::Equal, "Expected assignment operator '='");
            let value = self.expression();
            attributes.push((name, value));
        }

        attributes
    }

    fn if_statement(&mut self) -> Statement<'ast> {
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

        Statement::IfStatement(Rc::new(IfStatement {
            condition,
            body,
            else_body,
        }))
    }

    fn for_statement(&mut self) -> Statement<'ast> {
        self.consume(TokenType::For, "Expected 'for' keyword");

        self.consume(TokenType::LeftParen, "Expected '('");

        // Check optional "var" keyword
        self.advance_check(TokenType::Var);

        // Consume identifier, if next keyword is in, is a for in loop
        let name = self
            .consume(TokenType::Identifier, "Expected identifier")
            .clone();

        if self.check(TokenType::In) {
            self.consume(TokenType::In, "Expected 'in' keyword");
            let expression = self.expression();
            self.consume(TokenType::RightParen, "Expected ')'");
            let body = self.consume_statement_block(false);
            Statement::ForStatement(Rc::new(ForStatement {
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
            Statement::ForStatement(Rc::new(ForStatement {
                control: ForControl::Increment {
                    init,
                    condition,
                    increment,
                },
                body,
            }))
        }
    }

    fn while_statement(&mut self) -> Statement<'ast> {
        if self.advance_check(TokenType::While) {
            self.consume(TokenType::LeftParen, "Expected '('");

            let condition = self.expression();

            self.consume(TokenType::RightParen, "Expected ')'");

            let body = self.consume_statement_block(false);

            return Statement::WhileStatement(Rc::new(WhileStatement {
                do_while: false,
                condition,
                body,
            }));
        } else if self.advance_check(TokenType::Do) {
            let body = self.consume_statement_block(false);

            self.consume(TokenType::While, "Expected 'while' keyword");

            self.consume(TokenType::LeftParen, "Expected '('");

            let condition = self.expression();

            self.consume(TokenType::RightParen, "Expected ')'");

            return Statement::WhileStatement(Rc::new(WhileStatement {
                do_while: true,
                condition,
                body,
            }));
        }

        self.error("Expected 'while' or 'do' keyword");
        panic!("Expected 'while' or 'do' keyword")
    }

    fn switch_statement(&mut self) -> Statement<'ast> {
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
                panic!("Expected 'case' or 'default' keyword");
            }

            // Case body: We consume until break or return expression, since don't need {} to declare
            // statement body
            let mut body = Vec::new();
            while !self.check(TokenType::Break)
                && !self.check(TokenType::Return)
                && !self.check(TokenType::RightBrace)
            {
                body.push(self.statement());
            }

            // Parse last break / return statement, optional as can be empty case
            if self.check(TokenType::Break) {
                let break_statement = self.advance().clone();
                self.consume(TokenType::Semicolon, "Expected ';'");
                body.push(Statement::LuceeFunction(Rc::new(LuceeFunction {
                    name: break_statement,
                    attributes: Vec::new(),
                    body: None,
                })));
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

        Statement::SwitchStatement(Rc::new(SwitchStatement { expression, cases }))
    }

    fn try_catch_statement(&mut self) -> Statement<'ast> {
        self.consume(TokenType::Try, "Expected 'try' keyword");
        let try_body = self.consume_statement_block(true);
        self.consume(TokenType::Catch, "Expected 'catch' keyword");
        self.consume(TokenType::LeftParen, "Expected '('");
        self.advance_check(TokenType::Var);
        let catch_var = self
            .consume(TokenType::Identifier, "Expected identifier")
            .clone();
        self.consume(TokenType::RightParen, "Expected ')'");
        let catch_body = self.consume_statement_block(true);
        Statement::TryCatchStatement(Rc::new(TryCatchStatement {
            try_body,
            catch_var,
            catch_body,
        }))
    }

    fn expression(&mut self) -> Expression<'ast> {
        self.ternary()
    }

    fn ternary(&mut self) -> Expression<'ast> {
        let mut expression = self.equality();

        if self.advance_check(TokenType::Question) {
            let true_expr = self.expression();
            self.consume(TokenType::Colon, "Expected ':'");
            let false_expr = self.expression();
            expression = Expression::TernaryExpression(Rc::new(TernaryExpression {
                condition: expression,
                true_expr,
                false_expr,
            }));
        }

        expression
    }

    fn equality(&mut self) -> Expression<'ast> {
        let mut expression = self.comparison();

        while self.check(TokenType::EqualEqual)
            || self.check(TokenType::BangEqual)
            || self.check(TokenType::Eq)
            || self.check(TokenType::Neq)
        {
            let operator = self.advance().token_type;
            let right = self.comparison();
            expression = Expression::BinaryExpression(Rc::new(BinaryExpression {
                left: expression,
                op: match operator {
                    TokenType::EqualEqual => BinaryOperator::Equal,
                    TokenType::BangEqual => BinaryOperator::NotEqual,
                    TokenType::Eq => BinaryOperator::Eq,
                    TokenType::Neq => BinaryOperator::Neq,
                    _ => BinaryOperator::Equal,
                },
                right,
            }));
        }

        expression
    }

    fn comparison(&mut self) -> Expression<'ast> {
        let mut expression = self.term();

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
            let operator = self.advance().token_type;
            let right = self.term();
            expression = Expression::BinaryExpression(Rc::new(BinaryExpression {
                left: expression,
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
                right,
            }));
        }

        expression
    }

    fn term(&mut self) -> Expression<'ast> {
        let mut expression = self.factor();

        while self.check(TokenType::Plus)
            || self.check(TokenType::Minus)
            || self.check(TokenType::Ampersand)
            || self.check(TokenType::PlusEqual)
            || self.check(TokenType::MinusEqual)
            || self.check(TokenType::AmpersandEqual)
        {
            let operator = self.advance().token_type;
            let right = self.factor();
            expression = Expression::BinaryExpression(Rc::new(BinaryExpression {
                left: expression,
                op: match operator {
                    TokenType::Plus => BinaryOperator::Add,
                    TokenType::Minus => BinaryOperator::Subtract,
                    TokenType::Ampersand => BinaryOperator::StringConcat,
                    TokenType::PlusEqual => BinaryOperator::PlusEqual,
                    TokenType::MinusEqual => BinaryOperator::MinusEqual,
                    TokenType::AmpersandEqual => BinaryOperator::ConcatEqual,
                    _ => BinaryOperator::Add,
                },
                right,
            }));
        }

        expression
    }

    fn factor(&mut self) -> Expression<'ast> {
        let mut expression = self.unary();

        while self.check(TokenType::Star)
            || self.check(TokenType::Slash)
            || self.check(TokenType::StarEqual)
            || self.check(TokenType::SlashEqual)
        {
            let operator = self.advance().token_type;
            let right = self.unary();
            expression = Expression::BinaryExpression(Rc::new(BinaryExpression {
                left: expression,
                op: match operator {
                    TokenType::Star => BinaryOperator::Multiply,
                    TokenType::Slash => BinaryOperator::Divide,
                    TokenType::StarEqual => BinaryOperator::MultiplyEqual,
                    TokenType::SlashEqual => BinaryOperator::DivideEqual,
                    _ => BinaryOperator::Multiply,
                },
                right,
            }));
        }

        expression
    }

    fn unary(&mut self) -> Expression<'ast> {
        // Unary operator - or !
        if self.check(TokenType::Minus) || self.check(TokenType::Bang) {
            let operator = self.advance().token_type;
            let right = self.dot_access();
            return Expression::UnaryExpression(Rc::new(UnaryExpression {
                op: match operator {
                    TokenType::Minus => UnaryOperator::Negate,
                    TokenType::Bang => UnaryOperator::Not,
                    _ => UnaryOperator::Negate,
                },
                expr: right,
            }));
        }

        self.dot_access()
    }

    fn dot_access(&mut self) -> Expression<'ast> {
        let mut expression = self.index_access();

        while self.advance_check(TokenType::Dot) {
            let property = self.index_access();
            expression = Expression::MemberAccess(Rc::new(MemberAccess {
                object: expression,
                property,
            }));
        }

        expression
    }

    fn index_access(&mut self) -> Expression<'ast> {
        let mut expression = self.primary();

        while self.advance_check(TokenType::LeftBracket) {
            let index = self.expression();
            self.consume(TokenType::RightBracket, "Expected ']'");
            expression = Expression::IndexAccess(Rc::new(IndexAccess {
                object: expression,
                index,
            }));
        }

        expression
    }

    fn primary(&mut self) -> Expression<'ast> {
        // Literals
        if self.check(TokenType::String) {
            let token = self.advance().clone();
            let value: String = String::from(token.lexeme);
            return ExpLiteral(Rc::new(Literal {
                token,
                value: LiteralValue::String(value),
            }));
        }

        if self.check(TokenType::Number) {
            let token = self.advance().clone();
            let value: f64 = token.lexeme.parse().unwrap();
            return ExpLiteral(Rc::new(Literal {
                token,
                value: LiteralValue::Number(value),
            }));
        }

        if self.check(TokenType::True) {
            return ExpLiteral(Rc::new(Literal {
                token: self.advance().clone(),
                value: LiteralValue::Boolean(true),
            }));
        }

        if self.check(TokenType::False) {
            return ExpLiteral(Rc::new(Literal {
                token: self.advance().clone(),
                value: LiteralValue::Boolean(false),
            }));
        }

        if self.check(TokenType::Null) {
            return ExpLiteral(Rc::new(Literal {
                token: self.advance().clone(),
                value: LiteralValue::Null,
            }));
        }

        // Identifier for function call
        // Edge-case: contains is both a keyword and a in-build function
        if self.check(TokenType::Identifier) || self.check(TokenType::Contains) {
            // If followed by (, it's a function call
            if self.check_next(TokenType::LeftParen) {
                let function = self.advance().clone();

                self.consume(TokenType::LeftParen, "Expected '('");

                // Function arguments, can be named, eg arg1 = "value"
                // Can be separate by commas or not,
                // cannot mix named and unnamed arguments
                if !self.advance_check(TokenType::RightParen) {
                    let mut arguments = Vec::new();

                    loop {
                        // Named argument
                        if self.check_next(TokenType::Equal) {
                            let name = self.advance().clone();

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

                    return Expression::FunctionCall(Rc::new(FunctionCall {
                        name: function,
                        args: arguments,
                    }));
                }

                self.advance_check(TokenType::Semicolon);

                return Expression::FunctionCall(Rc::new(FunctionCall {
                    name: function,
                    args: Vec::new(),
                }));
            }

            // If followed by =>, single argument lambda
            if self.check_next(TokenType::Lambda) {
                return self.lambda_expression();
            }

            return Expression::Identifier(Rc::new(self.advance().clone()));
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
            return Expression::ObjectCreation(Rc::new(ObjectCreation { expr: expression }));
        }

        // Array literal
        // println!("Start array expression");
        if self.advance_check(TokenType::LeftBracket) {
            let mut elements = Vec::new();
            while !self.advance_check(TokenType::RightBracket) {
                elements.push(self.expression());
                if self.advance_check(TokenType::RightBracket) {
                    break;
                }
                self.consume(TokenType::Comma, "Expected ','");
            }
            return Expression::ArrayExpression(Rc::new(ArrayExpression { elements }));
        }

        // Struct literal
        if self.advance_check(TokenType::LeftBrace) {
            let mut elements = Vec::new();
            while !self.advance_check(TokenType::RightBrace) {
                // Key is identifier or String
                let mut key = None;
                if self.check(TokenType::Identifier) || self.check(TokenType::String) {
                    key = Some(self.advance().clone());
                } else {
                    self.error("Expected struct key");
                }
                if !self.advance_check(TokenType::Colon) && !self.advance_check(TokenType::Equal) {
                    self.error("Struct keys can be assigned with ':' or '='");
                }
                let value = self.expression();
                elements.push((key.unwrap(), value));
                if self.advance_check(TokenType::RightBrace) {
                    break;
                }
                self.consume(TokenType::Comma, "Expected ','");
            }
            return Expression::StructExpression(Rc::new(StructExpression { elements }));
        }

        // Lambda expression
        if self.advance_check(TokenType::LeftParen) {
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
            return Expression::GroupExpression(Rc::new(GroupExpression { expr: expression }));
        }

        Expression::None
    }

    fn lambda_expression(&mut self) -> Expression<'ast> {
        // Consume params as comma separated identifiers
        let mut parameters = Vec::new();

        while !self.check(TokenType::Lambda) && !self.check(TokenType::RightParen) {
            parameters.push(
                self.consume(TokenType::Identifier, "Expected identifier")
                    .clone(),
            );
            if self.advance_check(TokenType::RightParen) || self.advance_check(TokenType::Lambda) {
                break;
            }
            self.consume(TokenType::Comma, "Expected ','");
        }

        self.consume(TokenType::Lambda, "Expected '=>'");

        let body = self.consume_statement_block(true);
        Expression::LambdaExpression(Rc::new(LambdaExpression { parameters, body }))
    }
}
