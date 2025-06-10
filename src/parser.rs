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
                comments: None,
                lines_before: 0,
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
            let return_token = self
                .consume(TokenType::Return, "Expected return statement")
                .clone();
            let expression = self.expression();
            self.advance_check(TokenType::Semicolon);
            return Statement::ReturnStatement(Rc::new(ReturnStatement {
                return_token,
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
        if self.check(TokenType::Equal) {
            let equals_token = self.consume(TokenType::Equal, "Expected '='").clone();
            let value = self.expression();
            self.advance_check(TokenType::Semicolon);
            return Statement::VariableAssignment(Rc::new(VariableAssignment {
                equals_token,
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
                    op: op.clone(),
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
        let var_token = self
            .consume(TokenType::Var, "Expected 'var' keyword")
            .clone();
        let name = self
            .consume(TokenType::Identifier, "Expected variable name")
            .clone();
        let equals_token = self
            .consume(TokenType::Equal, "Expected assignment operator '='")
            .clone();
        let value = self.expression();
        self.advance_check(TokenType::Semicolon);
        return Statement::VariableDeclaration(Rc::new(VariableDeclaration {
            var_token,
            name,
            value,
            equals_token,
        }));
    }

    /// Consume a statement block. Takes param if braces are optional for the block, and if they are it means
    /// can consume a single statement without braces
    /// Return type is a tuple of the statements, and the optional opening and closing brace tokens
    fn consume_statement_block(
        &mut self,
        optional_braces: bool,
    ) -> (
        Vec<Statement<'ast>>,
        Option<Token<'ast>>,
        Option<Token<'ast>>,
    ) {
        let mut has_braces = false;
        let mut left_brace = None;
        let mut right_brace = None;

        // If { exists, consume multiple statements, otherwise only consume one
        if optional_braces {
            has_braces = self.check(TokenType::LeftBrace);
            if has_braces {
                left_brace = Some(self.advance().clone());
            }
        } else {
            has_braces = true;
            left_brace = Some(self.consume(TokenType::LeftBrace, "Expected '{'").clone());
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
            if self.check(TokenType::RightBrace) {
                right_brace = Some(self.advance().clone());
            }
        } else {
            right_brace = Some(self.consume(TokenType::RightBrace, "Expected '}'").clone());
        }

        (body, left_brace, right_brace)
    }

    fn function_definition(&mut self) -> Statement<'ast> {
        let access_modifier = match self.peek().token_type {
            TokenType::Public => Some(AccessModifier::Public),
            TokenType::Private => Some(AccessModifier::Private),
            TokenType::Protected => Some(AccessModifier::Protected),
            _ => None,
        };
        let mut access_modifier_token = None;
        if (access_modifier.is_some()) {
            access_modifier_token = Some(self.advance().clone());
        }

        let mut return_type = None;
        if self.check(TokenType::Identifier) {
            return_type = Some(self.advance().clone());
        }

        let function_token = self
            .consume(TokenType::Function, "Expected 'function' keyword")
            .clone();

        let name = self
            .consume(TokenType::Identifier, "Expected function name")
            .clone();

        let left_paren = self.consume(TokenType::LeftParen, "Expected '('").clone();
        let mut parameters = Vec::new();
        if !self.check(TokenType::RightParen) {
            parameters = self.parameters();
        }
        let right_paren = self.consume(TokenType::RightParen, "Expected ')'").clone();
        let (body, left_brace, right_brace) = self.consume_statement_block(false);

        let function_definition: FunctionDefinition = FunctionDefinition {
            access_modifier,
            access_modifier_token,
            return_type,
            function_token,
            name,
            left_paren,
            parameters,
            right_paren,
            body,
            left_brace: left_brace.unwrap(),
            right_brace: right_brace.unwrap(),
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
        let mut required = None;
        if self.check(TokenType::Required) {
            required = Some(self.advance().clone());
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
        let mut default_value_token = None;
        if self.check(TokenType::Equal) {
            default_value_token = Some(self.advance().clone());
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
            equals_token: default_value_token,
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
            body = Some(self.consume_statement_block(true).0);
        }

        self.advance_check(TokenType::Semicolon);

        Statement::LuceeFunction(Rc::new(LuceeFunction {
            name,
            attributes,
            body,
        }))
    }

    fn component_definition(&mut self) -> Statement<'ast> {
        let component_token = self
            .consume(TokenType::Component, "Expected 'component' keyword")
            .clone();

        let attributes = self.attribute_definitions();

        let (body, left_brace, right_brace) = self.consume_statement_block(false);

        Statement::ComponentDefinition(Rc::new(ComponentDefinition {
            component_token,
            attributes,
            body,
            left_brace: left_brace.unwrap(),
            right_brace: right_brace.unwrap(),
        }))
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
        let if_token = self.consume(TokenType::If, "Expected 'if' keyword").clone();
        self.consume(TokenType::LeftParen, "Expected '('");

        let condition = self.expression();

        self.consume(TokenType::RightParen, "Expected ')'");

        let (body, _, _) = self.consume_statement_block(true);

        let mut else_body = None;
        let mut else_token = None;
        if self.check(TokenType::Else) {
            else_token = Some(self.advance().clone());
            if self.check(TokenType::If) {
                // Else body is another IF statement
                else_body = Some(vec![self.if_statement()]);
            } else {
                // Else body is consumed statements
                else_body = Some(self.consume_statement_block(true).0);
            }
        }

        Statement::IfStatement(Rc::new(IfStatement {
            if_token,
            condition,
            body,
            else_body,
            else_token,
        }))
    }

    fn for_statement(&mut self) -> Statement<'ast> {
        let for_token = self
            .consume(TokenType::For, "Expected 'for' keyword")
            .clone();

        let left_paren = self.consume(TokenType::LeftParen, "Expected '('").clone();

        // Check optional "var" keyword
        let mut var_token = None;
        if self.check(TokenType::Var) {
            var_token = Some(self.advance().clone());
        }

        // Consume identifier, if next keyword is in, is a for in loop
        let name = self
            .consume(TokenType::Identifier, "Expected identifier")
            .clone();

        if self.check(TokenType::In) {
            let in_token = self.consume(TokenType::In, "Expected 'in' keyword").clone();
            let expression = self.expression();
            let right_paren = self.consume(TokenType::RightParen, "Expected ')'").clone();
            let (body, left_brace, right_brace) = self.consume_statement_block(false);
            Statement::ForStatement(Rc::new(ForStatement {
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
                left_brace: left_brace.unwrap(),
                right_brace: right_brace.unwrap(),
            }))
        } else {
            let equals_token = self
                .consume(TokenType::Equal, "Expected assignment operator '='")
                .clone();
            let init = self.expression();
            self.consume(TokenType::Semicolon, "Expected ';'");
            let condition = self.expression();
            self.consume(TokenType::Semicolon, "Expected ';'");
            let increment = self.expression();
            let right_paren = self.consume(TokenType::RightParen, "Expected ')'").clone();
            let (body, left_brace, right_brace) = self.consume_statement_block(false);
            Statement::ForStatement(Rc::new(ForStatement {
                for_token,
                left_paren,
                right_paren,
                control: ForControl::Increment {
                    var_token,
                    init,
                    equals_token,
                    condition,
                    increment,
                },
                body,
                left_brace: left_brace.unwrap(),
                right_brace: right_brace.unwrap(),
            }))
        }
    }

    fn while_statement(&mut self) -> Statement<'ast> {
        if self.check(TokenType::While) {
            let while_token = self.advance().clone();
            let left_paren = self.consume(TokenType::LeftParen, "Expected '('").clone();

            let condition = self.expression();

            let right_paren = self.consume(TokenType::RightParen, "Expected ')'").clone();

            let (body, left_brace, right_brace) = self.consume_statement_block(false);

            return Statement::WhileStatement(Rc::new(WhileStatement {
                do_while: false,
                do_token: None,
                while_token,
                condition,
                left_paren,
                right_paren,
                body,
                left_brace: left_brace.unwrap(),
                right_brace: right_brace.unwrap(),
            }));
        } else if self.check(TokenType::Do) {
            let do_token = Some(self.advance().clone());
            let (body, left_brace, right_brace) = self.consume_statement_block(false);

            let while_token = self
                .consume(TokenType::While, "Expected 'while' keyword")
                .clone();

            let left_paren = self.consume(TokenType::LeftParen, "Expected '('").clone();

            let condition = self.expression();

            let right_paren = self.consume(TokenType::RightParen, "Expected ')'").clone();

            return Statement::WhileStatement(Rc::new(WhileStatement {
                do_while: true,
                do_token,
                while_token,
                condition,
                left_paren,
                right_paren,
                body,
                left_brace: left_brace.unwrap(),
                right_brace: right_brace.unwrap(),
            }));
        }

        self.error("Expected 'while' or 'do' keyword");
        panic!("Expected 'while' or 'do' keyword")
    }

    fn switch_statement(&mut self) -> Statement<'ast> {
        let switch_token = self
            .consume(TokenType::Switch, "Expected 'switch' keyword")
            .clone();
        let left_paren = self.consume(TokenType::LeftParen, "Expected '('").clone();
        let expression = self.expression();
        let right_paren = self.consume(TokenType::RightParen, "Expected ')'").clone();
        let left_brace = self.consume(TokenType::LeftBrace, "Expected '{'").clone();

        let mut cases = Vec::new();

        // Handle case statements
        while !self.check(TokenType::RightBrace) {
            let mut condition = Vec::new();

            // Consume multiple case statements
            while self.check(TokenType::Case) {
                let case_token = self.advance().clone();
                let cond_expr = self.expression();
                let colon = self.consume(TokenType::Colon, "Expected ':'").clone();
                condition.push((case_token, cond_expr, colon));
            }

            let mut is_default = false;

            if self.check(TokenType::Default) {
                if !condition.is_empty() {
                    self.error("Cannot mix 'case' and 'default' in a switch statement");
                }
                let default_token = self.advance().clone();

                is_default = true;
                let colon = self.consume(TokenType::Colon, "Expected ':'").clone();
                condition.push((default_token, Expression::None, colon));
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

        let right_brace = self.consume(TokenType::RightBrace, "Expected '}'").clone();

        Statement::SwitchStatement(Rc::new(SwitchStatement {
            switch_token,
            left_paren,
            right_paren,
            expression,
            left_brace,
            right_brace,
            cases,
        }))
    }

    fn try_catch_statement(&mut self) -> Statement<'ast> {
        let try_token = self
            .consume(TokenType::Try, "Expected 'try' keyword")
            .clone();
        let (try_body, try_left_brace, try_right_brace) = self.consume_statement_block(true);
        let catch_token = self
            .consume(TokenType::Catch, "Expected 'catch' keyword")
            .clone();
        let left_paren = self.consume(TokenType::LeftParen, "Expected '('").clone();
        let mut catch_var_token = None;
        if self.check(TokenType::Var) {
            catch_var_token = Some(self.advance().clone());
        }
        let mut catch_var_type = None;
        if self.check(TokenType::Identifier)
            && (self.check_next(TokenType::Identifier) || self.check_next(TokenType::Dot))
        {
            catch_var_type = Some(self.expression());
        }
        let catch_var = self
            .consume(TokenType::Identifier, "Expected identifier")
            .clone();
        let right_paren = self.consume(TokenType::RightParen, "Expected ')'").clone();
        let (catch_body, catch_left_brace, catch_right_brace) = self.consume_statement_block(true);
        Statement::TryCatchStatement(Rc::new(TryCatchStatement {
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
        }))
    }

    fn expression(&mut self) -> Expression<'ast> {
        let expression = self.ternary();

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

        expression
    }

    fn ternary(&mut self) -> Expression<'ast> {
        let mut expression = self.equality();

        if self.check(TokenType::Question) {
            let question_token = self.advance().clone();
            let true_expr = self.expression();
            let colon_token = self.consume(TokenType::Colon, "Expected ':'").clone();
            let false_expr = self.expression();
            expression = Expression::TernaryExpression(Rc::new(TernaryExpression {
                condition: expression,
                question_token,
                true_expr,
                colon_token,
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
            let op = self.advance().clone();
            let right = self.comparison();
            expression = Expression::BinaryExpression(Rc::new(BinaryExpression {
                left: expression,
                op,
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
            let op = self.advance().clone();
            let right = self.term();
            expression = Expression::BinaryExpression(Rc::new(BinaryExpression {
                left: expression,
                op,
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
            let op = self.advance().clone();
            let right = self.factor();
            expression = Expression::BinaryExpression(Rc::new(BinaryExpression {
                left: expression,
                op,
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
            let op = self.advance().clone();
            let right = self.unary();
            expression = Expression::BinaryExpression(Rc::new(BinaryExpression {
                left: expression,
                op,
                right,
            }));
        }

        expression
    }

    fn unary(&mut self) -> Expression<'ast> {
        // Unary operator - or !
        if self.check(TokenType::Minus) || self.check(TokenType::Bang) {
            let op = self.advance().clone();
            let right = self.dot_access();
            return Expression::UnaryExpression(Rc::new(UnaryExpression { op, expr: right }));
        }

        self.dot_access()
    }

    fn dot_access(&mut self) -> Expression<'ast> {
        let mut expression = self.index_access();

        while self.check(TokenType::Dot) {
            let dot_token = self.advance().clone();
            let property = self.index_access();
            expression = Expression::MemberAccess(Rc::new(MemberAccess {
                object: expression,
                dot_token,
                property,
            }));
        }

        expression
    }

    fn index_access(&mut self) -> Expression<'ast> {
        let mut expression = self.primary();

        while self.check(TokenType::LeftBracket) {
            let left_bracket = self.advance().clone();
            let index = self.expression();
            let right_bracket = self
                .consume(TokenType::RightBracket, "Expected ']'")
                .clone();
            expression = Expression::IndexAccess(Rc::new(IndexAccess {
                object: expression,
                index,
                left_bracket,
                right_bracket,
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
                let name = Expression::Identifier(Rc::new(function));

                return self.function_call(name);
            }

            // If followed by =>, single argument lambda
            if self.check_next(TokenType::Lambda) {
                return self.lambda_expression(None);
            }

            return Expression::Identifier(Rc::new(self.advance().clone()));
        }

        // Object creation
        if self.check(TokenType::New) {
            let new_token = self.advance().clone();
            let expression = self.expression();
            match expression {
                Expression::ObjectCreation(_) => {
                    self.error("Invalid object creation; 'new' appeared more than once");
                }
                _ => {}
            }
            return Expression::ObjectCreation(Rc::new(ObjectCreation {
                new_token,
                expr: expression,
            }));
        }

        // Array literal
        // println!("Start array expression");
        if self.check(TokenType::LeftBracket) {
            let left_bracket = self.advance().clone();
            let mut elements = Vec::new();
            while !self.check(TokenType::RightBracket) {
                elements.push(self.expression());
                if self.check(TokenType::RightBracket) {
                    break;
                }
                self.consume(TokenType::Comma, "Expected ','");
            }
            let right_bracket = self
                .consume(TokenType::RightBracket, "Expected ']'")
                .clone();
            return Expression::ArrayExpression(Rc::new(ArrayExpression {
                left_bracket,
                right_bracket,
                elements,
            }));
        }

        // Struct literal
        if self.check(TokenType::LeftBrace) {
            let left_brace = self.advance().clone();
            let mut elements = Vec::new();
            while !self.check(TokenType::RightBrace) {
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
                if self.check(TokenType::RightBrace) {
                    break;
                }
                self.consume(TokenType::Comma, "Expected ','");
            }
            let right_brace = self.advance().clone();
            return Expression::StructExpression(Rc::new(StructExpression {
                left_brace,
                right_brace,
                elements,
            }));
        }

        // Lambda expression
        if self.check(TokenType::LeftParen) {
            let left_paren = self.advance().clone();
            if (self.check(TokenType::Identifier)
                && (self.check_next(TokenType::Comma) || self.check_next(TokenType::RightParen)))
                || self.check(TokenType::RightParen)
            {
                let expression = self.lambda_expression(Some(left_paren));
                return expression;
            }

            // Finally, group back to expression
            let expression = self.expression();
            let right_paren = self.consume(TokenType::RightParen, "Expected ')'").clone();
            return Expression::GroupExpression(Rc::new(GroupExpression {
                expr: expression,
                left_paren,
                right_paren,
            }));
        }

        Expression::None
    }

    fn lambda_expression(&mut self, left_paren: Option<Token<'ast>>) -> Expression<'ast> {
        // Consume params as comma separated identifiers
        let mut parameters = Vec::new();

        while !self.check(TokenType::Lambda) && !self.check(TokenType::RightParen) {
            parameters.push(
                self.consume(TokenType::Identifier, "Expected identifier")
                    .clone(),
            );
            if self.check(TokenType::RightParen) || self.check(TokenType::Lambda) {
                break;
            }
            self.consume(TokenType::Comma, "Expected ','");
        }

        let mut right_paren = None;
        if self.check(TokenType::RightParen) {
            right_paren = Some(self.advance().clone());
        }

        let lambda_token = self.consume(TokenType::Lambda, "Expected '=>'").clone();

        let (body, left_brace, right_brace) = self.consume_statement_block(true);
        Expression::LambdaExpression(Rc::new(LambdaExpression {
            left_paren,
            right_paren,
            parameters,
            lambda_token,
            body,
            left_brace,
            right_brace,
        }))
    }

    // Pass in function calling as expression, either identifier or array access usually
    // Processing function arguments
    fn function_call(&mut self, function: Expression<'ast>) -> Expression<'ast> {
        let left_paren = self.consume(TokenType::LeftParen, "Expected '('").clone();

        // Function arguments, can be named, eg arg1 = "value"
        // Can be separate by commas or not,
        // cannot mix named and unnamed arguments
        if !self.check(TokenType::RightParen) {
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
                if !self.advance_check(TokenType::Comma) || self.check(TokenType::RightParen) {
                    break;
                }
            }

            let right_paren = self.consume(TokenType::RightParen, "Expected ')'").clone();

            self.advance_check(TokenType::Semicolon);

            return Expression::FunctionCall(Rc::new(FunctionCall {
                name: function,
                args: arguments,
                left_paren,
                right_paren,
            }));
        }

        let right_paren = self.consume(TokenType::RightParen, "Expected ')'").clone();

        self.advance_check(TokenType::Semicolon);

        return Expression::FunctionCall(Rc::new(FunctionCall {
            name: function,
            args: Vec::new(),
            left_paren,
            right_paren,
        }));
    }
}
