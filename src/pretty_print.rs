use crate::ast::*;
use crate::visitor::{Visitor, Walkable};
use std::fmt::Write as FmtWrite;

/// Configuration options for pretty printing the AST
#[derive(Debug, Clone)]
pub struct PrettyPrintConfig {
    /// Whether to show token positions (line:col)
    pub show_tokens: bool,
    /// Whether to show actual token lexemes
    pub show_token_values: bool,
    /// Maximum depth to display (None for unlimited)
    pub max_depth: Option<usize>,
    /// Use ANSI color codes
    pub use_colors: bool,
    /// More concise output vs verbose
    pub compact_mode: bool,
}

impl Default for PrettyPrintConfig {
    fn default() -> Self {
        Self {
            show_tokens: false,
            show_token_values: true,
            max_depth: None,
            use_colors: false,
            compact_mode: false,
        }
    }
}

/// ANSI color codes for terminal output
struct Colors;

impl Colors {
    const RESET: &'static str = "\x1b[0m";
    const STATEMENT: &'static str = "\x1b[34m"; // Blue
    const EXPRESSION: &'static str = "\x1b[32m"; // Green
    const LITERAL: &'static str = "\x1b[33m"; // Yellow
    const OPERATOR: &'static str = "\x1b[36m"; // Cyan
    const KEYWORD: &'static str = "\x1b[35m"; // Magenta
    const DIM: &'static str = "\x1b[2m"; // Dim/gray
}

/// Visitor implementation for pretty printing the AST
pub struct PrettyPrintVisitor {
    output: String,
    /// Stack tracking which levels need continuation bars (│)
    indent_stack: Vec<bool>,
    current_depth: usize,
    config: PrettyPrintConfig,
}

impl PrettyPrintVisitor {
    pub fn new(config: PrettyPrintConfig) -> Self {
        Self {
            output: String::new(),
            indent_stack: Vec::new(),
            current_depth: 0,
            config,
        }
    }

    pub fn into_string(self) -> String {
        self.output
    }

    /// Write a node with tree drawing characters
    fn write_node(&mut self, text: &str) {
        let prefix = self.get_prefix();
        writeln!(self.output, "{}{}", prefix, text).unwrap();
    }

    /// Write a leaf value (no children)
    fn write_leaf(&mut self, text: &str) {
        self.write_node(text);
    }

    /// Get the current tree drawing prefix based on indent stack
    fn get_prefix(&self) -> String {
        let mut prefix = String::new();

        for (i, &needs_bar) in self.indent_stack.iter().enumerate() {
            if i == self.indent_stack.len() - 1 {
                // Last level - use branch character
                prefix.push_str(if needs_bar {
                    "├── "
                } else {
                    "└── "
                });
            } else {
                // Middle levels - use continuation or space
                prefix.push_str(if needs_bar { "│   " } else { "    " });
            }
        }

        prefix
    }

    /// Enter a new indentation level
    fn indent(&mut self, has_more_siblings: bool) {
        self.indent_stack.push(has_more_siblings);
        self.current_depth += 1;
    }

    /// Exit an indentation level
    fn dedent(&mut self) {
        self.indent_stack.pop();
        self.current_depth -= 1;
    }

    /// Check if we've exceeded max depth
    fn is_max_depth(&self) -> bool {
        if let Some(max) = self.config.max_depth {
            self.current_depth >= max
        } else {
            false
        }
    }

    /// Colorize text if colors are enabled
    fn colorize(&self, text: &str, color: &str) -> String {
        if self.config.use_colors {
            format!("{}{}{}", color, text, Colors::RESET)
        } else {
            text.to_string()
        }
    }

    /// Format a token with optional position and value
    fn format_token(&self, token: &crate::lexer::Token, prefix: &str) -> String {
        let mut parts = vec![prefix.to_string()];

        if self.config.show_token_values {
            parts.push(format!("\"{}\"", token.lexeme));
        }

        if self.config.show_tokens {
            parts.push(format!("({}:{})", token.line, token.column));
        }

        parts.join(" ")
    }

    /// Visit a collection of statements
    fn visit_statements(&mut self, statements: &[Statement], label: &str) {
        if statements.is_empty() {
            self.write_leaf(&format!("{}: (empty)", label));
            return;
        }

        self.write_node(&format!("{}: {} items", label, statements.len()));

        if self.is_max_depth() {
            self.indent(false);
            self.write_leaf("...(max depth reached)");
            self.dedent();
            return;
        }

        for (i, stmt) in statements.iter().enumerate() {
            let has_more = i < statements.len() - 1;
            self.indent(has_more);
            stmt.walk(self);
            self.dedent();
        }
    }

    /// Visit an optional expression
    fn visit_optional_expression(&mut self, expr: &Option<Expression>, label: &str) {
        match expr {
            Some(e) => {
                self.write_node(&format!("{}:", label));
                self.indent(false);
                e.walk(self);
                self.dedent();
            }
            None => {
                self.write_leaf(&format!("{}: (none)", label));
            }
        }
    }

    /// Visit a required expression
    fn visit_child_expression(&mut self, expr: &Expression, label: &str, has_more_siblings: bool) {
        self.write_node(&format!("{}:", label));
        self.indent(has_more_siblings);
        expr.walk(self);
        self.dedent();
    }
}

impl<'ast> Visitor<()> for PrettyPrintVisitor {
    fn visit(&mut self, ast: &AST) -> () {
        let title = self.colorize("AST", Colors::KEYWORD);
        self.write_node(&title);

        if ast.statements.is_empty() {
            self.indent(false);
            self.write_leaf("(empty)");
            self.dedent();
            return;
        }

        for (i, stmt) in ast.statements.iter().enumerate() {
            let has_more = i < ast.statements.len() - 1;
            self.indent(has_more);
            stmt.walk(self);
            self.dedent();
        }
    }

    fn visit_statement(&mut self, stmt: &Statement) -> () {
        match stmt {
            Statement::ExpressionStmt(e) => {
                let title = self.colorize("ExpressionStatement", Colors::STATEMENT);
                self.write_node(&title);
                self.indent(false);
                e.walk(self);
                self.dedent();
            }
            Statement::VariableDeclaration(v) => v.walk(self),
            Statement::ReturnStatement(r) => r.walk(self),
            Statement::BreakStatement(b) => b.walk(self),
            Statement::ContinueStatement(c) => c.walk(self),
            Statement::FunctionDefinition(f) => f.walk(self),
            Statement::ComponentDefinition(c) => c.walk(self),
            Statement::LuceeFunction(l) => l.walk(self),
            Statement::IfStatement(i) => i.walk(self),
            Statement::ForStatement(f) => f.walk(self),
            Statement::WhileStatement(w) => w.walk(self),
            Statement::SwitchStatement(s) => s.walk(self),
            Statement::TryCatchStatement(t) => t.walk(self),
        }
    }

    fn visit_expression(&mut self, expr: &Expression) -> () {
        match expr {
            Expression::Literal(l) => l.walk(self),
            Expression::Identifier(i) => {
                let name = self.format_token(i, "Identifier");
                let colored = self.colorize(&name, Colors::EXPRESSION);
                self.write_leaf(&colored);
            }
            Expression::FunctionCall(f) => f.walk(self),
            Expression::ObjectCreation(o) => o.walk(self),
            Expression::VariableAssignment(v) => v.walk(self),
            Expression::ArrayExpression(a) => a.walk(self),
            Expression::StructExpression(s) => s.walk(self),
            Expression::LambdaExpression(l) => l.walk(self),
            Expression::BinaryExpression(b) => b.walk(self),
            Expression::UnaryExpression(u) => u.walk(self),
            Expression::TernaryExpression(t) => t.walk(self),
            Expression::GroupExpression(g) => g.walk(self),
            Expression::MemberAccess(m) => m.walk(self),
            Expression::IndexAccess(i) => i.walk(self),
            Expression::StaticAccess(s) => s.walk(self),
            Expression::None => {
                let colored = self.colorize("Expression::None", Colors::DIM);
                self.write_leaf(&colored);
            }
        }
    }

    fn visit_expression_statement(&mut self, stmt: &ExpressionStatement) -> () {
        stmt.expression.walk(self);
    }

    fn visit_variable_declaration(&mut self, var: &VariableDeclaration) -> () {
        let name = self.format_token(&var.name, "VariableDeclaration");
        let colored = self.colorize(&name, Colors::STATEMENT);
        self.write_node(&colored);

        self.indent(false);
        self.write_node("value:");
        self.indent(false);
        var.value.walk(self);
        self.dedent();
        self.dedent();
    }

    fn visit_variable_assignment(&mut self, assign: &VariableAssignment) -> () {
        let colored = self.colorize("VariableAssignment", Colors::EXPRESSION);
        self.write_node(&colored);

        self.indent(true);
        self.write_node("target:");
        self.indent(false);
        assign.name.walk(self);
        self.dedent();
        self.dedent();

        self.indent(false);
        self.write_node("value:");
        self.indent(false);
        assign.value.walk(self);
        self.dedent();
        self.dedent();
    }

    fn visit_return_statement(&mut self, ret: &ReturnStatement) -> () {
        let colored = self.colorize("ReturnStatement", Colors::STATEMENT);
        self.write_node(&colored);

        if let Some(ref value) = ret.value {
            self.indent(false);
            self.write_node("value:");
            self.indent(false);
            value.walk(self);
            self.dedent();
            self.dedent();
        }
    }

    fn visit_break_statement(&mut self, _brk: &BreakStatement) -> () {
        let colored = self.colorize("BreakStatement", Colors::STATEMENT);
        self.write_leaf(&colored);
    }

    fn visit_continue_statement(&mut self, _cont: &ContinueStatement) -> () {
        let colored = self.colorize("ContinueStatement", Colors::STATEMENT);
        self.write_leaf(&colored);
    }

    fn visit_function_definition(&mut self, func: &FunctionDefinition) -> () {
        let name = self.format_token(&func.name, "FunctionDefinition");
        let colored = self.colorize(&name, Colors::STATEMENT);
        self.write_node(&colored);

        self.indent(true);
        if let Some(ref access) = func.access_modifier {
            self.write_leaf(&format!("access: {:?}", access));
        } else {
            self.write_leaf("access: (none)");
        }
        self.dedent();

        self.indent(true);
        if let Some(ref ret_type) = func.return_type {
            self.write_leaf(&self.format_token(ret_type, "return_type:"));
        } else {
            self.write_leaf("return_type: (none)");
        }
        self.dedent();

        self.indent(true);
        self.write_node(&format!("parameters: {} items", func.parameters.len()));
        if !func.parameters.is_empty() && !self.is_max_depth() {
            for (i, (param, _)) in func.parameters.iter().enumerate() {
                let has_more = i < func.parameters.len() - 1;
                self.indent(has_more);
                let param_name = self.format_token(&param.name, "Parameter");
                self.write_node(&param_name);

                if param.required.is_some() {
                    self.indent(param.param_type.is_some() || param.default_value.is_some());
                    self.write_leaf("required: true");
                    self.dedent();
                }

                if let Some(ref ptype) = param.param_type {
                    self.indent(param.default_value.is_some());
                    self.write_leaf(&self.format_token(ptype, "type:"));
                    self.dedent();
                }

                if let Some(ref default) = param.default_value {
                    self.indent(false);
                    self.write_node("default:");
                    self.indent(false);
                    default.walk(self);
                    self.dedent();
                    self.dedent();
                }

                self.dedent();
            }
        }
        self.dedent();

        self.indent(!func.body.is_empty());
        if !func.attributes.is_empty() {
            self.write_node(&format!("attributes: {} items", func.attributes.len()));
        } else {
            self.write_leaf("attributes: (none)");
        }
        self.dedent();

        self.indent(false);
        self.visit_statements(&func.body, "body");
        self.dedent();
    }

    fn visit_component_definition(&mut self, comp: &ComponentDefinition) -> () {
        let colored = self.colorize("ComponentDefinition", Colors::STATEMENT);
        self.write_node(&colored);

        self.indent(true);
        self.write_node(&format!("attributes: {} items", comp.attributes.len()));
        self.dedent();

        self.indent(false);
        self.visit_statements(&comp.body, "body");
        self.dedent();
    }

    fn visit_lucee_function(&mut self, func: &LuceeFunction) -> () {
        let name = self.format_token(&func.name, "LuceeFunction");
        let colored = self.colorize(&name, Colors::STATEMENT);
        self.write_node(&colored);

        self.indent(true);
        self.write_node(&format!("attributes: {} items", func.attributes.len()));
        self.dedent();

        self.indent(false);
        if let Some(ref body) = func.body {
            self.visit_statements(body, "body");
        } else {
            self.write_leaf("body: (none)");
        }
        self.dedent();
    }

    fn visit_if_statement(&mut self, if_stmt: &IfStatement) -> () {
        let colored = self.colorize("IfStatement", Colors::STATEMENT);
        self.write_node(&colored);

        self.indent(true);
        self.write_node("condition:");
        self.indent(false);
        if_stmt.condition.walk(self);
        self.dedent();
        self.dedent();

        self.indent(if_stmt.else_body.is_some());
        self.visit_statements(&if_stmt.body, "then_body");
        self.dedent();

        if let Some(ref else_body) = if_stmt.else_body {
            self.indent(false);
            self.visit_statements(else_body, "else_body");
            self.dedent();
        }
    }

    fn visit_for_statement(&mut self, for_stmt: &ForStatement) -> () {
        let colored = self.colorize("ForStatement", Colors::STATEMENT);
        self.write_node(&colored);

        self.indent(true);
        match &for_stmt.control {
            ForControl::Increment {
                variable,
                init,
                condition,
                increment,
                ..
            } => {
                self.write_node("control: Increment");

                self.indent(true);
                self.write_leaf(&self.format_token(variable, "variable:"));
                self.dedent();

                self.indent(true);
                self.write_node("init:");
                self.indent(false);
                init.walk(self);
                self.dedent();
                self.dedent();

                self.indent(true);
                self.write_node("condition:");
                self.indent(false);
                condition.walk(self);
                self.dedent();
                self.dedent();

                self.indent(false);
                self.write_node("increment:");
                self.indent(false);
                increment.walk(self);
                self.dedent();
                self.dedent();
            }
            ForControl::LoopOver {
                variable, array, ..
            } => {
                self.write_node("control: LoopOver");

                self.indent(true);
                self.write_leaf(&self.format_token(variable, "variable:"));
                self.dedent();

                self.indent(false);
                self.write_node("array:");
                self.indent(false);
                array.walk(self);
                self.dedent();
                self.dedent();
            }
        }
        self.dedent();

        self.indent(false);
        self.visit_statements(&for_stmt.body, "body");
        self.dedent();
    }

    fn visit_while_statement(&mut self, while_stmt: &WhileStatement) -> () {
        let kind = if while_stmt.do_while {
            "DoWhileStatement"
        } else {
            "WhileStatement"
        };
        let colored = self.colorize(kind, Colors::STATEMENT);
        self.write_node(&colored);

        self.indent(true);
        self.write_node("condition:");
        self.indent(false);
        while_stmt.condition.walk(self);
        self.dedent();
        self.dedent();

        self.indent(false);
        if let Some(ref body) = while_stmt.body {
            self.visit_statements(body, "body");
        } else {
            self.write_leaf("body: (none)");
        }
        self.dedent();
    }

    fn visit_switch_statement(&mut self, switch: &SwitchStatement) -> () {
        let colored = self.colorize("SwitchStatement", Colors::STATEMENT);
        self.write_node(&colored);

        self.indent(true);
        self.write_node("expression:");
        self.indent(false);
        switch.expression.walk(self);
        self.dedent();
        self.dedent();

        self.indent(false);
        self.write_node(&format!("cases: {} items", switch.cases.len()));

        if !switch.cases.is_empty() && !self.is_max_depth() {
            for (i, case) in switch.cases.iter().enumerate() {
                let has_more = i < switch.cases.len() - 1;
                self.indent(has_more);

                let case_type = if case.is_default {
                    "DefaultCase"
                } else {
                    "Case"
                };
                self.write_node(case_type);

                self.indent(true);
                self.write_node(&format!("conditions: {} items", case.condition.len()));
                for (j, (_, expr, _)) in case.condition.iter().enumerate() {
                    let has_more_cond = j < case.condition.len() - 1;
                    self.indent(has_more_cond);
                    expr.walk(self);
                    self.dedent();
                }
                self.dedent();

                self.indent(false);
                self.visit_statements(&case.body, "body");
                self.dedent();

                self.dedent();
            }
        }
        self.dedent();
    }

    fn visit_try_catch_statement(&mut self, try_catch: &TryCatchStatement) -> () {
        let colored = self.colorize("TryCatchStatement", Colors::STATEMENT);
        self.write_node(&colored);

        self.indent(true);
        self.visit_statements(&try_catch.try_body, "try_body");
        self.dedent();

        self.indent(try_catch.finally_body.is_some());
        self.write_node("catch:");
        self.indent(true);
        self.write_leaf(&self.format_token(&try_catch.catch_var, "variable:"));
        self.dedent();
        if let Some(ref catch_type) = try_catch.catch_var_type {
            self.indent(true);
            self.write_node("type:");
            self.indent(false);
            catch_type.walk(self);
            self.dedent();
            self.dedent();
        }
        self.indent(false);
        self.visit_statements(&try_catch.catch_body, "catch_body");
        self.dedent();
        self.dedent();

        if let Some(ref finally_body) = try_catch.finally_body {
            self.indent(false);
            self.visit_statements(finally_body, "finally_body");
            self.dedent();
        }
    }

    fn visit_literal(&mut self, lit: &Literal) -> () {
        let value_str = match &lit.value {
            LiteralValue::Number(n) => format!("Number({})", n),
            LiteralValue::String(s) => {
                let quote = if s.is_double_quote { "\"" } else { "'" };
                format!("String({}{}{})", quote, s.value, quote)
            }
            LiteralValue::Boolean(b) => format!("Boolean({})", b),
            LiteralValue::Null => "Null".to_string(),
        };

        let colored = self.colorize(&format!("Literal: {}", value_str), Colors::LITERAL);
        self.write_leaf(&colored);
    }

    fn visit_function_call(&mut self, call: &FunctionCall) -> () {
        let colored = self.colorize("FunctionCall", Colors::EXPRESSION);
        self.write_node(&colored);

        self.indent(true);
        self.write_node("name:");
        self.indent(false);
        call.name.walk(self);
        self.dedent();
        self.dedent();

        self.indent(false);
        self.write_node(&format!("arguments: {} items", call.args.len()));

        if !call.args.is_empty() && !self.is_max_depth() {
            for (i, (name, expr, _)) in call.args.iter().enumerate() {
                let has_more = i < call.args.len() - 1;
                self.indent(has_more);

                if let Some(ref arg_name) = name {
                    self.write_node(&self.format_token(arg_name, "named:"));
                    self.indent(false);
                    expr.walk(self);
                    self.dedent();
                } else {
                    expr.walk(self);
                }

                self.dedent();
            }
        }
        self.dedent();
    }

    fn visit_object_creation(&mut self, obj: &ObjectCreation) -> () {
        let colored = self.colorize("ObjectCreation", Colors::EXPRESSION);
        self.write_node(&colored);

        self.indent(false);
        self.write_node("expression:");
        self.indent(false);
        obj.expr.walk(self);
        self.dedent();
        self.dedent();
    }

    fn visit_array_expression(&mut self, arr: &ArrayExpression) -> () {
        let colored = self.colorize(
            &format!("ArrayExpression: {} elements", arr.elements.len()),
            Colors::EXPRESSION,
        );
        self.write_node(&colored);

        if !arr.elements.is_empty() && !self.is_max_depth() {
            for (i, (expr, _)) in arr.elements.iter().enumerate() {
                let has_more = i < arr.elements.len() - 1;
                self.indent(has_more);
                expr.walk(self);
                self.dedent();
            }
        }
    }

    fn visit_struct_expression(&mut self, struct_expr: &StructExpression) -> () {
        let colored = self.colorize(
            &format!("StructExpression: {} elements", struct_expr.elements.len()),
            Colors::EXPRESSION,
        );
        self.write_node(&colored);

        if !struct_expr.elements.is_empty() && !self.is_max_depth() {
            for (i, (key, value, _)) in struct_expr.elements.iter().enumerate() {
                let has_more = i < struct_expr.elements.len() - 1;
                self.indent(has_more);

                self.write_node(&self.format_token(key, "key:"));
                self.indent(false);
                value.walk(self);
                self.dedent();

                self.dedent();
            }
        }
    }

    fn visit_lambda_expression(&mut self, lambda: &LambdaExpression) -> () {
        let colored = self.colorize(
            &format!("LambdaExpression: {} params", lambda.parameters.len()),
            Colors::EXPRESSION,
        );
        self.write_node(&colored);

        self.indent(true);
        self.write_node(&format!("parameters: {} items", lambda.parameters.len()));
        if !lambda.parameters.is_empty() {
            for (i, (param, _)) in lambda.parameters.iter().enumerate() {
                let has_more = i < lambda.parameters.len() - 1;
                self.indent(has_more);
                self.write_leaf(&self.format_token(param, ""));
                self.dedent();
            }
        }
        self.dedent();

        self.indent(false);
        self.visit_statements(&lambda.body, "body");
        self.dedent();
    }

    fn visit_binary_expression(&mut self, bin: &BinaryExpression) -> () {
        let op = self.format_token(&bin.op, "");
        let colored = self.colorize(&format!("BinaryExpression: {}", op), Colors::OPERATOR);
        self.write_node(&colored);

        self.indent(true);
        self.write_node("left:");
        self.indent(false);
        bin.left.walk(self);
        self.dedent();
        self.dedent();

        self.indent(false);
        self.write_node("right:");
        self.indent(false);
        bin.right.walk(self);
        self.dedent();
        self.dedent();
    }

    fn visit_unary_expression(&mut self, unary: &UnaryExpression) -> () {
        let op = self.format_token(&unary.op, "");
        let colored = self.colorize(&format!("UnaryExpression: {}", op), Colors::OPERATOR);
        self.write_node(&colored);

        self.indent(false);
        self.write_node("operand:");
        self.indent(false);
        unary.expr.walk(self);
        self.dedent();
        self.dedent();
    }

    fn visit_ternary_expression(&mut self, ternary: &TernaryExpression) -> () {
        let colored = self.colorize("TernaryExpression", Colors::EXPRESSION);
        self.write_node(&colored);

        self.indent(true);
        self.write_node("condition:");
        self.indent(false);
        ternary.condition.walk(self);
        self.dedent();
        self.dedent();

        self.indent(true);
        self.write_node("true_branch:");
        self.indent(false);
        ternary.true_expr.walk(self);
        self.dedent();
        self.dedent();

        self.indent(false);
        self.write_node("false_branch:");
        self.indent(false);
        ternary.false_expr.walk(self);
        self.dedent();
        self.dedent();
    }

    fn visit_group_expression(&mut self, group: &GroupExpression) -> () {
        let colored = self.colorize("GroupExpression", Colors::EXPRESSION);
        self.write_node(&colored);

        self.indent(false);
        group.expr.walk(self);
        self.dedent();
    }

    fn visit_member_expression(&mut self, member: &MemberAccess) -> () {
        let colored = self.colorize("MemberAccess", Colors::EXPRESSION);
        self.write_node(&colored);

        self.indent(true);
        self.write_node("object:");
        self.indent(false);
        member.object.walk(self);
        self.dedent();
        self.dedent();

        self.indent(false);
        self.write_node("property:");
        self.indent(false);
        member.property.walk(self);
        self.dedent();
        self.dedent();
    }

    fn visit_index_access(&mut self, index: &IndexAccess) -> () {
        let colored = self.colorize("IndexAccess", Colors::EXPRESSION);
        self.write_node(&colored);

        self.indent(true);
        self.write_node("object:");
        self.indent(false);
        index.object.walk(self);
        self.dedent();
        self.dedent();

        self.indent(false);
        self.write_node("index:");
        self.indent(false);
        index.index.walk(self);
        self.dedent();
        self.dedent();
    }

    fn visit_static_access(&mut self, static_access: &StaticAccess) -> () {
        let colored = self.colorize("StaticAccess", Colors::EXPRESSION);
        self.write_node(&colored);

        self.indent(true);
        self.write_leaf(&self.format_token(&static_access.class_name, "class:"));
        self.dedent();

        self.indent(false);
        self.write_node("call:");
        self.indent(false);
        static_access.function_call.walk(self);
        self.dedent();
        self.dedent();
    }

    fn combine_docs(&mut self, _docs: &[()]) -> () {
        // For pretty printing, we don't need to combine docs since we're building a string
        ()
    }
}

/// Pretty print an AST with default configuration
pub fn pretty_print(ast: &AST) -> String {
    let mut visitor = PrettyPrintVisitor::new(PrettyPrintConfig::default());
    ast.walk(&mut visitor);
    visitor.into_string()
}

/// Pretty print an AST with custom configuration
pub fn pretty_print_with_config(ast: &AST, config: PrettyPrintConfig) -> String {
    let mut visitor = PrettyPrintVisitor::new(config);
    ast.walk(&mut visitor);
    visitor.into_string()
}
