use crate::ast::{AccessModifier, ForControl, LiteralValue, Statement, AST};
use crate::lexer::Token;
use crate::visitor::Visitor;

pub struct Formatter {
    pub formatted_source: String,

    pub indent_level: usize,

    // Indicates when printing comments, if should print inline
    // Mostly useful for tokens / expressions that may or not be inline
    pub(crate) inline_comment: bool,
}

impl Formatter {
    fn add_current_indent(&mut self) {
        for _ in 0..self.indent_level {
            self.formatted_source.push_str("    ");
        }
    }

    /// Utility function for popping comments on closing braces
    /// Handles the indent since usually the indent decreases before
    /// printing the final token, but we want to comment to be printed
    /// before the indent stepdown. Without this utility function, it's
    /// harder to preserve indent.
    fn pop_closing_comment(&mut self, token: &Token) {
        // TODO: Handle closing indent for multiple comments
        if token.comments.is_some() {
            let old_indent = self.indent_level;
            self.indent_level = 1; // Reset indent for right brace comment
            self.add_current_indent();
            self.indent_level = old_indent; // Restore indent level
            self.pop_comment(&token, false);
        }
    }

    /// Formats / pops comment on this token. Handles formatting indents, by stripping out newlines
    fn pop_comment(&mut self, token: &Token, inline: bool) {
        let comment = &token.comments;
        // Comments always printed one after the other, no space in between
        if comment.is_some() {
            comment.clone().unwrap().iter().for_each(|comment| {
                let formatted_comment = self.format_comment(&comment.lexeme);
                for line in formatted_comment.lines() {
                    self.formatted_source.push_str(line.trim_end());
                    if (!inline) {
                        self.formatted_source.push('\n');
                    }
                }
                // Add current indent after printing comment, so normal code can continue being printed
                if (!inline) {
                    self.add_current_indent();
                } else {
                    // If inline, just add a space after the comment
                    self.formatted_source.push(' ');
                }
            });
        }

        // Auto set remaining comments to inline
        // Must be manually reset
        self.inline_comment = true;
    }

    fn format_comment(&self, raw: &str) -> String {
        if raw.trim_start().starts_with("/*") {
            self.format_block_comment(raw)
        } else if raw.trim_start().starts_with("//") {
            raw.lines()
                .map(|line| line.trim_start().to_string())
                .collect::<Vec<_>>()
                .join("\n")
        } else {
            raw.to_string()
        }
    }

    fn format_block_comment(&self, raw: &str) -> String {
        let trimmed = raw.trim();

        // Handle inline single-line block comment
        if !trimmed.contains('\n') {
            // Just return with correct indent
            return format!("{}", trimmed);
        }

        let content = trimmed.trim_start_matches("/*").trim_end_matches("*/");

        // Collect non-empty lines and normalize indent
        let lines: Vec<&str> = content.lines().collect();

        // Strip leading and trailing blank lines
        let lines = lines
            .into_iter()
            .skip_while(|line| line.trim().is_empty())
            .collect::<Vec<_>>();

        let lines = lines
            .into_iter()
            .rev()
            .skip_while(|line| line.trim().is_empty())
            .collect::<Vec<_>>()
            .into_iter()
            .rev()
            .collect::<Vec<_>>();

        // Compute minimum common indentation (ignoring blank lines)
        let min_indent = lines
            .iter()
            .filter(|line| !line.trim().is_empty())
            .map(|line| line.chars().take_while(|c| c.is_whitespace()).count())
            .min()
            .unwrap_or(0);

        // Remove min_indent from each line
        let cleaned: Vec<String> = lines
            .iter()
            .map(|line| line.chars().skip(min_indent).collect::<String>())
            .collect();

        let mut result = String::new();
        // Indent already printed before this char
        result.push_str("/*\n");

        for (i, line) in cleaned.iter().enumerate() {
            // Add current indent before every line
            for _ in 0..self.indent_level {
                result.push_str("    ");
            }
            result.push_str("  ");
            result.push_str(line.trim_end());
            result.push('\n');
        }

        // Print closing char with indent
        for _ in 0..self.indent_level {
            result.push_str("    ");
        }
        result.push_str("*/");

        result
    }
}

impl Visitor for Formatter {
    fn visit_statement(&mut self, statement: &Statement) {
        // At every new statement, reset inline comment
        self.inline_comment = false;
        self.walk_statement(statement);
    }

    fn visit_literal(&mut self, literal: &crate::ast::Literal) {
        self.pop_comment(&literal.token, self.inline_comment);
        let literal_value = &literal.value;
        let value = match literal_value {
            LiteralValue::Number(number) => number.to_string(),
            LiteralValue::String(string) => '"'.to_string() + &*string.clone() + &*'"'.to_string(),
            LiteralValue::Boolean(bool) => bool.to_string(),
            LiteralValue::Null => "null".to_string(),
        };
        self.formatted_source.push_str(&value);
    }
    fn visit_identifier(&mut self, identifier: &Token) {
        self.pop_comment(identifier, self.inline_comment);
        self.formatted_source.push_str(&identifier.lexeme);
    }
    fn visit_function_call(&mut self, function_call: &crate::ast::FunctionCall) {
        self.visit_expression(&function_call.name);
        self.pop_comment(&function_call.left_paren, true);
        self.formatted_source.push('(');

        let mut it = function_call.args.iter().peekable();

        while let Some(arg) = it.next() {
            match &arg.0 {
                Some(token) => {
                    self.pop_comment(token, true);
                    self.formatted_source.push_str(&token.lexeme);
                    self.formatted_source.push_str(" = ");
                }
                None => {}
            }

            self.visit_expression(&arg.1);

            if it.peek().is_some() {
                self.formatted_source.push_str(", ");
            }
        }

        self.pop_comment(&function_call.right_paren, true);
        self.formatted_source.push(')');
    }
    fn visit_object_creation(&mut self, object_creation: &crate::ast::ObjectCreation) {
        self.pop_comment(&object_creation.new_token, self.inline_comment);
        self.formatted_source.push_str("new ");
        self.visit_expression(&object_creation.expr);
    }
    fn visit_array_expression(&mut self, array_expression: &crate::ast::ArrayExpression) {
        self.pop_comment(&array_expression.left_bracket, self.inline_comment);
        self.formatted_source.push('[');

        let mut it = array_expression.elements.iter().peekable();

        while let Some(arg) = it.next() {
            self.visit_expression(arg);

            if it.peek().is_some() {
                self.formatted_source.push_str(", ");
            }
        }

        self.pop_comment(&array_expression.right_bracket, true);
        self.formatted_source.push(']');
    }
    fn visit_struct_expression(&mut self, struct_expression: &crate::ast::StructExpression) {
        self.pop_comment(&struct_expression.left_brace, self.inline_comment);
        self.formatted_source.push('{');
        if struct_expression.elements.len() > 0 {
            self.formatted_source.push('\n');
        }
        self.indent_level += 1;

        // TODO: No trailing comma
        struct_expression.elements.iter().for_each(|(key, value)| {
            self.pop_comment(&key, false);
            self.add_current_indent();
            self.formatted_source.push_str(&key.lexeme);
            self.formatted_source.push_str(": ");
            self.visit_expression(value);
            self.formatted_source.push_str(", ");
            self.formatted_source.push('\n');
        });

        self.indent_level -= 1;

        if struct_expression.elements.len() > 0 {
            self.add_current_indent();
        }

        self.pop_closing_comment(&struct_expression.right_brace);
        self.formatted_source.push('}');
    }
    // TODO: Inline short lambdas where it has one statement in the body
    fn visit_lambda_expression(&mut self, lambda_expression: &crate::ast::LambdaExpression) {
        if lambda_expression.left_paren.is_some() {
            self.pop_comment(
                &lambda_expression.left_paren.clone().unwrap(),
                self.inline_comment,
            );
        }
        self.formatted_source.push('(');

        let mut it = lambda_expression.parameters.iter().peekable();

        while let Some(arg) = it.next() {
            self.pop_comment(&arg, self.inline_comment);
            self.formatted_source.push_str(&arg.lexeme);

            if it.peek().is_some() {
                self.formatted_source.push_str(", ");
            }
        }

        if lambda_expression.right_paren.is_some() {
            self.pop_comment(&lambda_expression.right_paren.clone().unwrap(), true);
        }
        self.formatted_source.push_str(") ");

        self.pop_comment(&lambda_expression.lambda_token, true);
        self.formatted_source.push_str("=> ");

        if lambda_expression.left_brace.is_some() {
            self.pop_comment(&lambda_expression.left_brace.clone().unwrap(), true);
        }
        self.formatted_source.push_str("{");

        self.formatted_source.push('\n');
        self.indent_level += 1;
        lambda_expression.body.iter().for_each(|body| {
            self.add_current_indent();
            self.visit_statement(body);
            self.formatted_source.push('\n');
        });

        self.indent_level -= 1;
        self.add_current_indent();
        if lambda_expression.right_brace.is_some() {
            self.pop_closing_comment(&lambda_expression.right_brace.clone().unwrap());
        }
        self.formatted_source.push('}');
    }
    fn visit_binary_expression(&mut self, binary_expression: &crate::ast::BinaryExpression) {
        self.visit_expression(&binary_expression.left);
        self.formatted_source.push(' ');
        self.pop_comment(&binary_expression.op, true);
        self.formatted_source.push_str(binary_expression.op.lexeme);
        self.formatted_source.push(' ');
        self.visit_expression(&binary_expression.right);
    }
    fn visit_unary_expression(&mut self, unary_expression: &crate::ast::UnaryExpression) {
        self.pop_comment(&unary_expression.op, self.inline_comment);
        self.formatted_source.push_str(unary_expression.op.lexeme);
        self.visit_expression(&unary_expression.expr);
    }
    // TODO: Handle wrapping for long chaining
    fn visit_ternary_expression(&mut self, ternary_expression: &crate::ast::TernaryExpression) {
        self.visit_expression(&ternary_expression.condition);
        self.pop_comment(&ternary_expression.question_token, true);
        self.formatted_source.push_str(" ? ");
        self.visit_expression(&ternary_expression.true_expr);
        self.pop_comment(&ternary_expression.colon_token, true);
        self.formatted_source.push_str(" : ");
        self.visit_expression(&ternary_expression.false_expr);
    }
    fn visit_group_expression(&mut self, group_expression: &crate::ast::GroupExpression) {
        self.pop_comment(&group_expression.left_paren, self.inline_comment);
        self.formatted_source.push('(');
        self.visit_expression(&group_expression.expr);
        self.pop_comment(&group_expression.right_paren, true);
        self.formatted_source.push(')');
    }
    fn visit_member_expression(&mut self, member_expression: &crate::ast::MemberAccess) {
        self.visit_expression(&member_expression.object);
        self.pop_comment(&member_expression.dot_token, true);
        self.formatted_source.push('.');
        self.visit_expression(&member_expression.property);
    }
    fn visit_index_access(&mut self, index_access: &crate::ast::IndexAccess) {
        self.visit_expression(&index_access.object);
        self.pop_comment(&index_access.left_bracket, true);
        self.formatted_source.push('[');
        self.visit_expression(&index_access.index);
        self.pop_comment(&index_access.right_bracket, true);
        self.formatted_source.push(']');
    }

    fn visit_variable_declaration(
        &mut self,
        variable_declaration: &crate::ast::VariableDeclaration,
    ) {
        // TODO: Handle setting for all variables use 'var'
        self.pop_comment(&variable_declaration.var_token, self.inline_comment);
        self.formatted_source.push_str("var ");
        self.pop_comment(&variable_declaration.name, true);
        self.formatted_source
            .push_str(&variable_declaration.name.lexeme);
        self.pop_comment(&variable_declaration.equals_token, true);
        self.formatted_source.push_str(" = ");
        self.visit_expression(&variable_declaration.value);
    }
    fn visit_variable_assignment(&mut self, variable_assignment: &crate::ast::VariableAssignment) {
        self.visit_expression(&variable_assignment.name);
        self.pop_comment(&variable_assignment.equals_token, true);
        self.formatted_source.push_str(" = ");
        self.visit_expression(&variable_assignment.value);
    }
    fn visit_return_statement(&mut self, return_statement: &crate::ast::ReturnStatement) {
        self.pop_comment(&return_statement.return_token, self.inline_comment);
        self.formatted_source.push_str("return ");
        match &return_statement.value {
            Some(value) => self.visit_expression(value),
            None => {}
        }
    }
    fn visit_function_definition(&mut self, function_definition: &crate::ast::FunctionDefinition) {
        match &function_definition.access_modifier {
            Some(access_modifier) => {
                self.pop_comment(
                    &function_definition.access_modifier_token.clone().unwrap(),
                    self.inline_comment,
                );
                match access_modifier {
                    AccessModifier::Public => self.formatted_source.push_str("public"),
                    AccessModifier::Private => self.formatted_source.push_str("private"),
                    AccessModifier::Protected => self.formatted_source.push_str("protected"),
                    _ => {}
                }
                self.formatted_source.push(' ');
            }
            None => {}
        }

        match &function_definition.return_type {
            Some(return_type) => {
                self.pop_comment(&function_definition.return_type.clone().unwrap(), true);
                self.formatted_source.push_str(&return_type.lexeme);
                self.formatted_source.push(' ');
            }
            None => {}
        }

        self.pop_comment(&function_definition.function_token, true);
        self.formatted_source.push_str("function ");
        self.pop_comment(&function_definition.name, true);
        self.formatted_source
            .push_str(&function_definition.name.lexeme);
        self.pop_comment(&function_definition.left_paren, true);
        self.formatted_source.push('(');

        let mut it = function_definition.parameters.iter().peekable();

        while let Some(param) = it.next() {
            if param.required.is_some() {
                self.pop_comment(&param.required.clone().unwrap(), true);
                self.formatted_source.push_str("required ");
            }
            match &param.param_type {
                Some(param_type) => {
                    self.pop_comment(&param_type, true);
                    self.formatted_source.push_str(&param_type.lexeme);
                    self.formatted_source.push(' ');
                }
                None => {}
            }
            self.pop_comment(&param.name, true);
            self.formatted_source.push_str(&param.name.lexeme);

            match &param.default_value {
                Some(default_value) => {
                    self.pop_comment(&param.equals_token.clone().unwrap(), true);
                    self.formatted_source.push_str(" = ");
                    self.visit_expression(default_value);
                }
                None => {}
            }

            if it.peek().is_some() {
                self.formatted_source.push_str(", ");
            }
        }

        self.pop_comment(&function_definition.right_paren, true);
        self.formatted_source.push_str(") ");
        self.pop_comment(&function_definition.left_brace, true);
        self.formatted_source.push_str("{");
        self.formatted_source.push('\n');
        self.indent_level += 1;
        function_definition.body.iter().for_each(|body| {
            self.add_current_indent();
            self.visit_statement(body);
            self.formatted_source.push('\n');
        });

        self.indent_level -= 1;
        self.add_current_indent();
        self.pop_closing_comment(&function_definition.right_brace);
        self.formatted_source.push('}');
    }
    fn visit_component_definition(
        &mut self,
        component_definition: &crate::ast::ComponentDefinition,
    ) {
        self.pop_comment(&component_definition.component_token, self.inline_comment);
        self.formatted_source.push_str("component ");

        component_definition
            .attributes
            .iter()
            .for_each(|attribute| {
                self.pop_comment(&attribute.0, true);
                self.formatted_source.push_str(&attribute.0.lexeme);
                self.formatted_source.push_str("=");
                self.visit_expression(&attribute.1);
                self.formatted_source.push_str(" ");
            });

        self.pop_comment(&component_definition.left_brace, true);
        self.formatted_source.push('{');
        self.formatted_source.push('\n');

        self.indent_level += 1;
        self.formatted_source.push('\n');
        component_definition.body.iter().for_each(|body| {
            self.add_current_indent();

            self.visit_statement(body);
            self.formatted_source.push('\n');
            self.formatted_source.push('\n');
        });

        self.indent_level -= 1;
        self.add_current_indent();
        self.pop_closing_comment(&component_definition.right_brace);
        self.formatted_source.push('}');
    }
    fn visit_lucee_function(&mut self, lucee_function: &crate::ast::LuceeFunction) {
        self.pop_comment(&lucee_function.name, self.inline_comment);
        self.formatted_source.push_str(lucee_function.name.lexeme);

        lucee_function.attributes.iter().for_each(|attribute| {
            self.pop_comment(&attribute.0, true);
            self.formatted_source.push_str(&attribute.0.lexeme);
            self.formatted_source.push_str("=");
            self.visit_expression(&attribute.1);
            self.formatted_source.push_str(" ");
        });

        match &lucee_function.body {
            Some(body) => {
                if lucee_function.left_brace.is_some() {
                    self.pop_comment(
                        &lucee_function.left_brace.clone().unwrap(),
                        self.inline_comment,
                    );
                }
                self.formatted_source.push('{');
                self.formatted_source.push('\n');
                self.indent_level += 1;
                body.iter().for_each(|body| {
                    self.add_current_indent();
                    self.visit_statement(body);
                    self.formatted_source.push('\n');
                });
                self.indent_level -= 1;
                self.add_current_indent();
                if lucee_function.right_brace.is_some() {
                    self.pop_closing_comment(&lucee_function.right_brace.clone().unwrap());
                }
                self.formatted_source.push('}');
            }
            None => {}
        }

        self.formatted_source.push(';');
    }
    // TODO: Inline short single statement if statements
    fn visit_if_statement(&mut self, if_statement: &crate::ast::IfStatement) {
        self.pop_comment(&if_statement.if_token, self.inline_comment);
        self.formatted_source.push_str("if ");
        self.pop_comment(&if_statement.left_paren, true);
        self.formatted_source.push_str("(");
        self.visit_expression(&if_statement.condition);
        self.pop_comment(&if_statement.right_paren, true);
        self.formatted_source.push_str(") ");
        if if_statement.left_brace.is_some() {
            self.pop_comment(&if_statement.left_brace.clone().unwrap(), true);
        }
        self.formatted_source.push_str("{");
        self.formatted_source.push('\n');

        self.indent_level += 1;
        if_statement.body.iter().for_each(|body| {
            self.add_current_indent();
            self.visit_statement(body);
            self.formatted_source.push('\n');
        });

        self.indent_level -= 1;
        self.add_current_indent();

        if if_statement.right_brace.is_some() {
            self.pop_closing_comment(&if_statement.right_brace.clone().unwrap());
        }
        self.formatted_source.push('}');

        match &if_statement.else_body {
            Some(else_body) => {
                // Check if first else body is not if statement
                match &else_body.get(0) {
                    Some(Statement::IfStatement(if_state)) => {
                        self.formatted_source.push_str(" else ");
                        self.visit_if_statement(if_state);
                    }
                    _ => {
                        self.pop_comment(&if_statement.else_token.clone().unwrap(), true);
                        self.formatted_source.push_str(" else ");
                        if if_statement.else_left_brace.is_some() {
                            self.pop_comment(&if_statement.else_left_brace.clone().unwrap(), true);
                        }
                        self.formatted_source.push_str("{");
                        self.formatted_source.push('\n');

                        self.indent_level += 1;
                        else_body.iter().for_each(|body| {
                            self.add_current_indent();
                            self.visit_statement(body);
                            self.formatted_source.push('\n');
                        });

                        self.indent_level -= 1;
                        self.add_current_indent();

                        if if_statement.else_right_brace.is_some() {
                            self.pop_closing_comment(
                                &if_statement.else_right_brace.clone().unwrap(),
                            );
                        }
                        self.formatted_source.push('}');
                    }
                }
            }
            None => {}
        }
    }
    fn visit_for_statement(&mut self, for_statement: &crate::ast::ForStatement) {
        self.pop_comment(&for_statement.for_token, self.inline_comment);
        self.formatted_source.push_str("for ");
        self.pop_comment(&for_statement.left_paren, true);
        self.formatted_source.push_str("(");

        // Print control
        match &for_statement.control {
            ForControl::Increment {
                var_token,
                variable,
                equals_token,
                init,
                condition,
                increment,
                ..
            } => {
                if var_token.is_some() {
                    self.pop_comment(&var_token.clone().unwrap(), true);
                }
                // TODO: Option if all for loops use 'var' to declare variable
                self.pop_comment(&variable, true);
                self.formatted_source.push_str(&variable.lexeme);
                self.pop_comment(&equals_token, true);
                self.formatted_source.push_str(" = ");
                self.visit_expression(init);
                self.formatted_source.push_str("; ");
                self.visit_expression(condition);
                self.formatted_source.push_str("; ");
                self.visit_expression(increment);
            }
            ForControl::LoopOver {
                var_token,
                variable,
                in_token,
                array,
                ..
            } => {
                if var_token.is_some() {
                    self.pop_comment(&var_token.clone().unwrap(), true);
                }
                // TODO: Option if all for loops use 'var' to declare variable
                self.pop_comment(&variable, true);
                self.formatted_source.push_str(&variable.lexeme);
                self.pop_comment(&in_token, true);
                self.formatted_source.push_str(" in ");
                self.visit_expression(array);
            }
        }

        self.pop_comment(&for_statement.right_paren, true);
        self.formatted_source.push_str(") ");
        self.pop_comment(&for_statement.left_brace, true);
        self.formatted_source.push_str("{");
        self.formatted_source.push('\n');

        self.indent_level += 1;
        for_statement.body.iter().for_each(|body| {
            self.add_current_indent();
            self.visit_statement(body);
            self.formatted_source.push('\n');
        });

        self.indent_level -= 1;
        self.add_current_indent();
        self.pop_closing_comment(&for_statement.right_brace);
        self.formatted_source.push('}');
    }
    fn visit_while_statement(&mut self, while_statement: &crate::ast::WhileStatement) {
        if (while_statement.do_while) {
            if while_statement.do_token.is_some() {
                self.pop_comment(
                    &while_statement.do_token.clone().unwrap(),
                    self.inline_comment,
                );
            }
            self.formatted_source.push_str("do ");
            self.pop_comment(&while_statement.left_brace, true);
            self.formatted_source.push_str("{");
        } else {
            self.pop_comment(&while_statement.while_token, self.inline_comment);
            self.formatted_source.push_str("while (");
            self.visit_expression(&while_statement.condition);
            self.pop_comment(&while_statement.left_paren, true);
            self.formatted_source.push_str(") ");
            self.pop_comment(&while_statement.left_brace, true);
            self.formatted_source.push_str("{");
        }

        self.formatted_source.push('\n');

        self.indent_level += 1;
        while_statement.body.iter().for_each(|body| {
            self.add_current_indent();
            self.visit_statement(body);
            self.formatted_source.push('\n');
        });

        self.indent_level -= 1;
        self.add_current_indent();
        self.pop_closing_comment(&while_statement.right_brace);
        self.formatted_source.push('}');

        if while_statement.do_while {
            self.pop_comment(&while_statement.while_token, true);
            self.formatted_source.push_str(" while ");
            self.pop_comment(&while_statement.left_paren, true);
            self.formatted_source.push_str("(");
            self.visit_expression(&while_statement.condition);
            self.pop_comment(&while_statement.right_paren, true);
            self.formatted_source.push_str(")");
        }
    }
    fn visit_switch_statement(&mut self, switch_statement: &crate::ast::SwitchStatement) {
        self.pop_comment(&switch_statement.switch_token, self.inline_comment);
        self.formatted_source.push_str("switch ");
        self.pop_comment(&switch_statement.left_paren, true);
        self.formatted_source.push_str("(");
        self.visit_expression(&switch_statement.expression);
        self.pop_comment(&switch_statement.right_paren, true);
        self.formatted_source.push_str(") ");
        self.pop_comment(&switch_statement.left_brace, true);
        self.formatted_source.push_str("{");
        self.formatted_source.push('\n');

        self.indent_level += 1;
        switch_statement.cases.iter().for_each(|case| {
            self.add_current_indent();
            if case.is_default {
                let default_condition = &case.condition.get(0);
                if let Some(condition) = default_condition {
                    self.pop_comment(&condition.0, false);
                }
                self.formatted_source.push_str("default:");
                self.formatted_source.push('\n');
            } else {
                case.condition.iter().for_each(|condition| {
                    self.pop_comment(&condition.0, false);
                    self.formatted_source.push_str("case ");
                    self.visit_expression(&condition.1);
                    self.pop_comment(&condition.2, true);
                    self.formatted_source.push_str(":");
                    self.formatted_source.push('\n');
                });
            }

            self.indent_level += 1;
            case.body.iter().for_each(|body| {
                self.add_current_indent();
                self.visit_statement(body);
                self.formatted_source.push('\n');
            });
            self.indent_level -= 1;
        });

        self.indent_level -= 1;
        self.add_current_indent();
        self.pop_closing_comment(&switch_statement.right_brace);
        self.formatted_source.push('}');
    }
    fn visit_try_catch_statement(&mut self, try_catch_statement: &crate::ast::TryCatchStatement) {
        self.pop_comment(&try_catch_statement.try_token, self.inline_comment);
        self.formatted_source.push_str("try ");
        self.pop_comment(&try_catch_statement.try_left_brace, true);
        self.formatted_source.push_str("{");
        self.formatted_source.push('\n');

        self.indent_level += 1;
        try_catch_statement.try_body.iter().for_each(|body| {
            self.add_current_indent();
            self.visit_statement(body);
            self.formatted_source.push('\n');
        });

        self.indent_level -= 1;
        self.add_current_indent();
        self.pop_closing_comment(&try_catch_statement.try_right_brace);
        self.formatted_source.push_str("} ");
        self.pop_comment(&try_catch_statement.catch_token, true);
        self.formatted_source.push_str("catch ");
        self.pop_comment(&try_catch_statement.left_paren, true);
        self.formatted_source.push_str("(");

        match &try_catch_statement.catch_var_token {
            Some(var_token) => {
                self.pop_comment(var_token, true);
            }
            None => {}
        }

        match &try_catch_statement.catch_var_type {
            Some(var_type) => {
                self.visit_expression(var_type);
                self.formatted_source.push(' ');
            }
            None => {}
        }
        self.pop_comment(&try_catch_statement.catch_var, true);
        self.formatted_source
            .push_str(&try_catch_statement.catch_var.lexeme);

        self.pop_comment(&try_catch_statement.right_paren, true);
        self.formatted_source.push_str(") ");
        self.pop_comment(&try_catch_statement.catch_left_brace, true);
        self.formatted_source.push_str("{");
        self.formatted_source.push('\n');

        self.indent_level += 1;
        try_catch_statement.catch_body.iter().for_each(|body| {
            self.add_current_indent();
            self.visit_statement(body);
            self.formatted_source.push('\n');
        });

        self.indent_level -= 1;
        self.add_current_indent();

        self.pop_closing_comment(&try_catch_statement.catch_right_brace);
        self.formatted_source.push('}');
    }
}
