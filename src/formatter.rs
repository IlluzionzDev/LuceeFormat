use crate::ast::{AccessModifier, ForControl, LiteralValue, Statement, AST};
use crate::lexer::Token;
use crate::visitor::Visitor;
use std::iter;

#[derive(Clone, Debug)]
pub enum Doc {
    // Represents string literal. No breaks can occur in the middle of the string
    Text(String),
    // Breaks new line that may be condensed (removed). Use if you want to break here if too long
    Line,
    // Always a line break
    HardLine,
    // Indents the doc
    Indent(Box<Doc>),
    // Main way to group elements together
    Group(Vec<Doc>),
    Docs(Vec<Doc>), // Just a grouping that renders it's children, doesn't do anything else
    // Space that line breaks can freely happen between, represented by " " if doesn't break
    BreakableSpace,
    Nil, // Ignored
}

impl Doc {
    pub fn width(&self) -> usize {
        match self {
            Doc::Text(text) => text.len(),
            Doc::Indent(doc) => doc.width(),
            Doc::Group(docs) => docs.iter().map(|doc| doc.width()).sum(),
            Doc::BreakableSpace => 1,
            _ => 0,
        }
    }
}

pub struct DocFormatter {
    pub max_width: usize,
    pub indent_size: usize,
    current_indent: usize,
    current_line_length: usize,
    output: String,
}

impl DocFormatter {
    pub fn new(max_width: usize, indent_size: usize) -> Self {
        DocFormatter {
            max_width,
            indent_size,
            current_indent: 0,
            current_line_length: 0,
            output: String::new(),
        }
    }

    pub fn format(&mut self, doc: &Doc) -> String {
        self.output.clear();
        self.current_line_length = 0;
        self.current_indent = 0;
        self.write_doc(doc, false);
        std::mem::take(&mut self.output)
    }

    fn write_doc(&mut self, doc: &Doc, is_flat: bool) {
        match doc {
            Doc::Text(text) => {
                self.output.push_str(text);
                self.current_line_length += text.len();
            }
            Doc::Line => {
                if !is_flat {
                    self.output.push('\n');
                    self.output
                        .extend(iter::repeat(' ').take(self.current_indent));
                    self.current_line_length = self.current_indent;
                }
            }
            Doc::HardLine => {
                self.output.push('\n');
                self.output
                    .extend(iter::repeat(' ').take(self.current_indent));
                self.current_line_length = self.current_indent;
            }
            Doc::Indent(doc) => {
                if !is_flat {
                    self.current_line_length += self.indent_size;
                    self.current_indent += self.indent_size;
                    self.output.extend(iter::repeat(' ').take(self.indent_size));
                    self.write_doc(doc, is_flat);
                    self.current_indent -= self.indent_size;
                } else {
                    self.write_doc(doc, is_flat);
                }
            }
            Doc::Docs(docs) => {
                for doc in docs {
                    self.write_doc(doc, is_flat);
                }
            }
            Doc::Group(docs) => {
                let total_length: usize = docs.iter().map(|d| d.width()).sum();

                if total_length > self.max_width {
                    for doc in docs {
                        self.write_doc(doc, false);
                    }
                } else {
                    for doc in docs {
                        self.write_doc(doc, true);
                    }
                }
            }
            Doc::BreakableSpace => {
                if !is_flat {
                    self.output.push('\n');
                    self.output
                        .extend(iter::repeat(' ').take(self.current_indent));
                    self.current_line_length = self.current_indent;
                } else {
                    self.output.push(' ');
                    self.current_line_length += 1;
                }
            }
            Doc::Nil => {}
        }
    }
}

pub struct Formatter {
    pub formatted_source: String,

    pub indent_level: usize,

    /// Indicates that we are at the beginning of a statement. Useful for expressions
    /// that may or may not be starting statements. Should be manually switched while formatting
    pub beginning_statement: bool,

    /// Flag can be set to true to ignore popping whitespace
    /// for the next call. For instance set to true when starting to print body
    /// of a function, to ensure the first statement has no whitespace before it.
    /// Once #pop_whitespace is called, this flag is reset to false.
    pub collapse_whitespace: bool,
}

impl Formatter {
    pub fn new() -> Self {
        Self {
            formatted_source: String::new(),
            indent_level: 0,
            beginning_statement: true,
            collapse_whitespace: true,
        }
    }

    fn add_current_indent(&mut self) {
        for _ in 0..self.indent_level {
            self.formatted_source.push_str("    ");
        }
    }

    /// Process lines before on a token, and prints 1 newline if there was a blank whitespace
    fn pop_whitespace(&mut self, token: &Token) -> Doc {
        let blank_lines = token.lines_before;
        let mut emit_space = false;
        if blank_lines > 0 && !self.collapse_whitespace && self.beginning_statement {
            emit_space = true;
        }
        self.collapse_whitespace = false; // Reset after printing

        if emit_space {
            Doc::HardLine
        } else {
            Doc::Nil
        }
    }

    /// Utility function for popping comments on closing braces
    /// Handles the indent since usually the indent decreases before
    /// printing the final token, but we want to comment to be printed
    /// before the indent stepdown. Without this utility function, it's
    /// harder to preserve indent.
    fn pop_closing_comment(&mut self, token: &Token) -> Doc {
        self._pop_comment(token, false, true)
    }

    /// Formats / pops comment on this token. Handles formatting indents, by stripping out newlines
    fn pop_comment(&mut self, token: &Token, inline: bool) -> Doc {
        self._pop_comment(token, inline, false)
    }

    /// Formats / pops comment on this token. Handles formatting indents, by stripping out newlines
    /// extra_indent flag for closing comments to maintain indent
    fn _pop_comment(&mut self, token: &Token, inline: bool, extra_indent: bool) -> Doc {
        let mut docs = vec![];

        if let Some(comments) = &token.comments {
            for comment in comments {
                if extra_indent {
                    let old_indent = self.indent_level;
                    self.indent_level = 1; // Reset indent for right brace comment
                    self.add_current_indent();
                    self.indent_level = old_indent; // Restore indent level
                }

                docs.push(self.pop_whitespace(comment));

                // Store formatted comment in the formatter to extend its lifetime
                let formatted_comment = self.format_comment(&comment.lexeme);

                for line in formatted_comment.lines() {
                    docs.push(Doc::Text(String::from(line.trim_end())));
                    if !inline {
                        docs.push(Doc::HardLine);
                    }
                }

                if !inline {
                    self.add_current_indent();
                } else {
                    docs.push(Doc::Text(String::from(" ")));
                    docs.push(Doc::BreakableSpace);
                }
            }
        }

        Doc::Group(docs)
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

    // TODO: Only first line is indented correctly, rest indented one too much
    // ^ I think happens when java doc comments
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

impl Visitor<Doc> for Formatter {
    fn combine_docs(&mut self, docs: &[Doc]) -> Doc {
        Doc::Group(docs.to_vec())
    }

    fn visit_statement(&mut self, statement: &Statement) -> Doc {
        // At every new statement, reset inline comment
        self.beginning_statement = true;
        self.walk_statement(statement)
    }

    fn visit_literal(&mut self, literal: &crate::ast::Literal) -> Doc {
        let mut docs = vec![];
        docs.push(self.pop_whitespace(&literal.token));
        docs.push(self.pop_comment(&literal.token, !self.beginning_statement));
        self.beginning_statement = false;

        let literal_value = &literal.value;
        let value = match literal_value {
            LiteralValue::Number(number) => number.to_string(),
            LiteralValue::String(string) => '"'.to_string() + &*string.clone() + &*'"'.to_string(),
            LiteralValue::Boolean(bool) => bool.to_string(),
            LiteralValue::Null => "null".to_string(),
        };

        docs.push(Doc::Text(value));
        Doc::Group(docs)
    }
    fn visit_identifier(&mut self, identifier: &Token) -> Doc {
        let mut docs = vec![];
        docs.push(self.pop_whitespace(identifier));
        docs.push(self.pop_comment(identifier, !self.beginning_statement));
        self.beginning_statement = false;
        docs.push(Doc::Text(identifier.lexeme.to_string()));
        Doc::Group(docs)
    }
    fn visit_function_call(&mut self, function_call: &crate::ast::FunctionCall) -> Doc {
        let mut docs = vec![];
        docs.push(self.visit_expression(&function_call.name));
        docs.push(self.pop_comment(&function_call.left_paren, true));
        docs.push(Doc::Text("(".to_string()));
        docs.push(Doc::Line);

        let mut it = function_call.args.iter().peekable();

        let mut full_arg_docs = vec![];
        while let Some(arg) = it.next() {
            let mut arg_docs = vec![];
            match &arg.0 {
                Some(token) => {
                    arg_docs.push(self.pop_comment(token, true));
                    arg_docs.push(Doc::Text(token.lexeme.to_string()));
                    arg_docs.push(Doc::Text(" = ".to_string()));
                }
                None => {}
            }

            arg_docs.push(self.visit_expression(&arg.1));

            full_arg_docs.push(Doc::Group(arg_docs));

            if it.peek().is_some() {
                full_arg_docs.push(Doc::Text(",".to_string()));
                full_arg_docs.push(Doc::BreakableSpace);
            }
        }
        docs.push(Doc::Indent(Box::new(Doc::Group(full_arg_docs))));

        docs.push(self.pop_comment(&function_call.right_paren, true));
        docs.push(Doc::Line);
        docs.push(Doc::Text(")".to_string()));

        Doc::Group(docs)
    }
    fn visit_object_creation(&mut self, object_creation: &crate::ast::ObjectCreation) -> Doc {
        let mut docs = vec![];
        docs.push(self.pop_whitespace(&object_creation.new_token));
        docs.push(self.pop_comment(&object_creation.new_token, !self.beginning_statement));
        self.beginning_statement = false;

        docs.push(Doc::Text("new ".to_string()));
        docs.push(self.visit_expression(&object_creation.expr));

        Doc::Group(docs)
    }
    fn visit_array_expression(&mut self, array_expression: &crate::ast::ArrayExpression) -> Doc {
        let mut docs = vec![];
        docs.push(self.pop_whitespace(&array_expression.left_bracket));
        docs.push(self.pop_comment(&array_expression.left_bracket, !self.beginning_statement));
        self.beginning_statement = false;

        docs.push(Doc::Text("[".to_string()));
        docs.push(Doc::Line);

        let mut it = array_expression.elements.iter().peekable();

        let mut args_docs = vec![];
        while let Some(arg) = it.next() {
            args_docs.push(self.visit_expression(arg));

            if it.peek().is_some() {
                args_docs.push(Doc::Text(",".to_string()));
                args_docs.push(Doc::BreakableSpace);
            }
        }
        docs.push(Doc::Indent(Box::new(Doc::Group(args_docs))));

        docs.push(self.pop_comment(&array_expression.right_bracket, true));
        docs.push(Doc::Line);
        docs.push(Doc::Text("]".to_string()));

        Doc::Group(docs)
    }
    fn visit_struct_expression(&mut self, struct_expression: &crate::ast::StructExpression) -> Doc {
        let mut docs = vec![];
        docs.push(self.pop_whitespace(&struct_expression.left_brace));
        docs.push(self.pop_comment(&struct_expression.left_brace, !self.beginning_statement));
        self.beginning_statement = false;

        docs.push(Doc::Text("{".to_string()));
        docs.push(Doc::BreakableSpace);

        let mut it = struct_expression.elements.iter().peekable();

        let mut body_docs = vec![];
        while let Some((key, value)) = it.next() {
            body_docs.push(self.pop_comment(&key, true));
            body_docs.push(Doc::Text(key.lexeme.to_string()));
            body_docs.push(Doc::Text(": ".to_string()));
            body_docs.push(self.visit_expression(value));

            if it.peek().is_some() {
                body_docs.push(Doc::Text(",".to_string()));
                body_docs.push(Doc::BreakableSpace);
            }
        }
        docs.push(Doc::Indent(Box::new(Doc::Group(body_docs))));

        docs.push(self.pop_closing_comment(&struct_expression.right_brace));

        docs.push(Doc::BreakableSpace);
        docs.push(Doc::Text("}".to_string()));

        Doc::Group(docs)
    }
    // TODO: Inline short lambdas where it has one statement in the body
    fn visit_lambda_expression(&mut self, lambda_expression: &crate::ast::LambdaExpression) -> Doc {
        let mut docs = vec![];
        if lambda_expression.left_paren.is_some() {
            docs.push(self.pop_whitespace(&lambda_expression.left_paren.clone().unwrap()));
            docs.push(self.pop_comment(
                &lambda_expression.left_paren.clone().unwrap(),
                !self.beginning_statement,
            ));
        }
        docs.push(Doc::Text("(".to_string()));
        docs.push(Doc::Line);

        let mut it = lambda_expression.parameters.iter().peekable();

        let mut arg_docs = vec![];
        while let Some(arg) = it.next() {
            arg_docs.push(self.pop_whitespace(&arg));
            arg_docs.push(self.pop_comment(&arg, !self.beginning_statement));
            self.beginning_statement = false;

            arg_docs.push(Doc::Text(arg.lexeme.to_string()));

            if it.peek().is_some() {
                arg_docs.push(Doc::Text(",".to_string()));
                arg_docs.push(Doc::BreakableSpace);
            }
        }
        docs.push(Doc::Indent(Box::new(Doc::Group(arg_docs))));

        if lambda_expression.right_paren.is_some() {
            docs.push(self.pop_comment(&lambda_expression.right_paren.clone().unwrap(), true));
        }
        docs.push(Doc::Text(") ".to_string()));

        docs.push(self.pop_comment(&lambda_expression.lambda_token, true));
        docs.push(Doc::Text("=> ".to_string()));

        if lambda_expression.left_brace.is_some() {
            docs.push(self.pop_comment(&lambda_expression.left_brace.clone().unwrap(), true));
        }
        docs.push(Doc::Text("{".to_string()));
        docs.push(Doc::Line);

        self.collapse_whitespace = true;
        let mut body_docs = vec![];
        lambda_expression.body.iter().for_each(|body| {
            body_docs.push(Doc::Group(vec![self.visit_statement(body), Doc::HardLine]));
        });
        docs.push(Doc::Indent(Box::new(Doc::Group(body_docs))));

        if lambda_expression.right_brace.is_some() {
            docs.push(self.pop_closing_comment(&lambda_expression.right_brace.clone().unwrap()));
        }
        docs.push(Doc::Line);
        docs.push(Doc::Text("}".to_string()));

        Doc::Group(docs)
    }
    fn visit_binary_expression(&mut self, binary_expression: &crate::ast::BinaryExpression) -> Doc {
        let mut docs = vec![];
        docs.push(self.visit_expression(&binary_expression.left));
        docs.push(Doc::Text(" ".to_string()));
        docs.push(self.pop_comment(&binary_expression.op, true));
        docs.push(Doc::Text(binary_expression.op.lexeme.to_string()));
        docs.push(Doc::Text(" ".to_string()));
        docs.push(self.visit_expression(&binary_expression.right));
        Doc::Group(docs)
    }
    fn visit_unary_expression(&mut self, unary_expression: &crate::ast::UnaryExpression) -> Doc {
        let mut docs = vec![];
        docs.push(self.pop_whitespace(&unary_expression.op));
        docs.push(self.pop_comment(&unary_expression.op, !self.beginning_statement));
        self.beginning_statement = false;

        docs.push(Doc::Text(unary_expression.op.lexeme.to_string()));
        docs.push(self.visit_expression(&unary_expression.expr));
        Doc::Group(docs)
    }
    // TODO: Handle wrapping for long chaining
    fn visit_ternary_expression(
        &mut self,
        ternary_expression: &crate::ast::TernaryExpression,
    ) -> Doc {
        let mut docs = vec![];
        docs.push(self.visit_expression(&ternary_expression.condition));
        docs.push(self.pop_comment(&ternary_expression.question_token, true));
        docs.push(Doc::Text(" ?".to_string()));
        docs.push(Doc::BreakableSpace);
        docs.push(self.visit_expression(&ternary_expression.true_expr));
        docs.push(self.pop_comment(&ternary_expression.colon_token, true));
        docs.push(Doc::Text(" :".to_string()));
        docs.push(Doc::BreakableSpace);
        docs.push(self.visit_expression(&ternary_expression.false_expr));
        Doc::Group(docs)
    }
    fn visit_group_expression(&mut self, group_expression: &crate::ast::GroupExpression) -> Doc {
        let mut docs = vec![];
        docs.push(self.pop_whitespace(&group_expression.left_paren));
        docs.push(self.pop_comment(&group_expression.left_paren, !self.beginning_statement));
        self.beginning_statement = false;

        docs.push(Doc::Text("(".to_string()));
        docs.push(Doc::Line);
        docs.push(self.visit_expression(&group_expression.expr));
        docs.push(self.pop_comment(&group_expression.right_paren, true));
        docs.push(Doc::Line);
        docs.push(Doc::Text(")".to_string()));
        Doc::Group(docs)
    }
    fn visit_member_expression(&mut self, member_expression: &crate::ast::MemberAccess) -> Doc {
        let mut docs = vec![];
        docs.push(self.visit_expression(&member_expression.object));
        docs.push(self.pop_comment(&member_expression.dot_token, true));
        docs.push(Doc::Text(".".to_string()));
        docs.push(Doc::Line);
        docs.push(self.visit_expression(&member_expression.property));
        Doc::Group(docs)
    }
    fn visit_index_access(&mut self, index_access: &crate::ast::IndexAccess) -> Doc {
        let mut docs = vec![];
        docs.push(self.visit_expression(&index_access.object));
        docs.push(self.pop_comment(&index_access.left_bracket, true));
        docs.push(Doc::Text("[".to_string()));
        docs.push(Doc::Line);
        docs.push(self.visit_expression(&index_access.index));
        docs.push(self.pop_comment(&index_access.right_bracket, true));
        docs.push(Doc::Line);
        docs.push(Doc::Text("]".to_string()));
        Doc::Group(docs)
    }

    fn visit_variable_declaration(
        &mut self,
        variable_declaration: &crate::ast::VariableDeclaration,
    ) -> Doc {
        let mut docs = vec![];
        docs.push(self.pop_whitespace(&variable_declaration.var_token));
        docs.push(self.pop_comment(&variable_declaration.var_token, !self.beginning_statement));
        self.beginning_statement = false;

        docs.push(Doc::Text("var ".to_string()));
        docs.push(self.pop_comment(&variable_declaration.name, true));
        docs.push(Doc::Text(variable_declaration.name.lexeme.to_string()));
        docs.push(self.pop_comment(&variable_declaration.equals_token, true));
        docs.push(Doc::Text(" = ".to_string()));
        docs.push(self.visit_expression(&variable_declaration.value));
        Doc::Group(docs)
    }
    fn visit_variable_assignment(
        &mut self,
        variable_assignment: &crate::ast::VariableAssignment,
    ) -> Doc {
        let mut docs = vec![];
        docs.push(self.visit_expression(&variable_assignment.name));
        docs.push(self.pop_comment(&variable_assignment.equals_token, true));
        docs.push(Doc::Text(" =".to_string()));
        docs.push(Doc::BreakableSpace);
        docs.push(self.visit_expression(&variable_assignment.value));
        Doc::Group(docs)
    }
    fn visit_return_statement(&mut self, return_statement: &crate::ast::ReturnStatement) -> Doc {
        let mut docs = vec![];
        docs.push(self.pop_whitespace(&return_statement.return_token));
        docs.push(self.pop_comment(&return_statement.return_token, !self.beginning_statement));
        self.beginning_statement = false;

        docs.push(Doc::Text("return ".to_string()));
        match &return_statement.value {
            Some(value) => docs.push(self.visit_expression(value)),
            None => {}
        }
        Doc::Group(docs)
    }
    fn visit_function_definition(
        &mut self,
        function_definition: &crate::ast::FunctionDefinition,
    ) -> Doc {
        let mut docs = vec![];
        match &function_definition.access_modifier {
            Some(access_modifier) => {
                docs.push(
                    self.pop_whitespace(
                        &function_definition.access_modifier_token.clone().unwrap(),
                    ),
                );
                docs.push(self.pop_comment(
                    &function_definition.access_modifier_token.clone().unwrap(),
                    !self.beginning_statement,
                ));
                self.beginning_statement = false;

                docs.push(Doc::Text(
                    match access_modifier {
                        AccessModifier::Public => "public",
                        AccessModifier::Private => "private",
                        AccessModifier::Protected => "protected",
                    }
                    .to_string(),
                ));
                docs.push(Doc::Text(" ".to_string()))
            }
            None => {}
        }

        match &function_definition.return_type {
            Some(return_type) => {
                docs.push(self.pop_whitespace(&function_definition.return_type.clone().unwrap()));
                docs.push(self.pop_comment(
                    &function_definition.return_type.clone().unwrap(),
                    !self.beginning_statement,
                ));
                self.beginning_statement = false;

                docs.push(Doc::Text(return_type.lexeme.to_string()));
                docs.push(Doc::Text(" ".to_string()));
            }
            None => {}
        }

        docs.push(self.pop_whitespace(&function_definition.function_token));
        docs.push(self.pop_comment(
            &function_definition.function_token,
            !self.beginning_statement,
        ));
        self.beginning_statement = false;

        docs.push(Doc::Text("function ".to_string()));
        docs.push(self.pop_comment(&function_definition.name, true));
        docs.push(Doc::Text(function_definition.name.lexeme.to_string()));
        docs.push(self.pop_comment(&function_definition.left_paren, true));
        docs.push(Doc::Text("(".to_string()));
        docs.push(Doc::Line);

        let mut it = function_definition.parameters.iter().peekable();

        let mut param_docs = vec![];
        while let Some(param) = it.next() {
            let mut param_doc = vec![];
            if param.required.is_some() {
                param_doc.push(self.pop_comment(&param.required.clone().unwrap(), true));
                param_doc.push(Doc::Text("required ".to_string()));
            }
            match &param.param_type {
                Some(param_type) => {
                    param_doc.push(self.pop_comment(&param_type, true));
                    param_doc.push(Doc::Text(param_type.lexeme.to_string()));
                    param_doc.push(Doc::Text(" ".to_string()));
                }
                None => {}
            }
            param_doc.push(self.pop_comment(&param.name, true));
            param_doc.push(Doc::Text(param.name.lexeme.to_string()));

            match &param.default_value {
                Some(default_value) => {
                    param_doc.push(self.pop_comment(&param.equals_token.clone().unwrap(), true));
                    param_doc.push(Doc::Text(" = ".to_string()));
                    param_doc.push(self.visit_expression(default_value));
                }
                None => {}
            }

            if it.peek().is_some() {
                param_doc.push(Doc::Text(",".to_string()));
                param_doc.push(Doc::BreakableSpace);
            }
            param_docs.push(Doc::Group(param_doc));
        }
        docs.push(Doc::Indent(Box::new(Doc::Group(param_docs))));

        docs.push(self.pop_comment(&function_definition.right_paren, true));
        docs.push(Doc::Text(") ".to_string()));
        docs.push(self.pop_comment(&function_definition.left_brace, true));
        docs.push(Doc::Text("{".to_string()));

        self.collapse_whitespace = true;
        let mut body_docs = vec![Doc::HardLine];
        function_definition.body.iter().for_each(|body| {
            body_docs.push(self.visit_statement(body));
            body_docs.push(Doc::HardLine);
            // body_docs.push(Doc::Group(vec![self.visit_statement(body), Doc::HardLine]));
        });
        docs.push(Doc::Indent(Box::new(Doc::Group(body_docs))));

        docs.push(self.pop_closing_comment(&function_definition.right_brace));
        docs.push(Doc::Text("}".to_string()));
        Doc::Group(docs)
    }
    fn visit_component_definition(
        &mut self,
        component_definition: &crate::ast::ComponentDefinition,
    ) -> Doc {
        let mut docs = vec![];
        docs.push(self.pop_whitespace(&component_definition.component_token));
        docs.push(self.pop_comment(
            &component_definition.component_token,
            !self.beginning_statement,
        ));
        self.beginning_statement = false;

        docs.push(Doc::Text("component ".to_string()));

        component_definition
            .attributes
            .iter()
            .for_each(|attribute| {
                docs.push(self.pop_comment(&attribute.0, true));
                docs.push(Doc::Text(attribute.0.lexeme.to_string()));
                docs.push(Doc::Text("=".to_string()));
                docs.push(self.visit_expression(&attribute.1));
                docs.push(Doc::Text(" ".to_string()));
            });

        docs.push(self.pop_comment(&component_definition.left_brace, true));
        docs.push(Doc::Text("{".to_string()));
        docs.push(Doc::HardLine);
        docs.push(Doc::HardLine);

        let mut body_docs = vec![];
        // TODO: Decide if preserve spacing or force line breaks between statements
        component_definition.body.iter().for_each(|body| {
            self.collapse_whitespace = true;

            body_docs.push(Doc::Group(vec![self.visit_statement(body), Doc::HardLine]));
        });
        docs.push(Doc::Indent(Box::new(Doc::Group(body_docs))));

        docs.push(self.pop_closing_comment(&component_definition.right_brace));
        docs.push(Doc::HardLine);
        docs.push(Doc::Text("}".to_string()));
        Doc::Group(docs)
    }
    fn visit_lucee_function(&mut self, lucee_function: &crate::ast::LuceeFunction) -> Doc {
        let mut docs = vec![];
        docs.push(self.pop_whitespace(&lucee_function.name));
        docs.push(self.pop_comment(&lucee_function.name, !self.beginning_statement));
        self.beginning_statement = false;

        docs.push(Doc::Text(lucee_function.name.lexeme.to_string()));
        docs.push(Doc::Text(" ".to_string()));

        let mut param_docs = vec![];
        lucee_function.attributes.iter().for_each(|attribute| {
            param_docs.push(self.pop_comment(&attribute.0, true));
            param_docs.push(Doc::Text(attribute.0.lexeme.to_string()));
            param_docs.push(Doc::Text("=".to_string()));
            param_docs.push(self.visit_expression(&attribute.1));
            param_docs.push(Doc::BreakableSpace);
        });
        docs.push(Doc::Group(param_docs));

        match &lucee_function.body {
            Some(body) => {
                if lucee_function.left_brace.is_some() {
                    docs.push(self.pop_comment(&lucee_function.left_brace.clone().unwrap(), true));
                }
                docs.push(Doc::Text("{".to_string()));
                docs.push(Doc::HardLine);
                self.collapse_whitespace = true;
                let mut body_docs = vec![];
                body.iter().for_each(|body| {
                    body_docs.push(Doc::Group(vec![self.visit_statement(body), Doc::HardLine]));
                });
                docs.push(Doc::Indent(Box::new(Doc::Group(body_docs))));
                if lucee_function.right_brace.is_some() {
                    docs.push(
                        self.pop_closing_comment(&lucee_function.right_brace.clone().unwrap()),
                    );
                }
                docs.push(Doc::HardLine);
                docs.push(Doc::Text("}".to_string()));
            }
            None => {}
        }

        docs.push(Doc::Text(";".to_string()));
        Doc::Group(docs)
    }
    // TODO: Inline short single statement if statements
    fn visit_if_statement(&mut self, if_statement: &crate::ast::IfStatement) -> Doc {
        let mut docs = vec![];
        docs.push(self.pop_whitespace(&if_statement.if_token));
        docs.push(self.pop_comment(&if_statement.if_token, !self.beginning_statement));
        self.beginning_statement = false;

        let mut test_group = vec![];
        test_group.push(Doc::Text("if ".to_string()));
        test_group.push(self.pop_comment(&if_statement.left_paren, true));
        test_group.push(Doc::Text("(".to_string()));
        test_group.push(Doc::Indent(Box::new(Doc::Group(vec![
            Doc::Line,
            self.visit_expression(&if_statement.condition),
            self.pop_comment(&if_statement.right_paren, true),
        ]))));
        test_group.push(Doc::Line);
        test_group.push(Doc::Text(") ".to_string()));
        if if_statement.left_brace.is_some() {
            test_group.push(self.pop_comment(&if_statement.left_brace.clone().unwrap(), true));
        }
        docs.push(Doc::Group(test_group));

        let mut full_body_docs = vec![];
        full_body_docs.push(Doc::Text("{".to_string()));

        self.collapse_whitespace = true;
        let mut body_docs = vec![];
        if if_statement.body.len() > 1 {
            body_docs.push(Doc::HardLine);
        } else {
            body_docs.push(Doc::BreakableSpace);
        }
        // If statements > 1, force line breaks
        if_statement.body.iter().for_each(|body| {
            body_docs.push(self.visit_statement(body));
            if if_statement.body.len() > 1 {
                body_docs.push(Doc::HardLine);
            } else {
                body_docs.push(Doc::Line);
            }
        });
        body_docs.remove(body_docs.len() - 1); // Remove last line break
        full_body_docs.push(Doc::Indent(Box::new(Doc::Group(body_docs))));

        if if_statement.right_brace.is_some() {
            full_body_docs
                .push(self.pop_closing_comment(&if_statement.right_brace.clone().unwrap()));
        }
        if if_statement.body.len() > 1 {
            full_body_docs.push(Doc::HardLine);
        } else {
            full_body_docs.push(Doc::BreakableSpace);
        }
        full_body_docs.push(Doc::Text("}".to_string()));

        docs.push(Doc::Group(full_body_docs));

        match &if_statement.else_body {
            Some(else_body) => {
                // Check if first else body is not if statement
                match &else_body.get(0) {
                    Some(Statement::IfStatement(if_state)) => {
                        docs.push(
                            self.pop_comment(&if_statement.else_token.clone().unwrap(), true),
                        );
                        docs.push(Doc::Text(" else ".to_string()));
                        docs.push(self.visit_if_statement(if_state));
                    }
                    _ => {
                        docs.push(
                            self.pop_comment(&if_statement.else_token.clone().unwrap(), true),
                        );
                        docs.push(Doc::Text(" else ".to_string()));
                        if if_statement.else_left_brace.is_some() {
                            docs.push(
                                self.pop_comment(
                                    &if_statement.else_left_brace.clone().unwrap(),
                                    true,
                                ),
                            );
                        }
                        let mut else_body_docs = vec![];
                        else_body_docs.push(Doc::Text("{".to_string()));
                        else_body_docs.push(Doc::Line);

                        self.collapse_whitespace = true;
                        let mut body_docs = vec![];
                        if else_body.len() > 1 {
                            body_docs.push(Doc::HardLine);
                        } else {
                            body_docs.push(Doc::BreakableSpace);
                        }
                        else_body.iter().for_each(|body| {
                            body_docs.push(self.visit_statement(body));
                            if if_statement.body.len() > 1 {
                                body_docs.push(Doc::HardLine);
                            } else {
                                body_docs.push(Doc::Line);
                            }
                        });
                        body_docs.remove(body_docs.len() - 1); // Remove last line break
                        else_body_docs.push(Doc::Indent(Box::new(Doc::Group(body_docs))));

                        if if_statement.else_right_brace.is_some() {
                            else_body_docs.push(self.pop_closing_comment(
                                &if_statement.else_right_brace.clone().unwrap(),
                            ));
                        }

                        if else_body.len() > 1 {
                            else_body_docs.push(Doc::HardLine);
                        } else {
                            else_body_docs.push(Doc::BreakableSpace);
                        }
                        else_body_docs.push(Doc::Text("}".to_string()));

                        docs.push(Doc::Group(else_body_docs));
                    }
                }
            }
            None => {}
        }

        Doc::Group(docs)
    }
    fn visit_for_statement(&mut self, for_statement: &crate::ast::ForStatement) -> Doc {
        let mut docs = vec![];
        docs.push(self.pop_whitespace(&for_statement.for_token));
        docs.push(self.pop_comment(&for_statement.for_token, !self.beginning_statement));
        self.beginning_statement = false;

        docs.push(Doc::Text("for ".to_string()));
        docs.push(self.pop_comment(&for_statement.left_paren, true));
        docs.push(Doc::Text("(".to_string()));
        docs.push(Doc::Line);

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
                let mut control_docs = vec![];
                if var_token.is_some() {
                    control_docs.push(self.pop_comment(&var_token.clone().unwrap(), true));
                }
                // TODO: Option if all for loops use 'var' to declare variable
                control_docs.push(self.pop_comment(&variable, true));
                control_docs.push(Doc::Text(variable.lexeme.to_string()));
                control_docs.push(self.pop_comment(&equals_token, true));
                control_docs.push(Doc::Text(" = ".to_string()));
                control_docs.push(self.visit_expression(init));
                control_docs.push(Doc::Text("; ".to_string()));
                control_docs.push(self.visit_expression(condition));
                control_docs.push(Doc::Text("; ".to_string()));
                control_docs.push(self.visit_expression(increment));
                docs.push(Doc::Indent(Box::new(Doc::Group(control_docs))));
            }
            ForControl::LoopOver {
                var_token,
                variable,
                in_token,
                array,
                ..
            } => {
                let mut control_docs = vec![];
                if var_token.is_some() {
                    control_docs.push(self.pop_comment(&var_token.clone().unwrap(), true));
                }
                // TODO: Option if all for loops use 'var' to declare variable
                control_docs.push(self.pop_comment(&variable, true));
                control_docs.push(Doc::Text(variable.lexeme.to_string()));
                control_docs.push(self.pop_comment(&in_token, true));
                control_docs.push(Doc::Text(" in ".to_string()));
                control_docs.push(self.visit_expression(array));
                docs.push(Doc::Indent(Box::new(Doc::Group(control_docs))));
            }
        }

        docs.push(self.pop_comment(&for_statement.right_paren, true));
        docs.push(Doc::Line);
        docs.push(Doc::Text(") ".to_string()));
        docs.push(self.pop_comment(&for_statement.left_brace, true));
        docs.push(Doc::Text("{".to_string()));
        docs.push(Doc::Line);

        self.collapse_whitespace = true;
        let mut body_docs = vec![];
        for_statement.body.iter().for_each(|body| {
            body_docs.push(Doc::Group(vec![self.visit_statement(body), Doc::HardLine]));
        });
        docs.push(Doc::Indent(Box::new(Doc::Group(body_docs))));

        docs.push(self.pop_closing_comment(&for_statement.right_brace));
        docs.push(Doc::Line);
        docs.push(Doc::Text("}".to_string()));
        Doc::Group(docs)
    }
    fn visit_while_statement(&mut self, while_statement: &crate::ast::WhileStatement) -> Doc {
        let mut docs = vec![];
        if (while_statement.do_while) {
            if while_statement.do_token.is_some() {
                docs.push(self.pop_whitespace(&while_statement.do_token.clone().unwrap()));
                docs.push(self.pop_comment(
                    &while_statement.do_token.clone().unwrap(),
                    !self.beginning_statement,
                ));
                self.beginning_statement = false;
            }
            docs.push(Doc::Text("do ".to_string()));
            docs.push(self.pop_comment(&while_statement.left_brace, true));
            docs.push(Doc::Text("{".to_string()));
        } else {
            docs.push(self.pop_whitespace(&while_statement.while_token));
            docs.push(self.pop_comment(&while_statement.while_token, !self.beginning_statement));
            self.beginning_statement = false;

            docs.push(Doc::Text("while (".to_string()));
            docs.push(Doc::Line);
            docs.push(Doc::Indent(Box::new(
                self.visit_expression(&while_statement.condition),
            )));
            docs.push(self.pop_comment(&while_statement.left_paren, true));
            docs.push(Doc::Line);
            docs.push(Doc::Text(") ".to_string()));
            docs.push(self.pop_comment(&while_statement.left_brace, true));
            docs.push(Doc::Text("{".to_string()));
        }

        docs.push(Doc::Line);

        self.collapse_whitespace = true;
        let mut body_docs = vec![];
        while_statement.body.iter().for_each(|body| {
            body_docs.push(Doc::Group(vec![self.visit_statement(body), Doc::HardLine]));
        });
        docs.push(Doc::Indent(Box::new(Doc::Group(body_docs))));

        docs.push(self.pop_closing_comment(&while_statement.right_brace));
        docs.push(Doc::Line);
        docs.push(Doc::Text("}".to_string()));

        if while_statement.do_while {
            docs.push(self.pop_comment(&while_statement.while_token, true));
            docs.push(Doc::Text(" while ".to_string()));
            docs.push(self.pop_comment(&while_statement.left_paren, true));
            docs.push(Doc::Text("(".to_string()));
            docs.push(Doc::Line);
            docs.push(Doc::Indent(Box::new(
                self.visit_expression(&while_statement.condition),
            )));
            docs.push(self.pop_comment(&while_statement.right_paren, true));
            docs.push(Doc::Line);
            docs.push(Doc::Text(")".to_string()));
        }
        Doc::Group(docs)
    }
    fn visit_switch_statement(&mut self, switch_statement: &crate::ast::SwitchStatement) -> Doc {
        let mut docs = vec![];
        docs.push(self.pop_whitespace(&switch_statement.switch_token));
        docs.push(self.pop_comment(&switch_statement.switch_token, !self.beginning_statement));
        self.beginning_statement = false;

        docs.push(Doc::Text("switch ".to_string()));
        docs.push(self.pop_comment(&switch_statement.left_paren, true));
        docs.push(Doc::Text("(".to_string()));
        docs.push(Doc::Line);
        docs.push(Doc::Indent(Box::new(
            self.visit_expression(&switch_statement.expression),
        )));
        docs.push(Doc::Line);
        docs.push(self.pop_comment(&switch_statement.right_paren, true));
        docs.push(Doc::Text(") ".to_string()));
        docs.push(self.pop_comment(&switch_statement.left_brace, true));
        docs.push(Doc::Text("{".to_string()));
        docs.push(Doc::HardLine);

        self.collapse_whitespace = true;
        let mut body_docs = vec![];
        switch_statement.cases.iter().for_each(|case| {
            body_docs.push(Doc::HardLine);
            if case.is_default {
                let default_condition = &case.condition.get(0);
                if let Some(condition) = default_condition {
                    body_docs.push(self.pop_comment(&condition.0, false));
                }
                body_docs.push(Doc::Text("default:".to_string()));
                body_docs.push(Doc::HardLine);
            } else {
                case.condition.iter().for_each(|condition| {
                    body_docs.push(self.pop_comment(&condition.0, false));
                    body_docs.push(Doc::Text("case ".to_string()));
                    body_docs.push(self.visit_expression(&condition.1));
                    body_docs.push(self.pop_comment(&condition.2, true));
                    body_docs.push(Doc::Text(":".to_string()));
                    body_docs.push(Doc::HardLine);
                });
            }

            self.collapse_whitespace = true;
            let mut inner_docs = vec![];
            case.body.iter().for_each(|body| {
                inner_docs.push(Doc::Group(vec![self.visit_statement(body), Doc::HardLine]));
            });
            body_docs.push(Doc::Indent(Box::new(Doc::Group(inner_docs))));
        });
        docs.push(Doc::Indent(Box::new(Doc::Group(body_docs))));

        docs.push(self.pop_closing_comment(&switch_statement.right_brace));
        docs.push(Doc::HardLine);
        docs.push(Doc::Text("}".to_string()));
        Doc::Group(docs)
    }
    fn visit_try_catch_statement(
        &mut self,
        try_catch_statement: &crate::ast::TryCatchStatement,
    ) -> Doc {
        let mut docs: Vec<Doc> = vec![];
        docs.push(self.pop_whitespace(&try_catch_statement.try_token));
        docs.push(self.pop_comment(&try_catch_statement.try_token, !self.beginning_statement));
        self.beginning_statement = false;

        docs.push(Doc::Text("try ".to_string()));
        docs.push(self.pop_comment(&try_catch_statement.try_left_brace, true));
        docs.push(Doc::Text("{".to_string()));
        docs.push(Doc::Line);

        self.collapse_whitespace = true;
        let mut body_docs = vec![];
        try_catch_statement.try_body.iter().for_each(|body| {
            body_docs.push(Doc::Group(vec![self.visit_statement(body), Doc::HardLine]));
        });
        docs.push(Doc::Indent(Box::new(Doc::Group(body_docs))));

        docs.push(self.pop_closing_comment(&try_catch_statement.try_right_brace));
        docs.push(Doc::Line);
        docs.push(Doc::Text("}".to_string()));
        docs.push(self.pop_comment(&try_catch_statement.catch_token, true));
        docs.push(Doc::Text("catch ".to_string()));
        docs.push(self.pop_comment(&try_catch_statement.left_paren, true));
        docs.push(Doc::Text("(".to_string()));

        match &try_catch_statement.catch_var_token {
            Some(var_token) => {
                docs.push(self.pop_comment(var_token, true));
            }
            None => {}
        }

        match &try_catch_statement.catch_var_type {
            Some(var_type) => {
                docs.push(self.visit_expression(var_type));
                docs.push(Doc::Text(" ".to_string()));
            }
            None => {}
        }
        docs.push(self.pop_comment(&try_catch_statement.catch_var, true));
        docs.push(Doc::Text(try_catch_statement.catch_var.lexeme.to_string()));

        docs.push(self.pop_comment(&try_catch_statement.right_paren, true));
        docs.push(Doc::Text(") ".to_string()));
        docs.push(self.pop_comment(&try_catch_statement.catch_left_brace, true));
        docs.push(Doc::Text("{".to_string()));
        docs.push(Doc::Line);

        self.collapse_whitespace = true;
        let mut catch_docs = vec![];
        try_catch_statement.catch_body.iter().for_each(|body| {
            catch_docs.push(Doc::Group(vec![self.visit_statement(body), Doc::HardLine]));
        });
        docs.push(Doc::Indent(Box::new(Doc::Group(catch_docs))));

        docs.push(self.pop_closing_comment(&try_catch_statement.catch_right_brace));
        docs.push(Doc::Line);
        docs.push(Doc::Text("}".to_string()));
        Doc::Group(docs)
    }
}
