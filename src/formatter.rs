use crate::ast::{AccessModifier, BinaryOperator, ForControl, LiteralValue, Statement, AST};
use crate::lexer::{CommentType, Token};
use crate::visitor::Visitor;
use std::cmp::PartialEq;
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
            Doc::Docs(docs) => docs.iter().map(|doc| doc.width()).sum(),
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
                self.current_indent += self.indent_size;
                if !is_flat {
                    self.current_line_length += self.indent_size;
                    self.output.extend(iter::repeat(' ').take(self.indent_size));
                }
                self.write_doc(doc, is_flat);
                self.current_indent -= self.indent_size;
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

/// Represents different types of access operations in a chain
#[derive(Clone)]
enum AccessOperation<'ast> {
    Member {
        dot_token: crate::lexer::Token<'ast>,
        property: crate::ast::Expression<'ast>,
    },
    Index {
        left_bracket: crate::lexer::Token<'ast>,
        index: crate::ast::Expression<'ast>,
        right_bracket: crate::lexer::Token<'ast>,
    },
}

/// Represents a binary operation in a chain
#[derive(Clone)]
struct BinaryOperation<'ast> {
    operator: crate::lexer::Token<'ast>,
    right_operand: crate::ast::Expression<'ast>,
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

    /// Formats a statement body with braces, handling compact single-line vs multi-line formatting
    ///
    /// Parameters:
    /// - body: The statements to format inside the braces
    /// - left_brace: Optional left brace token for comment processing
    /// - right_brace: Optional right brace token for comment processing  
    /// - allow_compact: If true, single statement bodies use compact formatting ({ stmt })
    fn format_statement_body(
        &mut self,
        body: &[Statement],
        left_brace: Option<&Token>,
        right_brace: Option<&Token>,
        allow_compact: bool,
    ) -> Doc {
        let mut full_body_docs = vec![];
        // Add opening brace with comments
        if let Some(left_brace_token) = left_brace {
            full_body_docs.push(self.pop_comment(left_brace_token, true));
        }
        full_body_docs.push(Doc::Text("{".to_string()));
        if let Some(left_brace_token) = left_brace {
            full_body_docs.push(self.pop_trailing_comments(left_brace_token));
        }

        // Set up body formatting
        let mut body_docs = vec![];
        self.collapse_whitespace = true;

        // Determine if we should use compact formatting
        let use_compact = allow_compact && body.len() == 1;

        // Add initial spacing
        if use_compact {
            body_docs.push(Doc::BreakableSpace);
        } else {
            body_docs.push(Doc::HardLine);
        }

        // Process each statement in the body
        body.iter().for_each(|stmt| {
            body_docs.push(self.visit_statement(stmt));
            if use_compact {
                body_docs.push(Doc::Line);
            } else {
                body_docs.push(Doc::HardLine);
            }
        });

        // Remove the last line break if there are statements
        if !body_docs.is_empty() {
            // Always remove trailing HardLine to avoid extra blank lines
            if let Some(Doc::HardLine) = body_docs.last() {
                body_docs.remove(body_docs.len() - 1);
            }
        }

        // Add closing brace comments as part of the body content (after removing trailing line breaks)
        if let Some(right_brace_token) = right_brace {
            if right_brace_token.comments.is_some() {
                // Add a line break before the closing comment to separate it from the last statement
                body_docs.push(Doc::HardLine);
                let closing_comment = self.pop_closing_comment(right_brace_token);
                // Add the closing comment to the body docs so it gets proper indentation
                body_docs.push(closing_comment);
            }
        }

        // Wrap body in indented group
        full_body_docs.push(Doc::Indent(Box::new(Doc::Group(body_docs))));

        // Add spacing before closing brace
        if use_compact {
            full_body_docs.push(Doc::BreakableSpace);
        } else {
            full_body_docs.push(Doc::HardLine);
        }

        full_body_docs.push(Doc::Text("}".to_string()));

        if let Some(right_brace_token) = right_brace {
            full_body_docs.push(self.pop_trailing_comments(right_brace_token));
        }

        Doc::Group(full_body_docs)
    }

    /// Process lines before on a token, and prints 1 newline if there was a blank whitespace
    fn pop_whitespace(&mut self, token: &Token) -> Doc {
        let blank_lines = token.lines_before;
        let mut emit_space = false;

        // Only emit space if we have blank lines AND we're not collapsing whitespace AND it's a beginning statement
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

    /// Formats / pops trailing comments on this token.
    /// Should be called after the token content has been processed
    fn pop_trailing_comments(&mut self, token: &Token) -> Doc {
        let mut docs = vec![];

        if let Some(trailing_comments) = &token.trailing_comments {
            for comment in trailing_comments {
                // println!(
                //     "[Formatter] Trailing comment: {0:?} for token {1:?}",
                //     comment.token.lexeme, token
                // );
                match comment.comment_type {
                    CommentType::Trailing => {
                        // Add space before trailing comment
                        docs.push(Doc::Text(" ".to_string()));
                        let formatted_comment = self.format_comment(&comment.token.lexeme);
                        docs.push(Doc::Text(formatted_comment.trim_end().to_string()));
                        // Don't add line break for trailing comments - they stay on the same line
                    }
                    _ => {} // Only handle trailing comments here
                }
            }
        }

        if docs.is_empty() {
            Doc::Nil
        } else {
            Doc::Group(docs)
        }
    }

    /// Formats / pops comment on this token. Handles formatting indents, by stripping out newlines
    /// extra_indent flag for closing comments to maintain indent
    fn _pop_comment(&mut self, token: &Token, inline: bool, extra_indent: bool) -> Doc {
        let mut docs = vec![];

        if let Some(comments) = &token.comments {
            for (comment_idx, comment) in comments.iter().enumerate() {
                // Handle different comment types
                match comment.comment_type {
                    CommentType::Trailing => {
                        // println!("[Formatter] Trailing comment: {}", comment.token.lexeme);
                        // For trailing comments, add a single space and format inline
                        docs.push(Doc::Text(String::from(" ")));

                        // Store formatted comment in the formatter to extend its lifetime
                        let formatted_comment = self.format_comment(&comment.token.lexeme);
                        docs.push(Doc::Text(String::from(formatted_comment.trim_end())));
                        // No line breaks for trailing comments
                    }
                    CommentType::Inline => {
                        // println!("[Formatter] Inline comment: {}", comment.token.lexeme);
                        // For inline comments, add space before and after (similar to trailing but maybe different spacing)
                        docs.push(Doc::Text(String::from(" ")));
                        let formatted_comment = self.format_comment(&comment.token.lexeme);
                        docs.push(Doc::Text(String::from(formatted_comment.trim_end())));
                        docs.push(Doc::Text(String::from(" ")));
                    }
                    CommentType::Leading => {
                        // Regular leading comment handling
                        docs.push(self.pop_whitespace(&comment.token));

                        // Store formatted comment in the formatter to extend its lifetime
                        let formatted_comment = self.format_comment(&comment.token.lexeme);

                        let comment_lines: Vec<&str> = formatted_comment.lines().collect();
                        for (i, line) in comment_lines.iter().enumerate() {
                            docs.push(Doc::Text(String::from(line.trim_end())));
                            // Add HardLine after each line except for the last line of the last comment when extra_indent is true
                            let is_last_line_of_comment = i == comment_lines.len() - 1;
                            let is_last_comment = comment_idx == comments.len() - 1;

                            if !(extra_indent && is_last_line_of_comment && is_last_comment) {
                                docs.push(Doc::HardLine);
                            }
                        }
                    }
                }
            }
        }

        if docs.is_empty() {
            Doc::Nil
        } else {
            Doc::Group(docs)
        }
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

        // Detect comment type (javadoc /** vs regular /*)
        let is_javadoc = trimmed.starts_with("/**");
        let start_marker = if is_javadoc { "/**" } else { "/*" };
        
        // Use appropriate trimming based on comment type
        let content = if is_javadoc {
            trimmed.trim_start_matches("/**").trim_end_matches("*/")
        } else {
            trimmed.trim_start_matches("/*").trim_end_matches("*/")
        };

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
        result.push_str(&format!("{}\n", start_marker));

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

    /// Detects if an expression is part of an access chain (member or index)
    fn is_access_chain(expr: &crate::ast::Expression) -> bool {
        matches!(
            expr,
            crate::ast::Expression::MemberAccess(_) | crate::ast::Expression::IndexAccess(_)
        )
    }

    /// Detects if an expression is part of a binary expression chain
    fn is_binary_chain(expr: &crate::ast::Expression) -> bool {
        matches!(expr, crate::ast::Expression::BinaryExpression(_))
    }

    /// Get precedence level for binary operators to ensure only same-precedence chains
    fn get_operator_precedence(&self, op: &BinaryOperator) -> u8 {
        match op {
            BinaryOperator::LogicalOr => 1,
            BinaryOperator::LogicalAnd => 2,
            BinaryOperator::Or => 3,
            BinaryOperator::Xor => 4,
            BinaryOperator::And => 5,
            BinaryOperator::Equal
            | BinaryOperator::NotEqual
            | BinaryOperator::Eq
            | BinaryOperator::Neq => 6,
            BinaryOperator::Less
            | BinaryOperator::Greater
            | BinaryOperator::LessEqual
            | BinaryOperator::GreaterEqual
            | BinaryOperator::Lt
            | BinaryOperator::Gt => 7,
            BinaryOperator::Contains => 8,
            BinaryOperator::StringConcat => 9,
            BinaryOperator::Add | BinaryOperator::Subtract => 10,
            BinaryOperator::Multiply | BinaryOperator::Divide => 11,
            // Assignment operators and increment/decrement don't chain
            _ => 255, // Max value to prevent chaining
        }
    }

    /// Formats a unified access chain (both member and index operations) as a single group
    fn format_access_chain(&mut self, expr: &crate::ast::Expression) -> Doc {
        // Collect the entire mixed chain
        let mut operations: Vec<AccessOperation> = Vec::new();
        let mut current_expr = expr;

        // Traverse from right to left, collecting operations
        loop {
            match current_expr {
                crate::ast::Expression::MemberAccess(member_access) => {
                    operations.push(AccessOperation::Member {
                        dot_token: member_access.dot_token.clone(),
                        property: member_access.property.clone(),
                    });
                    current_expr = &member_access.object;
                }
                crate::ast::Expression::IndexAccess(index_access) => {
                    operations.push(AccessOperation::Index {
                        left_bracket: index_access.left_bracket.clone(),
                        index: index_access.index.clone(),
                        right_bracket: index_access.right_bracket.clone(),
                    });
                    current_expr = &index_access.object;
                }
                _ => {
                    // Reached base object
                    break;
                }
            }
        }

        // Reverse since we collected from right to left
        operations.reverse();

        let mut docs = Vec::new();

        // Add base object
        docs.push(self.visit_expression(current_expr));

        let mut indent_docs = vec![];

        // Add all operations with consistent breaking
        for operation in operations {
            match operation {
                AccessOperation::Member {
                    dot_token,
                    property,
                } => {
                    indent_docs.push(Doc::Line); // Break before each dot
                    indent_docs.push(self.pop_comment(&dot_token, true));
                    indent_docs.push(Doc::Text(".".to_string()));
                    indent_docs.push(self.pop_trailing_comments(&dot_token));
                    indent_docs.push(self.visit_expression(&property));
                }
                AccessOperation::Index {
                    left_bracket,
                    index,
                    right_bracket,
                } => {
                    indent_docs.push(Doc::Line); // Break before each bracket
                    indent_docs.push(self.pop_comment(&left_bracket, true));
                    indent_docs.push(Doc::Text("[".to_string()));
                    indent_docs.push(self.pop_trailing_comments(&left_bracket));
                    // indent_docs.push(Doc::Line);
                    indent_docs.push(self.visit_expression(&index));
                    indent_docs.push(self.pop_comment(&right_bracket, true));
                    // indent_docs.push(Doc::Line);
                    indent_docs.push(Doc::Text("]".to_string()));
                    indent_docs.push(self.pop_trailing_comments(&right_bracket));
                }
            }
        }

        docs.push(Doc::Indent(Box::new(Doc::Group(indent_docs))));

        Doc::Group(docs) // Single group for entire mixed chain
    }

    /// Formats a binary expression chain as a single group for consistent line breaking
    fn format_binary_chain(&mut self, expr: &crate::ast::Expression) -> Doc {
        // Collect the entire binary chain of same-precedence operations
        let mut operations: Vec<BinaryOperation> = Vec::new();
        let mut current_expr = expr;
        let mut chain_precedence: Option<u8> = None;

        // Traverse left side, collecting same-precedence operations
        loop {
            match current_expr {
                crate::ast::Expression::BinaryExpression(binary_expr) => {
                    let op_precedence =
                        self.get_operator_precedence(&binary_expr.op.to_binary_op());

                    // Initialize or check precedence consistency
                    match chain_precedence {
                        None => chain_precedence = Some(op_precedence),
                        Some(precedence) if precedence != op_precedence => break,
                        Some(_) => {} // Same precedence, continue chaining
                    }

                    // Don't chain increment/decrement operators (they're special)
                    if matches!(
                        binary_expr.op.to_binary_op(),
                        BinaryOperator::PlusPlus | BinaryOperator::MinusMinus
                    ) {
                        break;
                    }

                    operations.push(BinaryOperation {
                        operator: binary_expr.op.clone(),
                        right_operand: binary_expr.right.clone(),
                    });

                    current_expr = &binary_expr.left;
                }
                _ => {
                    // Reached base operand
                    break;
                }
            }
        }

        // If we didn't collect multiple operations, not a chain
        if operations.is_empty() {
            // Fall back to single binary expression formatting
            return self.format_single_binary_expression(expr);
        }

        // Reverse since we collected from right to left
        operations.reverse();

        let mut docs = Vec::new();

        // Add base operand (leftmost)
        docs.push(self.visit_expression(current_expr));

        let mut indent_docs = vec![];

        // Add all operations with consistent breaking
        for operation in operations {
            indent_docs.push(Doc::BreakableSpace); // Break before each operator
            indent_docs.push(self.pop_comment(&operation.operator, true));
            indent_docs.push(Doc::Text(operation.operator.lexeme.to_string()));
            indent_docs.push(self.pop_trailing_comments(&operation.operator));
            indent_docs.push(Doc::Text(" ".to_string()));
            indent_docs.push(self.visit_expression(&operation.right_operand));
        }

        docs.push(Doc::Indent(Box::new(Doc::Group(indent_docs))));

        Doc::Group(docs)
    }

    /// Formats a single binary expression (fallback when not part of chain)
    fn format_single_binary_expression(&mut self, expr: &crate::ast::Expression) -> Doc {
        if let crate::ast::Expression::BinaryExpression(binary_expr) = expr {
            let mut docs = Vec::with_capacity(7);
            if matches!(
                binary_expr.op.to_binary_op(),
                BinaryOperator::PlusPlus | BinaryOperator::MinusMinus
            ) {
                // Special handling for increment/decrement
                docs.push(self.visit_expression(&binary_expr.left));
                docs.push(self.pop_comment(&binary_expr.op, true));
                docs.push(Doc::Text(binary_expr.op.lexeme.to_string()));
                docs.push(self.pop_trailing_comments(&binary_expr.op));
            } else {
                // Normal binary expression
                docs.push(self.visit_expression(&binary_expr.left));
                docs.push(Doc::Text(" ".to_string()));
                docs.push(self.pop_comment(&binary_expr.op, true));
                docs.push(Doc::Text(binary_expr.op.lexeme.to_string()));
                docs.push(self.pop_trailing_comments(&binary_expr.op));
                docs.push(Doc::Text(" ".to_string()));
                docs.push(self.visit_expression(&binary_expr.right));
            }
            Doc::Group(docs)
        } else {
            Doc::Nil // Should not happen
        }
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
        let mut docs = Vec::with_capacity(4);
        docs.push(self.pop_comment(&literal.token, !self.beginning_statement));
        docs.push(self.pop_whitespace(&literal.token));
        self.beginning_statement = false;

        let literal_value = &literal.value;
        let value = match literal_value {
            LiteralValue::Number(number) => number.to_string(),
            LiteralValue::String(string) => '"'.to_string() + &*string.clone() + &*'"'.to_string(),
            LiteralValue::Boolean(bool) => bool.to_string(),
            LiteralValue::Null => "null".to_string(),
        };

        docs.push(Doc::Text(value));
        docs.push(self.pop_trailing_comments(&literal.token));
        Doc::Group(docs)
    }
    fn visit_identifier(&mut self, identifier: &Token) -> Doc {
        let mut docs = Vec::with_capacity(4);
        docs.push(self.pop_comment(identifier, !self.beginning_statement));
        docs.push(self.pop_whitespace(identifier));
        self.beginning_statement = false;
        docs.push(Doc::Text(identifier.lexeme.to_string()));
        docs.push(self.pop_trailing_comments(identifier));
        Doc::Group(docs)
    }
    fn visit_function_call(&mut self, function_call: &crate::ast::FunctionCall) -> Doc {
        let mut docs = Vec::with_capacity(10);
        docs.push(self.visit_expression(&function_call.name));
        docs.push(self.pop_comment(&function_call.left_paren, true));
        docs.push(Doc::Text("(".to_string()));
        docs.push(self.pop_trailing_comments(&function_call.left_paren));
        docs.push(Doc::Line);

        let mut it = function_call.args.iter().peekable();

        let mut full_arg_docs = vec![];
        while let Some(arg) = it.next() {
            let mut arg_docs = Vec::with_capacity(4);
            match &arg.0 {
                Some(token) => {
                    arg_docs.push(self.pop_comment(token, true));
                    arg_docs.push(Doc::Text(token.lexeme.to_string()));
                    arg_docs.push(self.pop_trailing_comments(token));
                    arg_docs.push(Doc::Text(" = ".to_string()));
                }
                None => {}
            }

            arg_docs.push(self.visit_expression(&arg.1));

            full_arg_docs.push(Doc::Group(arg_docs));

            if let Some(comma) = &arg.2 {
                full_arg_docs.push(self.pop_comment(comma, true));
            }
            if it.peek().is_some() {
                full_arg_docs.push(Doc::Text(",".to_string()));
            }

            if let Some(comma) = &arg.2 {
                full_arg_docs.push(self.pop_trailing_comments(comma));
            }

            if it.peek().is_some() {
                full_arg_docs.push(Doc::BreakableSpace);
            }
        }
        docs.push(Doc::Indent(Box::new(Doc::Group(full_arg_docs))));

        docs.push(self.pop_comment(&function_call.right_paren, true));
        docs.push(Doc::Line);
        docs.push(Doc::Text(")".to_string()));
        docs.push(self.pop_trailing_comments(&function_call.right_paren));

        Doc::Group(docs)
    }
    fn visit_object_creation(&mut self, object_creation: &crate::ast::ObjectCreation) -> Doc {
        let mut docs = Vec::with_capacity(5);
        docs.push(self.pop_comment(&object_creation.new_token, !self.beginning_statement));
        docs.push(self.pop_whitespace(&object_creation.new_token));
        self.beginning_statement = false;

        docs.push(Doc::Text("new ".to_string()));
        docs.push(self.pop_trailing_comments(&object_creation.new_token));
        docs.push(self.visit_expression(&object_creation.expr));

        Doc::Group(docs)
    }
    fn visit_array_expression(&mut self, array_expression: &crate::ast::ArrayExpression) -> Doc {
        let mut docs = Vec::with_capacity(10);
        docs.push(self.pop_comment(&array_expression.left_bracket, !self.beginning_statement));
        docs.push(self.pop_whitespace(&array_expression.left_bracket));
        self.beginning_statement = false;

        docs.push(Doc::Text("[".to_string()));
        docs.push(self.pop_trailing_comments(&array_expression.left_bracket));
        docs.push(Doc::Line);

        let mut it = array_expression.elements.iter().peekable();

        let mut args_docs = vec![];
        while let Some((arg, comma_token)) = it.next() {
            args_docs.push(self.visit_expression(arg));

            if let Some(comma) = comma_token {
                args_docs.push(self.pop_comment(comma, true));
            }
            if it.peek().is_some() {
                args_docs.push(Doc::Text(",".to_string()));
            }

            if let Some(comma) = comma_token {
                args_docs.push(self.pop_trailing_comments(comma));
            }

            if it.peek().is_some() {
                args_docs.push(Doc::BreakableSpace);
            }
        }
        docs.push(Doc::Indent(Box::new(Doc::Group(args_docs))));

        docs.push(self.pop_comment(&array_expression.right_bracket, true));
        docs.push(Doc::Line);
        docs.push(Doc::Text("]".to_string()));
        docs.push(self.pop_trailing_comments(&array_expression.right_bracket));

        Doc::Group(docs)
    }
    fn visit_struct_expression(&mut self, struct_expression: &crate::ast::StructExpression) -> Doc {
        let mut docs = Vec::with_capacity(9);
        docs.push(self.pop_comment(&struct_expression.left_brace, !self.beginning_statement));
        docs.push(self.pop_whitespace(&struct_expression.left_brace));
        self.beginning_statement = false;

        docs.push(Doc::Text("{".to_string()));
        docs.push(self.pop_trailing_comments(&struct_expression.left_brace));
        docs.push(Doc::BreakableSpace);

        let mut it = struct_expression.elements.iter().peekable();

        let mut body_docs = vec![];
        while let Some((key, value, comma_token)) = it.next() {
            body_docs.push(self.pop_comment(&key, true));
            body_docs.push(Doc::Text(key.lexeme.to_string()));
            body_docs.push(Doc::Text(": ".to_string()));
            body_docs.push(self.visit_expression(value));

            if let Some(comma) = comma_token {
                body_docs.push(self.pop_comment(comma, true));
            }
            if it.peek().is_some() {
                body_docs.push(Doc::Text(",".to_string()));
            }

            if let Some(comma) = comma_token {
                body_docs.push(self.pop_trailing_comments(comma));
            }

            if it.peek().is_some() {
                body_docs.push(Doc::BreakableSpace);
            }
        }

        if struct_expression.right_brace.comments.is_some() {
            // Add a line break before the closing comment to separate it from the last statement
            body_docs.push(Doc::HardLine);
            let closing_comment = self.pop_closing_comment(&struct_expression.right_brace);
            // Add the closing comment to the body docs so it gets proper indentation
            body_docs.push(closing_comment);
        }

        docs.push(Doc::Indent(Box::new(Doc::Group(body_docs))));

        docs.push(Doc::BreakableSpace);
        docs.push(Doc::Text("}".to_string()));
        docs.push(self.pop_trailing_comments(&struct_expression.right_brace));

        Doc::Group(docs)
    }
    fn visit_lambda_expression(&mut self, lambda_expression: &crate::ast::LambdaExpression) -> Doc {
        let mut docs = Vec::with_capacity(8);

        // Handle function token if present (function lambdas)
        if let Some(function_token) = &lambda_expression.function_token {
            docs.push(self.pop_comment(function_token, !self.beginning_statement));
            docs.push(self.pop_whitespace(function_token));
            self.beginning_statement = false;
            docs.push(Doc::Text("function".to_string()));
            docs.push(self.pop_trailing_comments(function_token));
        }

        if lambda_expression.left_paren.is_some() {
            docs.push(self.pop_comment(
                &lambda_expression.left_paren.clone().unwrap(),
                !self.beginning_statement,
            ));
            docs.push(self.pop_whitespace(&lambda_expression.left_paren.clone().unwrap()));
        }

        let mut full_args_docs = Vec::with_capacity(8);
        full_args_docs.push(Doc::Text("(".to_string()));
        if let Some(left_paren) = &lambda_expression.left_paren {
            full_args_docs.push(self.pop_trailing_comments(left_paren))
        }
        full_args_docs.push(Doc::Line);

        let mut it = lambda_expression.parameters.iter().peekable();

        let mut arg_docs = vec![];
        while let Some((arg, comma_token)) = it.next() {
            arg_docs.push(self.pop_comment(&arg, !self.beginning_statement));
            arg_docs.push(self.pop_whitespace(&arg));
            self.beginning_statement = false;

            arg_docs.push(Doc::Text(arg.lexeme.to_string()));

            if let Some(comma) = comma_token {
                arg_docs.push(self.pop_comment(comma, true));
            }

            if it.peek().is_some() {
                arg_docs.push(Doc::Text(",".to_string()));
            }

            if let Some(comma) = comma_token {
                arg_docs.push(self.pop_trailing_comments(comma));
            }

            if it.peek().is_some() {
                arg_docs.push(Doc::BreakableSpace);
            }
        }
        full_args_docs.push(Doc::Indent(Box::new(Doc::Group(arg_docs))));

        if lambda_expression.right_paren.is_some() {
            full_args_docs
                .push(self.pop_comment(&lambda_expression.right_paren.clone().unwrap(), true));
        }
        full_args_docs.push(Doc::Line);
        full_args_docs.push(Doc::Text(") ".to_string()));
        if let Some(right_paren) = &lambda_expression.right_paren {
            full_args_docs.push(self.pop_trailing_comments(right_paren))
        }
        docs.push(Doc::Group(full_args_docs));

        // Only render => token for regular lambdas, not function lambdas
        if lambda_expression.function_token.is_none() {
            docs.push(self.pop_comment(&lambda_expression.lambda_token, true));
            docs.push(Doc::Text("=> ".to_string()));
            docs.push(self.pop_trailing_comments(&lambda_expression.lambda_token));
        }

        docs.push(self.format_statement_body(
            &lambda_expression.body,
            lambda_expression.left_brace.as_ref(),
            lambda_expression.right_brace.as_ref(),
            true, // lambda expressions can use compact formatting
        ));

        Doc::Group(docs)
    }
    fn visit_binary_expression(&mut self, binary_expression: &crate::ast::BinaryExpression) -> Doc {
        // Check if this binary expression's left operand is part of a chain
        if Self::is_binary_chain(&binary_expression.left) {
            // This is part of a binary chain - use unified chain formatting
            return self.format_binary_chain(&crate::ast::Expression::BinaryExpression(
                std::rc::Rc::new(binary_expression.clone()),
            ));
        }

        // Single binary expression
        self.format_single_binary_expression(&crate::ast::Expression::BinaryExpression(
            std::rc::Rc::new(binary_expression.clone()),
        ))
    }
    fn visit_unary_expression(&mut self, unary_expression: &crate::ast::UnaryExpression) -> Doc {
        let mut docs = Vec::with_capacity(5);
        docs.push(self.pop_comment(&unary_expression.op, !self.beginning_statement));
        docs.push(self.pop_whitespace(&unary_expression.op));
        self.beginning_statement = false;

        docs.push(Doc::Text(unary_expression.op.lexeme.to_string()));
        docs.push(self.pop_trailing_comments(&unary_expression.op));
        docs.push(self.visit_expression(&unary_expression.expr));
        Doc::Group(docs)
    }
    fn visit_ternary_expression(
        &mut self,
        ternary_expression: &crate::ast::TernaryExpression,
    ) -> Doc {
        let mut docs = Vec::with_capacity(4);
        docs.push(self.visit_expression(&ternary_expression.condition));
        docs.push(self.pop_comment(&ternary_expression.question_token, true));
        docs.push(Doc::BreakableSpace);

        docs.push(Doc::Indent(Box::new(Doc::Group(vec![
            Doc::Text("? ".to_string()),
            self.pop_trailing_comments(&ternary_expression.question_token),
            self.visit_expression(&ternary_expression.true_expr),
            self.pop_comment(&ternary_expression.colon_token, true),
            Doc::BreakableSpace,
            Doc::Text(": ".to_string()),
            self.pop_trailing_comments(&ternary_expression.colon_token),
            self.visit_expression(&ternary_expression.false_expr),
        ]))));

        Doc::Group(docs)
    }
    fn visit_group_expression(&mut self, group_expression: &crate::ast::GroupExpression) -> Doc {
        let mut docs = Vec::with_capacity(10);
        docs.push(self.pop_comment(&group_expression.left_paren, !self.beginning_statement));
        docs.push(self.pop_whitespace(&group_expression.left_paren));
        self.beginning_statement = false;

        docs.push(Doc::Text("(".to_string()));
        docs.push(self.pop_trailing_comments(&group_expression.left_paren));
        docs.push(Doc::Line);
        docs.push(Doc::Indent(Box::new(
            self.visit_expression(&group_expression.expr),
        )));
        docs.push(self.pop_comment(&group_expression.right_paren, true));
        docs.push(Doc::Line);
        docs.push(Doc::Text(")".to_string()));
        docs.push(self.pop_trailing_comments(&group_expression.right_paren));
        Doc::Group(docs)
    }
    fn visit_member_expression(&mut self, member_expression: &crate::ast::MemberAccess) -> Doc {
        // Check if this member expression's object is part of an access chain (member or index)
        if Self::is_access_chain(&member_expression.object) {
            // This is part of a mixed access chain - use unified chain formatting
            return self.format_access_chain(&crate::ast::Expression::MemberAccess(
                std::rc::Rc::new(member_expression.clone()),
            ));
        }

        // Simple member access (not part of a chain) - use existing logic
        let mut docs = Vec::with_capacity(6);
        docs.push(self.visit_expression(&member_expression.object));
        docs.push(Doc::Line); // Break before the dot
        let mut indent_docs = vec![];
        indent_docs.push(self.pop_comment(&member_expression.dot_token, true));
        indent_docs.push(Doc::Text(".".to_string()));
        indent_docs.push(self.pop_trailing_comments(&member_expression.dot_token));
        indent_docs.push(self.visit_expression(&member_expression.property));
        docs.push(Doc::Indent(Box::new(Doc::Group(indent_docs))));
        Doc::Group(docs)
    }
    fn visit_index_access(&mut self, index_access: &crate::ast::IndexAccess) -> Doc {
        // Check if this index access's object is part of an access chain (member or index)
        if Self::is_access_chain(&index_access.object) {
            // This is part of a mixed access chain - use unified chain formatting
            return self.format_access_chain(&crate::ast::Expression::IndexAccess(
                std::rc::Rc::new(index_access.clone()),
            ));
        }

        // Simple index access (not part of a chain) - use existing logic
        let mut docs = Vec::with_capacity(10);
        docs.push(self.visit_expression(&index_access.object));
        docs.push(self.pop_comment(&index_access.left_bracket, true));
        docs.push(Doc::Text("[".to_string()));
        docs.push(self.pop_trailing_comments(&index_access.left_bracket));
        docs.push(self.visit_expression(&index_access.index));
        docs.push(self.pop_comment(&index_access.right_bracket, true));
        docs.push(Doc::Text("]".to_string()));
        docs.push(self.pop_trailing_comments(&index_access.right_bracket));
        Doc::Group(docs)
    }
    fn visit_static_access(&mut self, static_access: &crate::ast::StaticAccess) -> Doc {
        let mut docs = Vec::with_capacity(8);
        docs.push(self.pop_comment(&static_access.class_name, !self.beginning_statement));
        docs.push(self.pop_whitespace(&static_access.class_name));
        self.beginning_statement = false;
        docs.push(Doc::Text(static_access.class_name.lexeme.to_string()));
        docs.push(self.pop_trailing_comments(&static_access.class_name));
        docs.push(self.pop_comment(&static_access.colon_colon_token, true));
        docs.push(Doc::Text("::".to_string()));
        docs.push(self.pop_trailing_comments(&static_access.colon_colon_token));
        docs.push(self.visit_function_call(&static_access.function_call));
        Doc::Group(docs)
    }

    fn visit_expression_statement(
        &mut self,
        expression_statement: &crate::ast::ExpressionStatement,
    ) -> Doc {
        let mut docs = Vec::with_capacity(3);
        docs.push(self.visit_expression(&expression_statement.expression));

        // Handle trailing comments on semicolon
        if let Some(semicolon) = &expression_statement.semicolon_token {
            docs.push(self.pop_comment(semicolon, true));
            docs.push(self.pop_trailing_comments(semicolon));
        }

        Doc::Group(docs)
    }

    fn visit_variable_declaration(
        &mut self,
        variable_declaration: &crate::ast::VariableDeclaration,
    ) -> Doc {
        let mut docs = Vec::with_capacity(13);
        docs.push(self.pop_comment(&variable_declaration.var_token, !self.beginning_statement));
        docs.push(self.pop_whitespace(&variable_declaration.var_token));
        self.beginning_statement = false;

        docs.push(Doc::Text("var ".to_string()));
        docs.push(self.pop_trailing_comments(&variable_declaration.var_token));
        docs.push(self.pop_comment(&variable_declaration.name, true));
        docs.push(Doc::Text(variable_declaration.name.lexeme.to_string()));
        docs.push(self.pop_trailing_comments(&variable_declaration.name));
        docs.push(self.pop_comment(&variable_declaration.equals_token, true));
        docs.push(Doc::Text(" = ".to_string()));
        docs.push(self.pop_trailing_comments(&variable_declaration.equals_token));
        docs.push(self.visit_expression(&variable_declaration.value));

        // Handle trailing comments on semicolon
        if let Some(semicolon) = &variable_declaration.semicolon_token {
            docs.push(self.pop_comment(semicolon, true));
            docs.push(self.pop_trailing_comments(semicolon));
        }

        Doc::Group(docs)
    }
    fn visit_variable_assignment(
        &mut self,
        variable_assignment: &crate::ast::VariableAssignment,
    ) -> Doc {
        let mut docs = Vec::with_capacity(7);
        docs.push(self.visit_expression(&variable_assignment.name));
        docs.push(self.pop_comment(&variable_assignment.equals_token, true));
        docs.push(Doc::Text(" = ".to_string()));
        docs.push(self.pop_trailing_comments(&variable_assignment.equals_token));
        docs.push(self.visit_expression(&variable_assignment.value));

        // Handle trailing comments on semicolon
        if let Some(semicolon) = &variable_assignment.semicolon_token {
            docs.push(self.pop_comment(semicolon, true));
            docs.push(self.pop_trailing_comments(semicolon));
        }

        Doc::Group(docs)
    }
    fn visit_return_statement(&mut self, return_statement: &crate::ast::ReturnStatement) -> Doc {
        let mut docs = Vec::with_capacity(7);
        docs.push(self.pop_comment(&return_statement.return_token, !self.beginning_statement));
        docs.push(self.pop_whitespace(&return_statement.return_token));
        self.beginning_statement = false;

        docs.push(Doc::Text("return ".to_string()));
        docs.push(self.pop_trailing_comments(&return_statement.return_token));
        match &return_statement.value {
            Some(value) => docs.push(self.visit_expression(value)),
            None => {}
        }

        // Handle trailing comments on semicolon
        if let Some(semicolon) = &return_statement.semicolon_token {
            docs.push(self.pop_comment(semicolon, true));
            docs.push(self.pop_trailing_comments(semicolon));
        }

        Doc::Group(docs)
    }
    fn visit_function_definition(
        &mut self,
        function_definition: &crate::ast::FunctionDefinition,
    ) -> Doc {
        let mut docs = Vec::with_capacity(13);
        match &function_definition.access_modifier {
            Some(access_modifier) => {
                docs.push(self.pop_comment(
                    &function_definition.access_modifier_token.clone().unwrap(),
                    !self.beginning_statement,
                ));
                docs.push(
                    self.pop_whitespace(
                        &function_definition.access_modifier_token.clone().unwrap(),
                    ),
                );

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
                docs.push(self.pop_comment(
                    &function_definition.return_type.clone().unwrap(),
                    !self.beginning_statement,
                ));
                docs.push(self.pop_whitespace(&function_definition.return_type.clone().unwrap()));

                self.beginning_statement = false;

                docs.push(Doc::Text(return_type.lexeme.to_string()));
                docs.push(Doc::Text(" ".to_string()));
            }
            None => {}
        }

        docs.push(self.pop_comment(
            &function_definition.function_token,
            !self.beginning_statement,
        ));
        docs.push(self.pop_whitespace(&function_definition.function_token));
        self.beginning_statement = false;

        docs.push(Doc::Text("function ".to_string()));

        let mut arg_docs = Vec::with_capacity(12);

        arg_docs.push(self.pop_comment(&function_definition.name, true));
        arg_docs.push(Doc::Text(function_definition.name.lexeme.to_string()));
        arg_docs.push(self.pop_trailing_comments(&function_definition.name));
        arg_docs.push(self.pop_comment(&function_definition.left_paren, true));
        arg_docs.push(Doc::Text("(".to_string()));
        arg_docs.push(self.pop_trailing_comments(&function_definition.left_paren));
        arg_docs.push(Doc::Line);

        let mut it = function_definition.parameters.iter().peekable();

        let mut param_docs = vec![];
        while let Some((param, comma_token)) = it.next() {
            let mut param_doc = Vec::with_capacity(14);
            if param.required.is_some() {
                let required_token = param.required.as_ref().unwrap();
                param_doc.push(self.pop_comment(&required_token, true));
                param_doc.push(Doc::Text("required ".to_string()));
                param_doc.push(self.pop_trailing_comments(&required_token));
            }
            match &param.param_type {
                Some(param_type) => {
                    param_doc.push(self.pop_comment(&param_type, true));
                    param_doc.push(Doc::Text(param_type.lexeme.to_string()));
                    param_doc.push(self.pop_trailing_comments(&param_type));
                    param_doc.push(Doc::Text(" ".to_string()));
                }
                None => {}
            }
            param_doc.push(self.pop_comment(&param.name, true));
            param_doc.push(Doc::Text(param.name.lexeme.to_string()));
            param_doc.push(self.pop_trailing_comments(&param.name));

            match &param.default_value {
                Some(default_value) => {
                    param_doc.push(self.pop_comment(&param.equals_token.clone().unwrap(), true));
                    param_doc.push(Doc::Text(" = ".to_string()));
                    param_doc
                        .push(self.pop_trailing_comments(&param.equals_token.clone().unwrap()));
                    param_doc.push(self.visit_expression(default_value));
                }
                None => {}
            }

            param_docs.push(Doc::Group(param_doc));

            if let Some(comma) = comma_token {
                param_docs.push(self.pop_comment(comma, true));
            }

            if it.peek().is_some() {
                param_docs.push(Doc::Text(",".to_string()));
            }

            if let Some(comma) = comma_token {
                param_docs.push(self.pop_trailing_comments(comma));
            }

            if it.peek().is_some() {
                param_docs.push(Doc::BreakableSpace);
            }
        }
        arg_docs.push(Doc::Indent(Box::new(Doc::Group(param_docs))));

        arg_docs.push(self.pop_comment(&function_definition.right_paren, true));
        arg_docs.push(Doc::Line);
        arg_docs.push(Doc::Text(") ".to_string()));
        arg_docs.push(self.pop_trailing_comments(&function_definition.right_paren));
        docs.push(Doc::Group(arg_docs));

        docs.push(self.format_statement_body(
            &function_definition.body,
            Some(&function_definition.left_brace),
            Some(&function_definition.right_brace),
            false, // function bodies don't use compact formatting
        ));
        Doc::Group(docs)
    }

    fn visit_component_definition(
        &mut self,
        component_definition: &crate::ast::ComponentDefinition,
    ) -> Doc {
        let mut docs = Vec::with_capacity(10);
        docs.push(self.pop_comment(
            &component_definition.component_token,
            !self.beginning_statement,
        ));
        docs.push(self.pop_whitespace(&component_definition.component_token));
        self.beginning_statement = false;

        let mut name_group = Vec::with_capacity(5);
        name_group.push(Doc::Text("component".to_string()));
        name_group.push(self.pop_trailing_comments(&component_definition.component_token));
        name_group.push(Doc::BreakableSpace);

        let mut arg_docs = vec![];
        component_definition
            .attributes
            .iter()
            .for_each(|attribute| {
                arg_docs.push(self.pop_comment(&attribute.0, true));
                arg_docs.push(Doc::Text(attribute.0.lexeme.to_string()));
                arg_docs.push(self.pop_trailing_comments(&attribute.0));
                arg_docs.push(Doc::Text("=".to_string()));
                arg_docs.push(self.visit_expression(&attribute.1));
                arg_docs.push(Doc::BreakableSpace);
            });
        if !arg_docs.is_empty() {
            arg_docs.remove(arg_docs.len() - 1); // Remove last line break
            arg_docs.push(Doc::Text(" ".to_string()));
        }
        name_group.push(Doc::Indent(Box::new(Doc::Group(arg_docs))));

        name_group.push(self.pop_comment(&component_definition.left_brace, true));
        docs.push(Doc::Group(name_group));
        docs.push(Doc::Text("{".to_string()));
        docs.push(self.pop_trailing_comments(&component_definition.left_brace));

        docs.push(Doc::HardLine);

        let mut body_docs = vec![];
        component_definition.body.iter().for_each(|body| {
            body_docs.push(self.visit_statement(body));
            body_docs.push(Doc::HardLine);
        });

        if component_definition.right_brace.comments.is_some() {
            // Add a line break before the closing comment to separate it from the last statement
            body_docs.push(Doc::HardLine);
            let closing_comment = self.pop_closing_comment(&component_definition.right_brace);
            // Add the closing comment to the body docs so it gets proper indentation
            body_docs.push(closing_comment);
        }

        docs.push(Doc::Indent(Box::new(Doc::Group(body_docs))));

        docs.push(Doc::HardLine);
        docs.push(Doc::Text("}".to_string()));
        docs.push(self.pop_trailing_comments(&component_definition.right_brace));
        Doc::Group(docs)
    }
    fn visit_lucee_function(&mut self, lucee_function: &crate::ast::LuceeFunction) -> Doc {
        let mut docs = Vec::with_capacity(6);
        docs.push(self.pop_comment(&lucee_function.name, !self.beginning_statement));
        docs.push(self.pop_whitespace(&lucee_function.name));
        self.beginning_statement = false;

        let mut name_group = Vec::with_capacity(4);
        name_group.push(Doc::Text(lucee_function.name.lexeme.to_string()));
        name_group.push(self.pop_trailing_comments(&lucee_function.name));
        name_group.push(Doc::Line);

        let mut param_docs = vec![];
        lucee_function.attributes.iter().for_each(|attribute| {
            param_docs.push(self.pop_comment(&attribute.0, true));
            param_docs.push(Doc::Text(attribute.0.lexeme.to_string()));
            param_docs.push(self.pop_trailing_comments(&attribute.0));
            param_docs.push(Doc::Text("=".to_string()));
            param_docs.push(self.visit_expression(&attribute.1));
            param_docs.push(Doc::BreakableSpace);
        });
        if !param_docs.is_empty() {
            param_docs.remove(param_docs.len() - 1); // Remove last line break
            param_docs.push(Doc::Text(" ".to_string()));
        }
        name_group.push(Doc::Indent(Box::new(Doc::Group(param_docs))));
        docs.push(Doc::Group(name_group));

        match &lucee_function.body {
            Some(body) => {
                docs.push(self.format_statement_body(
                    body,
                    lucee_function.left_brace.as_ref(),
                    lucee_function.right_brace.as_ref(),
                    false, // lucee functions don't use compact formatting
                ));
            }
            None => {
                if let Some(semicolon) = &lucee_function.semicolon_token {
                    docs.push(self.pop_comment(semicolon, true));
                }
                docs.push(Doc::Text(";".to_string()));
            }
        }

        if let Some(semicolon) = &lucee_function.semicolon_token {
            docs.push(self.pop_trailing_comments(semicolon));
        }
        Doc::Group(docs)
    }
    fn visit_if_statement(&mut self, if_statement: &crate::ast::IfStatement) -> Doc {
        let mut docs = Vec::with_capacity(8);
        docs.push(self.pop_comment(&if_statement.if_token, !self.beginning_statement));
        docs.push(self.pop_whitespace(&if_statement.if_token));
        self.beginning_statement = false;

        let mut test_group = Vec::with_capacity(10);
        test_group.push(Doc::Text("if ".to_string()));
        test_group.push(self.pop_trailing_comments(&if_statement.if_token));
        test_group.push(self.pop_comment(&if_statement.left_paren, true));
        test_group.push(Doc::Text("(".to_string()));
        test_group.push(self.pop_trailing_comments(&if_statement.left_paren));
        test_group.push(Doc::Line);
        test_group.push(Doc::Indent(Box::new(Doc::Group(vec![
            self.visit_expression(&if_statement.condition),
            self.pop_comment(&if_statement.right_paren, true),
        ]))));
        test_group.push(Doc::Line);
        test_group.push(Doc::Text(") ".to_string()));
        test_group.push(self.pop_trailing_comments(&if_statement.right_paren));
        docs.push(Doc::Group(test_group));

        docs.push(self.format_statement_body(
            &if_statement.body,
            if_statement.left_brace.as_ref(),
            if_statement.right_brace.as_ref(),
            true, // allow compact formatting for single statements
        ));

        match &if_statement.else_body {
            Some(else_body) => {
                // Check if first else body is not if statement
                match &else_body.get(0) {
                    Some(Statement::IfStatement(if_state)) => {
                        docs.push(
                            self.pop_comment(&if_statement.else_token.clone().unwrap(), true),
                        );
                        docs.push(Doc::Text(" else ".to_string()));
                        docs.push(
                            self.pop_trailing_comments(&if_statement.else_token.clone().unwrap()),
                        );
                        docs.push(self.visit_if_statement(if_state));
                    }
                    _ => {
                        docs.push(
                            self.pop_comment(&if_statement.else_token.clone().unwrap(), true),
                        );
                        docs.push(Doc::Text(" else ".to_string()));
                        docs.push(
                            self.pop_trailing_comments(&if_statement.else_token.clone().unwrap()),
                        );
                        docs.push(self.format_statement_body(
                            else_body,
                            if_statement.else_left_brace.as_ref(),
                            if_statement.else_right_brace.as_ref(),
                            true, // allow compact formatting for single statements
                        ));
                    }
                }
            }
            None => {}
        }

        Doc::Group(docs)
    }
    fn visit_for_statement(&mut self, for_statement: &crate::ast::ForStatement) -> Doc {
        let mut docs = Vec::with_capacity(4);
        docs.push(self.pop_comment(&for_statement.for_token, !self.beginning_statement));
        docs.push(self.pop_whitespace(&for_statement.for_token));
        self.beginning_statement = false;

        let mut arg_docs = Vec::with_capacity(11);
        arg_docs.push(Doc::Text("for ".to_string()));
        arg_docs.push(self.pop_trailing_comments(&for_statement.for_token));
        arg_docs.push(self.pop_comment(&for_statement.left_paren, true));
        arg_docs.push(Doc::Text("(".to_string()));
        arg_docs.push(self.pop_trailing_comments(&for_statement.left_paren));
        arg_docs.push(Doc::Line);

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
                let mut control_docs = Vec::with_capacity(10);
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
                arg_docs.push(Doc::Indent(Box::new(Doc::Group(control_docs))));
            }
            ForControl::LoopOver {
                var_token,
                variable,
                in_token,
                array,
                ..
            } => {
                let mut control_docs = Vec::with_capacity(6);
                if var_token.is_some() {
                    control_docs.push(self.pop_comment(&var_token.clone().unwrap(), true));
                }
                // TODO: Option if all for loops use 'var' to declare variable
                control_docs.push(self.pop_comment(&variable, true));
                control_docs.push(Doc::Text(variable.lexeme.to_string()));
                control_docs.push(self.pop_comment(&in_token, true));
                control_docs.push(Doc::Text(" in ".to_string()));
                control_docs.push(self.visit_expression(array));
                arg_docs.push(Doc::Indent(Box::new(Doc::Group(control_docs))));
            }
        }

        arg_docs.push(self.pop_comment(&for_statement.right_paren, true));
        arg_docs.push(Doc::Line);
        arg_docs.push(Doc::Text(") ".to_string()));
        arg_docs.push(self.pop_trailing_comments(&for_statement.right_paren));
        docs.push(Doc::Group(arg_docs));

        docs.push(self.format_statement_body(
            &for_statement.body,
            Some(&for_statement.left_brace),
            Some(&for_statement.right_brace),
            false, // for loops don't use compact formatting
        ));

        Doc::Group(docs)
    }

    fn visit_while_statement(&mut self, while_statement: &crate::ast::WhileStatement) -> Doc {
        let mut docs = Vec::with_capacity(6);
        if (while_statement.do_while) {
            if while_statement.do_token.is_some() {
                docs.push(self.pop_comment(
                    &while_statement.do_token.clone().unwrap(),
                    !self.beginning_statement,
                ));
                docs.push(self.pop_whitespace(&while_statement.do_token.clone().unwrap()));
                self.beginning_statement = false;
            }
            docs.push(Doc::Text("do ".to_string()));

            if let Some(do_token) = &while_statement.do_token {
                docs.push(self.pop_trailing_comments(do_token))
            }
        } else {
            docs.push(self.pop_comment(&while_statement.while_token, !self.beginning_statement));
            docs.push(self.pop_whitespace(&while_statement.while_token));
            self.beginning_statement = false;

            let mut while_docs = Vec::with_capacity(11);
            while_docs.push(Doc::Text("while ".to_string()));
            while_docs.push(self.pop_trailing_comments(&while_statement.while_token));
            while_docs.push(self.pop_comment(&while_statement.left_paren, true));
            while_docs.push(Doc::Text("(".to_string()));
            while_docs.push(self.pop_trailing_comments(&while_statement.left_paren));
            while_docs.push(Doc::Line);
            while_docs.push(Doc::Indent(Box::new(
                self.visit_expression(&while_statement.condition),
            )));
            while_docs.push(self.pop_comment(&while_statement.right_paren, true));
            while_docs.push(Doc::Line);
            while_docs.push(Doc::Text(") ".to_string()));
            while_docs.push(self.pop_trailing_comments(&while_statement.right_paren));
            docs.push(Doc::Group(while_docs));
        }

        let body_doc = self.format_statement_body(
            &while_statement.body,
            Some(&while_statement.left_brace),
            Some(&while_statement.right_brace),
            false, // while loops don't use compact formatting
        );
        docs.push(body_doc);

        if while_statement.do_while {
            docs.push(self.pop_comment(&while_statement.while_token, true));

            let mut while_docs = Vec::with_capacity(11);
            while_docs.push(Doc::Text(" while ".to_string()));
            while_docs.push(self.pop_trailing_comments(&while_statement.while_token));
            while_docs.push(self.pop_comment(&while_statement.left_paren, true));
            while_docs.push(Doc::Text("(".to_string()));
            while_docs.push(self.pop_trailing_comments(&while_statement.left_paren));
            while_docs.push(Doc::Line);
            while_docs.push(Doc::Indent(Box::new(
                self.visit_expression(&while_statement.condition),
            )));
            while_docs.push(self.pop_comment(&while_statement.right_paren, true));
            while_docs.push(Doc::Line);
            while_docs.push(Doc::Text(") ".to_string()));
            while_docs.push(self.pop_trailing_comments(&while_statement.right_paren));
            docs.push(Doc::Group(while_docs));
        }
        Doc::Group(docs)
    }

    fn visit_switch_statement(&mut self, switch_statement: &crate::ast::SwitchStatement) -> Doc {
        let mut docs = Vec::with_capacity(4);
        docs.push(self.pop_comment(&switch_statement.switch_token, !self.beginning_statement));
        docs.push(self.pop_whitespace(&switch_statement.switch_token));
        self.beginning_statement = false;

        let mut switch_docs = Vec::with_capacity(12);
        switch_docs.push(Doc::Text("switch ".to_string()));
        switch_docs.push(self.pop_trailing_comments(&switch_statement.switch_token));
        switch_docs.push(self.pop_comment(&switch_statement.left_paren, true));
        switch_docs.push(Doc::Text("(".to_string()));
        switch_docs.push(self.pop_trailing_comments(&switch_statement.left_paren));
        switch_docs.push(Doc::Line);
        switch_docs.push(Doc::Indent(Box::new(
            self.visit_expression(&switch_statement.expression),
        )));
        switch_docs.push(Doc::Line);
        switch_docs.push(self.pop_comment(&switch_statement.right_paren, true));
        switch_docs.push(Doc::Text(") ".to_string()));
        switch_docs.push(self.pop_trailing_comments(&switch_statement.right_paren));
        switch_docs.push(self.pop_comment(&switch_statement.left_brace, true));

        docs.push(Doc::Group(switch_docs));

        let mut full_body_docs = Vec::with_capacity(6);
        full_body_docs.push(Doc::Text("{".to_string()));
        full_body_docs.push(self.pop_trailing_comments(&switch_statement.left_brace));

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

                if let Some(condition) = default_condition {
                    body_docs.push(self.pop_trailing_comments(&condition.2));
                }

                body_docs.push(Doc::HardLine);
            } else {
                case.condition.iter().for_each(|condition| {
                    body_docs.push(self.pop_comment(&condition.0, false));
                    body_docs.push(Doc::Text("case ".to_string()));
                    body_docs.push(self.pop_trailing_comments(&condition.0));
                    body_docs.push(self.visit_expression(&condition.1));
                    body_docs.push(self.pop_comment(&condition.2, true));
                    body_docs.push(Doc::Text(":".to_string()));
                    body_docs.push(self.pop_trailing_comments(&condition.2));
                    body_docs.push(Doc::HardLine);
                });
            }

            self.collapse_whitespace = true;
            let mut inner_docs = vec![];
            case.body.iter().for_each(|body| {
                inner_docs.push(self.visit_statement(body));
                inner_docs.push(Doc::HardLine);
            });
            if !inner_docs.is_empty() {
                inner_docs.remove(inner_docs.len() - 1); // Remove last line break
            }

            body_docs.push(Doc::Indent(Box::new(Doc::Group(inner_docs))));
        });

        if switch_statement.right_brace.comments.is_some() {
            // Add a line break before the closing comment to separate it from the last statement
            body_docs.push(Doc::HardLine);
            let closing_comment = self.pop_closing_comment(&switch_statement.right_brace);
            // Add the closing comment to the body docs so it gets proper indentation
            body_docs.push(closing_comment);
        }

        full_body_docs.push(Doc::Indent(Box::new(Doc::Group(body_docs))));

        full_body_docs.push(Doc::HardLine);
        full_body_docs.push(Doc::Text("}".to_string()));
        full_body_docs.push(self.pop_trailing_comments(&switch_statement.right_brace));
        docs.push(Doc::Group(full_body_docs));
        Doc::Group(docs)
    }
    fn visit_try_catch_statement(
        &mut self,
        try_catch_statement: &crate::ast::TryCatchStatement,
    ) -> Doc {
        let mut docs: Vec<Doc> = Vec::with_capacity(7);
        docs.push(self.pop_comment(&try_catch_statement.try_token, !self.beginning_statement));
        docs.push(self.pop_whitespace(&try_catch_statement.try_token));
        self.beginning_statement = false;

        docs.push(Doc::Text("try ".to_string()));
        docs.push(self.pop_trailing_comments(&try_catch_statement.try_token));

        docs.push(self.format_statement_body(
            &try_catch_statement.try_body,
            Some(&try_catch_statement.try_left_brace),
            Some(&try_catch_statement.try_right_brace),
            false, // try blocks don't use compact formatting
        ));

        let mut catch_docs = Vec::with_capacity(12);
        catch_docs.push(self.pop_comment(&try_catch_statement.catch_token, true));
        catch_docs.push(Doc::Text(" catch ".to_string()));
        catch_docs.push(self.pop_trailing_comments(&try_catch_statement.catch_token));
        catch_docs.push(self.pop_comment(&try_catch_statement.left_paren, true));
        catch_docs.push(Doc::Text("(".to_string()));
        catch_docs.push(self.pop_trailing_comments(&try_catch_statement.left_paren));
        catch_docs.push(Doc::Line);

        let mut catch_expression_docs = Vec::with_capacity(6);
        match &try_catch_statement.catch_var_token {
            Some(var_token) => {
                catch_expression_docs.push(self.pop_comment(var_token, true));
            }
            None => {}
        }

        match &try_catch_statement.catch_var_type {
            Some(var_type) => {
                catch_expression_docs.push(self.visit_expression(var_type));
                catch_expression_docs.push(Doc::Text(" ".to_string()));
            }
            None => {}
        }
        catch_expression_docs.push(self.pop_comment(&try_catch_statement.catch_var, true));
        catch_expression_docs.push(Doc::Text(try_catch_statement.catch_var.lexeme.to_string()));
        catch_expression_docs.push(self.pop_trailing_comments(&try_catch_statement.catch_var));

        catch_docs.push(Doc::Indent(Box::new(Doc::Group(catch_expression_docs))));

        catch_docs.push(self.pop_comment(&try_catch_statement.right_paren, true));
        catch_docs.push(Doc::Line);
        catch_docs.push(Doc::Text(") ".to_string()));
        catch_docs.push(self.pop_trailing_comments(&try_catch_statement.right_paren));

        docs.push(Doc::Group(catch_docs));

        docs.push(self.format_statement_body(
            &try_catch_statement.catch_body,
            Some(&try_catch_statement.catch_left_brace),
            Some(&try_catch_statement.catch_right_brace),
            false, // catch blocks don't use compact formatting
        ));
        Doc::Group(docs)
    }
}
