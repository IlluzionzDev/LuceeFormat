use fusion_formatter::ast::AST;
use fusion_formatter::formatter::{DocFormatter, Formatter};
use fusion_formatter::parser::Parser;
use fusion_formatter::visitor::Walkable;

/// Test helper for full formatting pipeline: lex -> parse -> format
pub fn format_code(input: &str) -> String {
    // Parse into AST (parser internally handles lexing)
    let mut parser = Parser::new(input);
    let ast = parser.parse();

    // Format the AST using the visitor pattern
    let mut formatter = Formatter::new();
    let doc = ast.walk(&mut formatter);

    // Render the document
    let mut doc_formatter = DocFormatter::new(80, 4); // 80 char width, 4 space indent
    doc_formatter.format(&doc)
}

/// Test helper for parsing only: lex -> parse -> return AST
pub fn parse_code(input: &str) -> AST {
    let mut parser = Parser::new(input);
    parser.parse()
}

/// Assert that code formats to expected output
pub fn assert_formats_to(input: &str, expected: &str) {
    let result = format_code(input);
    assert_eq!(result.trim(), expected.trim(), "Formatting mismatch");
}

/// Assert that code parses successfully
pub fn assert_parses_successfully(input: &str) {
    // TODO: Assert against an actual AST tree
    let _ast = parse_code(input); // Just ensure it doesn't panic
}
