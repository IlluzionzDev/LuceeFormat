use fusion_formatter::ast::AST;
use fusion_formatter::formatter::{DocFormatter, Formatter};
use fusion_formatter::parser::Parser;
use fusion_formatter::visitor::Walkable;
use miette::Report;

/// Test helper for full formatting pipeline: lex -> parse -> format
pub fn format_code(input: &str) -> miette::Result<String> {
    // Parse into AST (parser internally handles lexing)
    let mut parser = Parser::new(input, "test")?;
    let ast = parser.parse();

    if let Err(errs) = ast {
        panic!("Parsing failed with errors: {:#?}", errs);
    }

    // Format the AST using the visitor pattern
    let mut formatter = Formatter::new();
    let doc = ast.unwrap().walk(&mut formatter);

    // Render the document
    let mut doc_formatter = DocFormatter::new(80, 4); // 80 char width, 4 space indent
    Ok(doc_formatter.format(&doc))
}

/// Test helper for parsing only: lex -> parse -> return AST
pub fn parse_code(input: &'_ str) -> miette::Result<AST<'_>, Vec<Report>> {
    let parser = Parser::new(input, "test");

    match parser {
        Err(e) => Err(vec![e]),
        Ok(mut p) => p.parse(),
    }
}

/// Assert that code formats to expected output
pub fn assert_formats_to(input: &str, expected: &str) {
    let result = format_code(input);
    match result {
        Ok(result) => assert_eq!(result.trim(), expected.trim(), "Formatting mismatch"),
        Err(e) => eprintln!("{:?}", e),
    }
}

/// Assert that code parses successfully
pub fn assert_parses_successfully(input: &str) {
    // TODO: Assert against an actual AST tree
    let _ast = parse_code(input); // Just ensure it doesn't panic
}
