#![feature(associated_type_defaults)]

use crate::visitor::Walkable;

mod ast;
mod formatter;
mod lexer;
mod parser;
mod visitor;

struct Diagnostics;
impl crate::visitor::Visitor for Diagnostics {
    fn visit_function_definition(&mut self, function_definition: &crate::ast::FunctionDefinition) {
        println!(
            "Function Definition Access Modifier: {:?}",
            function_definition.access_modifier_token
        );
    }
}

fn main() {
    // Start time
    let start_file = std::time::Instant::now();

    // Read in test.cfm into a string
    let source = std::fs::read_to_string("test/test.cfc").unwrap();

    let end_file = start_file.elapsed().as_micros();

    // Create a new parser
    let mut parser = parser::Parser::new(&*source);
    let start = std::time::Instant::now();
    let ast = parser.parse();
    let end = start.elapsed().as_micros();

    let lex_time = parser.lex_time;
    let parse_time = end - lex_time;

    // Print out the statements
    // for statement in ast.statements {
    //     println!("{:?}", statement);
    // }

    let traverse_start = std::time::Instant::now();
    let mut formatter = formatter::Formatter {
        formatted_source: String::new(),
        indent_level: 0,
    };
    ast.walk(&mut formatter);
    let traverse_end = traverse_start.elapsed().as_micros();

    // Diagnostics on AST for debugging
    let mut diagnostics = Diagnostics {};
    ast.walk(&mut diagnostics);

    let write_start = std::time::Instant::now();

    // Write string to file
    std::fs::write("test/output.cfc", formatter.formatted_source).unwrap();

    let write_end = write_start.elapsed().as_micros();

    let total_time = start_file.elapsed().as_micros();

    // Print out the time taken
    println!("File Open Time taken: {}us", end_file);
    println!("Lex Time taken: {}us", lex_time);
    println!("Parse Time taken: {}us", parse_time);
    println!("Traverse Time taken: {}us", traverse_end);
    println!("Write Time taken: {}us", write_end);
    println!("Total Time taken: {}us", total_time);
}
