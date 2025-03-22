#![feature(associated_type_defaults)]

use crate::visitor::Walkable;

mod lexer;
mod parser;
mod ast;
mod visitor;
mod formatter;

fn main() {
    // Start time
    let start_file = std::time::Instant::now();
    
    // Read in test.cfm into a string
    let source = std::fs::read_to_string("test/GenericDAO.cfc").unwrap();
    
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
        formatted_source: String::new()
    };
    ast.walk(&mut formatter);
    let traverse_end = traverse_start.elapsed().as_micros();

    // Print out the time taken
    println!("File Open Time taken: {}us", end_file);
    println!("Lex Time taken: {}us", lex_time);
    println!("Parse Time taken: {}us", parse_time);
    println!("Traverse Time taken: {}us", traverse_end);
}
