mod lexer;
mod parser;
mod ast;

fn main() {
    // Start time
    let start = std::time::Instant::now();
    
    // Read in test.cfm into a string
    let source = std::fs::read_to_string("test/GenericDAO.cfc").unwrap();
    
    // Create a new parser
    let mut parser = parser::Parser::new(source);
    let statements = parser.parse();

    let end = start.elapsed().as_millis();
    
    // Print out the statements
    // for statement in statements {
    //     println!("{:?}", statement);
    // }

    // Print out the time taken
    println!("Time taken: {}ms", end);
}
