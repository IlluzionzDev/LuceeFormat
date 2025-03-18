mod lexer;
mod parser;
mod ast;

fn main() {
    // Start time
    let start = std::time::Instant::now();
    
    // Read in test.cfm into a string
    let source = std::fs::read_to_string("test/GenericDAO.cfc").unwrap();
    
    // Create a new lexer
    let mut lexer = lexer::Lexer::new(source);
    
    // Tokenize the source
    lexer.scan_tokens();
    let tokens = lexer.tokens;
    
    // Print out the tokens
    // for token in tokens {
    //     println!("{:?}", token);
    // }
    
    // Create a new parser
    let mut parser = parser::Parser::new(tokens);
    let statements = parser.parse();

    // Print out the time taken
    println!("Time taken: {}ms", start.elapsed().as_millis());
    
    // Print out the statements
    for statement in statements {
        println!("{:?}", statement);
    }
}
