use crate::ast::AST;
use crate::visitor::Visitor;

pub struct Formatter {
    pub formatted_source: String
}

impl Visitor for Formatter {

    fn visit_function_definition(&mut self, function_definition: &crate::ast::FunctionDefinition) {
        println!("Visiting Function Definition: {:?}", function_definition.name);
    }
    
}