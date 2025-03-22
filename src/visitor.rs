use crate::ast::{Expression, Statement, AST};

pub trait VisitorResult {}

/// Walkable AST Node
/// To be implemented by AST nodes that can be traversed
/// Default implementation is to walk itself and all children
pub trait Walkable {
    fn walk<V: Visitor>(&self, visitor: &mut V);
}

/**
* Visitor representation for the base AST. Default implements visiting all base statements and expressions,
* but does not visit data on the structs, that is up to the implementation. Simply visits the enum variants in the AST.
*/
pub trait Visitor {
    fn visit(&mut self, ast: &AST) {
        for statement in &ast.statements {
            self.visit_statement(statement);
        }
    }

    fn visit_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::ExpressionStmt(expression) => self.visit_expression(expression),
            Statement::VariableDeclaration(variable_declaration) => {
                self.visit_variable_declaration(variable_declaration)
            }
            Statement::VariableAssignment(variable_assignment) => {
                self.visit_variable_assignment(variable_assignment)
            }
            Statement::ReturnStatement(return_statement) => {
                self.visit_return_statement(return_statement)
            }
            Statement::FunctionDefinition(function_definition) => {
                self.visit_function_definition(function_definition)
            }
            Statement::ComponentDefinition(component_definition) => {
                self.visit_component_definition(component_definition)
            }
            Statement::LuceeFunction(lucee_function) => self.visit_lucee_function(lucee_function),
            Statement::IfStatement(if_statement) => self.visit_if_statement(if_statement),
            Statement::ForStatement(for_statement) => self.visit_for_statement(for_statement),
            Statement::WhileStatement(while_statement) => {
                self.visit_while_statement(while_statement)
            }
            Statement::SwitchStatement(switch_statement) => {
                self.visit_switch_statement(switch_statement)
            }
            Statement::TryCatchStatement(try_catch_statement) => {
                self.visit_try_catch_statement(try_catch_statement)
            }
        }
    }

    fn visit_expression(&mut self, expression: &Expression) {
        match expression {
            Expression::Literal(literal) => self.visit_literal(literal),
            Expression::Identifier(identifier) => self.visit_identifier(identifier),
            Expression::FunctionCall(function_call) => self.visit_function_call(function_call),
            Expression::ObjectCreation(object_creation) => {
                self.visit_object_creation(object_creation)
            }
            Expression::ArrayExpression(array_expression) => {
                self.visit_array_expression(array_expression)
            }
            Expression::StructExpression(struct_expression) => {
                self.visit_struct_expression(struct_expression)
            }
            Expression::LambdaExpression(lambda_expression) => {
                self.visit_lambda_expression(lambda_expression)
            }
            Expression::BinaryExpression(binary_expression) => {
                self.visit_binary_expression(binary_expression)
            }
            Expression::UnaryExpression(unary_expression) => {
                self.visit_unary_expression(unary_expression)
            }
            Expression::TernaryExpression(ternary_expression) => {
                self.visit_ternary_expression(ternary_expression)
            }
            Expression::GroupExpression(group_expression) => {
                self.visit_group_expression(group_expression)
            }
            Expression::MemberAccess(member_expression) => {
                self.visit_member_expression(member_expression)
            }
            Expression::IndexAccess(index_access) => self.visit_index_access(index_access),
            _ => {}
        }
    }

    /// Visit expressions. Don't recursively visit the expression, just the expression itself
    fn visit_literal(&mut self, literal: &crate::ast::Literal) {
    }
    fn visit_identifier(&mut self, identifier: &String) {}
    fn visit_function_call(&mut self, function_call: &crate::ast::FunctionCall) {}
    fn visit_object_creation(&mut self, object_creation: &crate::ast::ObjectCreation) {}
    fn visit_array_expression(&mut self, array_expression: &crate::ast::ArrayExpression) {}
    fn visit_struct_expression(&mut self, struct_expression: &crate::ast::StructExpression) {}
    fn visit_lambda_expression(&mut self, lambda_expression: &crate::ast::LambdaExpression) {}
    fn visit_binary_expression(&mut self, binary_expression: &crate::ast::BinaryExpression) {}
    fn visit_unary_expression(&mut self, unary_expression: &crate::ast::UnaryExpression) {}
    fn visit_ternary_expression(&mut self, ternary_expression: &crate::ast::TernaryExpression) {}
    fn visit_group_expression(&mut self, group_expression: &crate::ast::GroupExpression) {}
    fn visit_member_expression(&mut self, member_expression: &crate::ast::MemberAccess) {}
    fn visit_index_access(&mut self, index_access: &crate::ast::IndexAccess) {}

    fn visit_variable_declaration(
        &mut self,
        variable_declaration: &crate::ast::VariableDeclaration,
    ) {
    }
    fn visit_variable_assignment(&mut self, variable_assignment: &crate::ast::VariableAssignment) {
    }
    fn visit_return_statement(&mut self, return_statement: &crate::ast::ReturnStatement) {}
    fn visit_function_definition(&mut self, function_definition: &crate::ast::FunctionDefinition) {
    }
    fn visit_component_definition(
        &mut self,
        component_definition: &crate::ast::ComponentDefinition,
    ) {
        component_definition.body.iter().for_each(|statement| { self.visit_statement(statement); });
    }
    fn visit_lucee_function(&mut self, lucee_function: &crate::ast::LuceeFunction) {}
    fn visit_if_statement(&mut self, if_statement: &crate::ast::IfStatement) {}
    fn visit_for_statement(&mut self, for_statement: &crate::ast::ForStatement) {}
    fn visit_while_statement(&mut self, while_statement: &crate::ast::WhileStatement) {}
    fn visit_switch_statement(&mut self, switch_statement: &crate::ast::SwitchStatement) {}
    fn visit_try_catch_statement(&mut self, try_catch_statement: &crate::ast::TryCatchStatement) {}
}
