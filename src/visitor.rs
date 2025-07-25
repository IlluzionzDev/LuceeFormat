use crate::ast::{Expression, ForControl, Statement, AST};
use crate::lexer::Token;

pub trait VisitorResult {}

/// Walkable AST Node
/// To be implemented by AST nodes that can be traversed
/// Default implementation is to walk itself and all children
pub trait Walkable<T> {
    fn walk<V: Visitor<T>>(&self, visitor: &mut V);
}

/**
* Visitor representation for the base AST. Default implements visiting all base statements and expressions,
* but does not visit data on the structs, that is up to the implementation. Simply visits the enum variants in the AST.
*/
pub trait Visitor<T> {
    fn visit(&mut self, ast: &AST) -> T {
        let mut docs = Vec::new();
        for statement in &ast.statements {
            docs.push(self.visit_statement(statement));
        }
        self.combine_docs(&mut docs)
    }

    /// Util to combine return types into one. To be implemented by the visitor.
    /// Used so visitors can partially implement visitor, but also define return types for visitors
    fn combine_docs(&mut self, docs: &mut Vec<T>) -> T;

    fn visit_statement(&mut self, statement: &Statement) -> T {
        self.walk_statement(statement)
    }

    fn visit_expression(&mut self, expression: &Expression) -> T {
        self.walk_expression(expression)
    }

    /// Visit expressions. Don't recursively visit the expression, just the expression itself
    fn visit_literal(&mut self, literal: &crate::ast::Literal) -> T {
        self.walk_literal(literal)
    }
    fn visit_identifier(&mut self, identifier: &Token) -> T {
        self.walk_identifier(identifier)
    }
    fn visit_function_call(&mut self, function_call: &crate::ast::FunctionCall) -> T {
        self.walk_function_call(function_call)
    }
    fn visit_object_creation(&mut self, object_creation: &crate::ast::ObjectCreation) -> T {
        self.walk_object_creation(object_creation)
    }
    fn visit_array_expression(&mut self, array_expression: &crate::ast::ArrayExpression) -> T {
        self.walk_array_expression(array_expression)
    }
    fn visit_struct_expression(&mut self, struct_expression: &crate::ast::StructExpression) -> T {
        self.walk_struct_expression(struct_expression)
    }
    fn visit_lambda_expression(&mut self, lambda_expression: &crate::ast::LambdaExpression) -> T {
        self.walk_lambda_expression(lambda_expression)
    }
    fn visit_binary_expression(&mut self, binary_expression: &crate::ast::BinaryExpression) -> T {
        self.walk_binary_expression(binary_expression)
    }
    fn visit_unary_expression(&mut self, unary_expression: &crate::ast::UnaryExpression) -> T {
        self.walk_unary_expression(unary_expression)
    }
    fn visit_ternary_expression(
        &mut self,
        ternary_expression: &crate::ast::TernaryExpression,
    ) -> T {
        self.walk_ternary_expression(ternary_expression)
    }
    fn visit_group_expression(&mut self, group_expression: &crate::ast::GroupExpression) -> T {
        self.walk_group_expression(group_expression)
    }
    fn visit_member_expression(&mut self, member_expression: &crate::ast::MemberAccess) -> T {
        self.walk_member_expression(member_expression)
    }
    fn visit_index_access(&mut self, index_access: &crate::ast::IndexAccess) -> T {
        self.walk_index_access(index_access)
    }

    fn visit_variable_declaration(
        &mut self,
        variable_declaration: &crate::ast::VariableDeclaration,
    ) -> T {
        self.walk_variable_declaration(variable_declaration)
    }
    fn visit_variable_assignment(
        &mut self,
        variable_assignment: &crate::ast::VariableAssignment,
    ) -> T {
        self.walk_variable_assignment(variable_assignment)
    }
    fn visit_return_statement(&mut self, return_statement: &crate::ast::ReturnStatement) -> T {
        self.walk_return_statement(return_statement)
    }
    fn visit_function_definition(
        &mut self,
        function_definition: &crate::ast::FunctionDefinition,
    ) -> T {
        self.walk_function_definition(function_definition)
    }
    fn visit_component_definition(
        &mut self,
        component_definition: &crate::ast::ComponentDefinition,
    ) -> T {
        self.walk_component_definition(component_definition)
    }
    fn visit_lucee_function(&mut self, lucee_function: &crate::ast::LuceeFunction) -> T {
        self.walk_lucee_function(lucee_function)
    }
    fn visit_if_statement(&mut self, if_statement: &crate::ast::IfStatement) -> T {
        self.walk_if_statement(if_statement)
    }
    fn visit_for_statement(&mut self, for_statement: &crate::ast::ForStatement) -> T {
        self.walk_for_statement(for_statement)
    }
    fn visit_while_statement(&mut self, while_statement: &crate::ast::WhileStatement) -> T {
        self.walk_while_statement(while_statement)
    }
    fn visit_switch_statement(&mut self, switch_statement: &crate::ast::SwitchStatement) -> T {
        self.walk_switch_statement(switch_statement)
    }
    fn visit_try_catch_statement(
        &mut self,
        try_catch_statement: &crate::ast::TryCatchStatement,
    ) -> T {
        self.walk_try_catch_statement(try_catch_statement)
    }

    /// Walk method stubs for each AST node
    /// Some AST nodes have direct AST node children (e.g Component has list of statements) that
    /// by default need to be walked. Some nodes are terminal or don't have easy direct children, so those
    /// walk methods are left unimplemented. Still declared for consistentecy sake but shouldn't be used. Those AST nodes
    /// are to be directly represented with the visit method and not default walked.
    ///
    /// Default implementations here are for common nodes that need walking
    fn walk_statement(&mut self, statement: &Statement) -> T {
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
    fn walk_expression(&mut self, expression: &Expression) -> T {
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
            _ => self.combine_docs(&mut vec![]),
        }
    }
    fn walk_literal(&mut self, literal: &crate::ast::Literal) -> T {
        self.combine_docs(&mut vec![])
    }
    fn walk_identifier(&mut self, identifier: &Token) -> T {
        self.combine_docs(&mut vec![])
    }
    fn walk_function_call(&mut self, function_call: &crate::ast::FunctionCall) -> T {
        self.combine_docs(&mut vec![])
    }
    fn walk_object_creation(&mut self, object_creation: &crate::ast::ObjectCreation) -> T {
        self.combine_docs(&mut vec![])
    }
    fn walk_array_expression(&mut self, array_expression: &crate::ast::ArrayExpression) -> T {
        let mut docs = vec![];
        array_expression.elements.iter().for_each(|element| {
            docs.push(self.visit_expression(element));
        });
        self.combine_docs(&mut docs)
    }
    fn walk_struct_expression(&mut self, struct_expression: &crate::ast::StructExpression) -> T {
        self.combine_docs(&mut vec![])
    }
    fn walk_lambda_expression(&mut self, lambda_expression: &crate::ast::LambdaExpression) -> T {
        let mut docs = vec![];
        lambda_expression.body.iter().for_each(|statement| {
            docs.push(self.visit_statement(statement));
        });
        self.combine_docs(&mut docs)
    }
    fn walk_binary_expression(&mut self, binary_expression: &crate::ast::BinaryExpression) -> T {
        let mut docs = vec![];
        docs.push(self.visit_expression(&binary_expression.left));
        docs.push(self.visit_expression(&binary_expression.right));
        self.combine_docs(&mut docs)
    }
    fn walk_unary_expression(&mut self, unary_expression: &crate::ast::UnaryExpression) -> T {
        self.visit_expression(&unary_expression.expr)
    }
    fn walk_ternary_expression(&mut self, ternary_expression: &crate::ast::TernaryExpression) -> T {
        let mut docs = vec![];
        docs.push(self.visit_expression(&ternary_expression.condition));
        docs.push(self.visit_expression(&ternary_expression.true_expr));
        docs.push(self.visit_expression(&ternary_expression.false_expr));
        self.combine_docs(&mut docs)
    }
    fn walk_group_expression(&mut self, group_expression: &crate::ast::GroupExpression) -> T {
        self.visit_expression(&group_expression.expr)
    }
    fn walk_member_expression(&mut self, member_expression: &crate::ast::MemberAccess) -> T {
        let mut docs = vec![];
        docs.push(self.visit_expression(&member_expression.object));
        docs.push(self.visit_expression(&member_expression.property));
        self.combine_docs(&mut docs)
    }
    fn walk_index_access(&mut self, index_access: &crate::ast::IndexAccess) -> T {
        let mut docs = Vec::new();
        docs.push(self.visit_expression(&index_access.object));
        docs.push(self.visit_expression(&index_access.index));
        self.combine_docs(&mut docs)
    }

    fn walk_variable_declaration(
        &mut self,
        variable_declaration: &crate::ast::VariableDeclaration,
    ) -> T {
        self.visit_expression(&variable_declaration.value)
    }
    fn walk_variable_assignment(
        &mut self,
        variable_assignment: &crate::ast::VariableAssignment,
    ) -> T {
        let mut docs = Vec::new();
        docs.push(self.visit_expression(&variable_assignment.name));
        docs.push(self.visit_expression(&variable_assignment.value));
        self.combine_docs(&mut docs)
    }
    fn walk_return_statement(&mut self, return_statement: &crate::ast::ReturnStatement) -> T {
        match &return_statement.value {
            Some(value) => self.visit_expression(value),
            None => self.combine_docs(&mut vec![]),
        }
    }
    fn walk_function_definition(
        &mut self,
        function_definition: &crate::ast::FunctionDefinition,
    ) -> T {
        let mut docs = Vec::new();
        function_definition.body.iter().for_each(|statement| {
            docs.push(self.visit_statement(statement));
        });
        self.combine_docs(&mut docs)
    }
    fn walk_component_definition(
        &mut self,
        component_definition: &crate::ast::ComponentDefinition,
    ) -> T {
        let mut docs = Vec::new();
        component_definition.body.iter().for_each(|statement| {
            docs.push(self.visit_statement(statement));
        });
        self.combine_docs(&mut docs)
    }
    fn walk_lucee_function(&mut self, lucee_function: &crate::ast::LuceeFunction) -> T {
        match &lucee_function.body {
            Some(body) => {
                let mut docs = Vec::new();
                body.iter().for_each(|statement| {
                    docs.push(self.visit_statement(statement));
                });
                self.combine_docs(&mut docs)
            }
            None => self.combine_docs(&mut vec![]),
        }
    }
    fn walk_if_statement(&mut self, if_statement: &crate::ast::IfStatement) -> T {
        let mut docs = Vec::new();
        docs.push(self.visit_expression(&if_statement.condition));
        if_statement.body.iter().for_each(|statement| {
            docs.push(self.visit_statement(statement));
        });
        if let Some(else_body) = &if_statement.else_body {
            else_body.iter().for_each(|statement| {
                docs.push(self.visit_statement(statement));
            });
        }
        self.combine_docs(&mut docs)
    }
    fn walk_for_statement(&mut self, for_statement: &crate::ast::ForStatement) -> T {
        let mut docs = vec![];
        match &for_statement.control {
            ForControl::Increment {
                init,
                condition,
                increment,
                ..
            } => {
                docs.push(self.visit_expression(init));
                docs.push(self.visit_expression(condition));
                docs.push(self.visit_expression(increment));
            }
            ForControl::LoopOver {
                variable, array, ..
            } => {
                docs.push(self.visit_expression(array));
            }
            _ => {}
        }
        for_statement.body.iter().for_each(|statement| {
            docs.push(self.visit_statement(statement));
        });
        self.combine_docs(&mut docs)
    }
    fn walk_while_statement(&mut self, while_statement: &crate::ast::WhileStatement) -> T {
        let mut docs = Vec::new();
        docs.push(self.visit_expression(&while_statement.condition));
        while_statement.body.iter().for_each(|statement| {
            docs.push(self.visit_statement(statement));
        });
        self.combine_docs(&mut docs)
    }
    fn walk_switch_statement(&mut self, switch_statement: &crate::ast::SwitchStatement) -> T {
        self.combine_docs(&mut vec![])
    }
    fn walk_try_catch_statement(
        &mut self,
        try_catch_statement: &crate::ast::TryCatchStatement,
    ) -> T {
        let mut docs = Vec::new();
        try_catch_statement.try_body.iter().for_each(|statement| {
            docs.push(self.visit_statement(statement));
        });
        try_catch_statement.catch_body.iter().for_each(|statement| {
            docs.push(self.visit_statement(statement));
        });
        self.combine_docs(&mut docs)
    }
}
