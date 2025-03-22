use crate::lexer::Token;
use crate::visitor::Walkable;
use std::rc::Rc;

/// Barebones AST representation of a file. Aims to contain as much
/// syntax information as possible for formatting and linting reasons. Can be parsed
/// and strip out stuff for more backend processing.
///
/// Terminal AST nodes will use Tokens as much as possible to represent lexemes or straight literals, because
/// positional data is already included.
#[derive(Debug, Clone)]
pub struct AST<'ast> {
    pub source: Rc<String>,
    pub statements: Vec<Statement<'ast>>,
}

impl<'ast> Walkable for AST<'ast> {
    fn walk<V: crate::visitor::Visitor>(&self, visitor: &mut V) {
        visitor.visit(self);
    }
}

#[derive(Debug, Clone)]
pub enum Statement<'ast> {
    ExpressionStmt(Rc<Expression<'ast>>),
    VariableDeclaration(Rc<VariableDeclaration<'ast>>),
    VariableAssignment(Rc<VariableAssignment<'ast>>),
    ReturnStatement(Rc<ReturnStatement<'ast>>),
    FunctionDefinition(Rc<FunctionDefinition<'ast>>),
    ComponentDefinition(Rc<ComponentDefinition<'ast>>),
    LuceeFunction(Rc<LuceeFunction<'ast>>),
    IfStatement(Rc<IfStatement<'ast>>),
    ForStatement(Rc<ForStatement<'ast>>),
    WhileStatement(Rc<WhileStatement<'ast>>),
    SwitchStatement(Rc<SwitchStatement<'ast>>),
    TryCatchStatement(Rc<TryCatchStatement<'ast>>),
}

impl<'ast> Walkable for Statement<'ast> {
    fn walk<V: crate::visitor::Visitor>(&self, visitor: &mut V) {
        visitor.visit_statement(self);
    }
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration<'ast> {
    pub name: Token<'ast>,
    pub value: Expression<'ast>,
}

impl<'ast> Walkable for VariableDeclaration<'ast> {
    fn walk<V: crate::visitor::Visitor>(&self, visitor: &mut V) {
        visitor.visit_variable_declaration(self);
    }
}

#[derive(Debug, Clone)]
pub struct VariableAssignment<'ast> {
    pub name: Expression<'ast>,
    pub value: Expression<'ast>,
}

impl<'ast> Walkable for VariableAssignment<'ast> {
    fn walk<V: crate::visitor::Visitor>(&self, visitor: &mut V) {
        visitor.visit_variable_assignment(self);
    }
}

#[derive(Debug, Clone)]
pub struct ReturnStatement<'ast> {
    pub value: Option<Expression<'ast>>,
}

impl<'ast> Walkable for ReturnStatement<'ast> {
    fn walk<V: crate::visitor::Visitor>(&self, visitor: &mut V) {
        visitor.visit_return_statement(self);
    }
}

#[derive(Debug, Clone)]
pub struct LuceeFunction<'ast> {
    pub attributes: Vec<(Token<'ast>, Expression<'ast>)>,
    pub body: Option<Vec<Statement<'ast>>>,
}

impl<'ast> Walkable for LuceeFunction<'ast> {
    fn walk<V: crate::visitor::Visitor>(&self, visitor: &mut V) {
        visitor.visit_lucee_function(self);
    }
}

#[derive(Debug, Clone)]
pub enum Expression<'ast> {
    Literal(Rc<Literal<'ast>>),
    Identifier(Rc<String>),
    FunctionCall(Rc<FunctionCall<'ast>>),
    ObjectCreation(Rc<ObjectCreation<'ast>>),
    ArrayExpression(Rc<ArrayExpression<'ast>>),
    StructExpression(Rc<StructExpression<'ast>>),
    LambdaExpression(Rc<LambdaExpression<'ast>>),
    BinaryExpression(Rc<BinaryExpression<'ast>>),
    UnaryExpression(Rc<UnaryExpression<'ast>>),
    TernaryExpression(Rc<TernaryExpression<'ast>>),
    GroupExpression(Rc<GroupExpression<'ast>>),
    MemberAccess(Rc<MemberAccess<'ast>>),
    IndexAccess(Rc<IndexAccess<'ast>>),
    None, // Trying to consume expression but there is none. Represented by blank space
}

impl<'ast> Walkable for Expression<'ast> {
    fn walk<V: crate::visitor::Visitor>(&self, visitor: &mut V) {
        visitor.visit_expression(self);
    }
}

#[derive(Debug, Clone)]
pub struct FunctionCall<'ast> {
    pub name: Token<'ast>,
    pub args: Vec<(Option<Token<'ast>>, Expression<'ast>)>,
}

impl<'ast> Walkable for FunctionCall<'ast> {
    fn walk<V: crate::visitor::Visitor>(&self, visitor: &mut V) {
        visitor.visit_function_call(self);
    }
}

#[derive(Debug, Clone)]
pub struct ObjectCreation<'ast> {
    pub expr: Expression<'ast>,
}

impl<'ast> Walkable for ObjectCreation<'ast> {
    fn walk<V: crate::visitor::Visitor>(&self, visitor: &mut V) {
        visitor.visit_object_creation(self);
    }
}

#[derive(Debug, Clone)]
pub struct ArrayExpression<'ast> {
    pub elements: Vec<Expression<'ast>>,
}

impl<'ast> Walkable for ArrayExpression<'ast> {
    fn walk<V: crate::visitor::Visitor>(&self, visitor: &mut V) {
        visitor.visit_array_expression(self);
    }
}

#[derive(Debug, Clone)]
pub struct StructExpression<'ast> {
    pub elements: Vec<(Token<'ast>, Expression<'ast>)>,
}

impl<'ast> Walkable for StructExpression<'ast> {
    fn walk<V: crate::visitor::Visitor>(&self, visitor: &mut V) {
        visitor.visit_struct_expression(self);
    }
}

#[derive(Debug, Clone)]
pub struct LambdaExpression<'ast> {
    pub parameters: Vec<Token<'ast>>,
    pub body: Vec<Statement<'ast>>,
}

impl<'ast> Walkable for LambdaExpression<'ast> {
    fn walk<V: crate::visitor::Visitor>(&self, visitor: &mut V) {
        visitor.visit_lambda_expression(self);
    }
}

#[derive(Debug, Clone)]
pub struct BinaryExpression<'ast> {
    pub left: Box<Expression<'ast>>,
    pub op: BinaryOperator,
    pub right: Box<Expression<'ast>>,
}

impl<'ast> Walkable for BinaryExpression<'ast> {
    fn walk<V: crate::visitor::Visitor>(&self, visitor: &mut V) {
        visitor.visit_binary_expression(self);
    }
}

#[derive(Debug, Clone)]
pub struct UnaryExpression<'ast> {
    pub op: UnaryOperator,
    pub expr: Box<Expression<'ast>>,
}

impl<'ast> Walkable for UnaryExpression<'ast> {
    fn walk<V: crate::visitor::Visitor>(&self, visitor: &mut V) {
        visitor.visit_unary_expression(self);
    }
}

#[derive(Debug, Clone)]
pub struct TernaryExpression<'ast> {
    pub condition: Box<Expression<'ast>>,
    pub true_expr: Box<Expression<'ast>>,
    pub false_expr: Box<Expression<'ast>>,
}

impl<'ast> Walkable for TernaryExpression<'ast> {
    fn walk<V: crate::visitor::Visitor>(&self, visitor: &mut V) {
        visitor.visit_ternary_expression(self);
    }
}

#[derive(Debug, Clone)]
pub struct GroupExpression<'ast> {
    pub expr: Expression<'ast>,
}

impl<'ast> Walkable for GroupExpression<'ast> {
    fn walk<V: crate::visitor::Visitor>(&self, visitor: &mut V) {
        visitor.visit_group_expression(self);
    }
}

#[derive(Debug, Clone)]
pub struct MemberAccess<'ast> {
    pub object: Expression<'ast>,
    pub property: Expression<'ast>,
}

impl<'ast> Walkable for MemberAccess<'ast> {
    fn walk<V: crate::visitor::Visitor>(&self, visitor: &mut V) {
        visitor.visit_member_expression(self);
    }
}

#[derive(Debug, Clone)]
pub struct IndexAccess<'ast> {
    pub object: Expression<'ast>,
    pub index: Expression<'ast>,
}

impl<'ast> Walkable for IndexAccess<'ast> {
    fn walk<V: crate::visitor::Visitor>(&self, visitor: &mut V) {
        visitor.visit_index_access(self);
    }
}

#[derive(Debug, Clone)]
pub struct Literal<'ast> {
    pub token: Token<'ast>,
    pub value: LiteralValue,
}

#[derive(Debug, Clone)]
pub enum LiteralValue {
    Number(f64),
    String(String),
    Boolean(bool),
    Null,
}

impl<'ast> Walkable for Literal<'ast> {
    fn walk<V: crate::visitor::Visitor>(&self, visitor: &mut V) {
        visitor.visit_literal(self);
    }
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    And,
    Or,
    Xor,
    Contains,
    Eq,
    Neq,
    Lt,
    Gt,
    StringConcat,
    LogicalAnd, // Actual AND
    LogicalOr,  // Actual OR
    PlusEqual,
    DivideEqual,
    MultiplyEqual,
    MinusEqual,
    PlusPlus,
    MinusMinus,
    ConcatEqual,
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Not,
    Negate,
}

#[derive(Debug, Clone)]
pub struct FunctionDefinition<'ast> {
    pub access_modifier: Option<AccessModifier>,
    pub return_type: Option<Token<'ast>>,
    pub name: Token<'ast>,
    pub parameters: Vec<Parameter<'ast>>,
    pub body: Vec<Statement<'ast>>,
}

impl<'ast> Walkable for FunctionDefinition<'ast> {
    fn walk<V: crate::visitor::Visitor>(&self, visitor: &mut V) {
        visitor.visit_function_definition(self);
    }
}

#[derive(Debug, Clone)]
pub struct ComponentDefinition<'ast> {
    pub attributes: Vec<(Token<'ast>, Expression<'ast>)>,
    pub body: Vec<Statement<'ast>>,
}

impl<'ast> Walkable for ComponentDefinition<'ast> {
    fn walk<V: crate::visitor::Visitor>(&self, visitor: &mut V) {
        visitor.visit_component_definition(self);
    }
}

#[derive(Debug, Clone)]
pub enum AccessModifier {
    Public,
    Private,
    Protected,
}

#[derive(Debug, Clone)]
pub struct Parameter<'ast> {
    pub required: bool,
    pub param_type: Option<Token<'ast>>,
    pub name: Token<'ast>,
    pub default_value: Option<Expression<'ast>>,
}

#[derive(Debug, Clone)]
pub struct IfStatement<'ast> {
    pub condition: Expression<'ast>,
    pub body: Vec<Statement<'ast>>,
    // Potentially contains another if statement
    pub else_body: Option<Vec<Statement<'ast>>>,
}

impl<'ast> Walkable for IfStatement<'ast> {
    fn walk<V: crate::visitor::Visitor>(&self, visitor: &mut V) {
        visitor.visit_if_statement(self);
    }
}

#[derive(Debug, Clone)]
pub struct ForStatement<'ast> {
    pub control: ForControl<'ast>,
    pub body: Vec<Statement<'ast>>,
}

impl<'ast> Walkable for ForStatement<'ast> {
    fn walk<V: crate::visitor::Visitor>(&self, visitor: &mut V) {
        visitor.visit_for_statement(self);
    }
}

#[derive(Debug, Clone)]
pub struct WhileStatement<'ast> {
    pub do_while: bool, // If is in form do { } while (condition), otherwise while (condition) { }
    pub condition: Expression<'ast>,
    pub body: Vec<Statement<'ast>>,
}

impl<'ast> Walkable for WhileStatement<'ast> {
    fn walk<V: crate::visitor::Visitor>(&self, visitor: &mut V) {
        visitor.visit_while_statement(self);
    }
}

#[derive(Debug, Clone)]
pub struct SwitchStatement<'ast> {
    pub expression: Expression<'ast>,
    pub cases: Vec<CaseStatement<'ast>>,
}

impl<'ast> Walkable for SwitchStatement<'ast> {
    fn walk<V: crate::visitor::Visitor>(&self, visitor: &mut V) {
        visitor.visit_switch_statement(self);
    }
}

#[derive(Debug, Clone)]
pub struct TryCatchStatement<'ast> {
    pub try_body: Vec<Statement<'ast>>,
    pub catch_var: Token<'ast>,
    pub catch_body: Vec<Statement<'ast>>,
}

impl<'ast> Walkable for TryCatchStatement<'ast> {
    fn walk<V: crate::visitor::Visitor>(&self, visitor: &mut V) {
        visitor.visit_try_catch_statement(self);
    }
}

// Used to determine if For loop is traditional var i = 0, or for (i in array)
#[derive(Debug, Clone)]
pub enum ForControl<'ast> {
    Increment {
        init: Expression<'ast>,
        condition: Expression<'ast>,
        increment: Expression<'ast>,
    },
    LoopOver {
        variable: Token<'ast>,
        array: Expression<'ast>,
    },
}

#[derive(Debug, Clone)]
pub struct CaseStatement<'ast> {
    pub is_default: bool,
    pub condition: Option<Vec<Expression<'ast>>>,
    pub body: Vec<Statement<'ast>>,
}

#[derive(Debug, Clone)]
pub struct Comment {
    pub content: String,
}
