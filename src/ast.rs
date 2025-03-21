use crate::lexer::Token;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct AST {
    pub source: Rc<String>,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    ExpressionStmt(Rc<Expression>),
    VariableDeclaration(Rc<VariableDeclaration>),
    VariableAssignment(Rc<VariableAssignment>),
    ReturnStatement(Rc<ReturnStatement>),
    FunctionDefinition(Rc<FunctionDefinition>),
    ComponentDefinition(Rc<ComponentDefinition>),
    LuceeFunction(Rc<LuceeFunction>),
    IfStatement(Rc<IfStatement>),
    ForStatement(Rc<ForStatement>),
    WhileStatement(Rc<WhileStatement>),
    SwitchStatement(Rc<SwitchStatement>),
    TryCatchStatement(Rc<TryCatchStatement>),
    CfmlTag(Rc<CfmlTag>),
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub name: String,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct VariableAssignment {
    pub name: Expression,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub value: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct LuceeFunction {
    pub attributes: Vec<(String, Expression)>,
    pub body: Option<Vec<Statement>>,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Rc<Literal>),
    Identifier(Rc<String>),
    FunctionCall(Rc<FunctionCall>),
    ObjectCreation(Rc<ObjectCreation>),
    ArrayExpression(Rc<ArrayExpression>),
    StructExpression(Rc<StructExpression>),
    LambdaExpression(Rc<LambdaExpression>),
    BinaryExpression(Rc<BinaryExpression>),
    UnaryExpression(Rc<UnaryExpression>),
    TernaryExpression(Rc<TernaryExpression>),
    GroupExpression(Rc<GroupExpression>),
    MemberAccess(Rc<MemberAccess>),
    IndexAccess(Rc<IndexAccess>),
    None, // Trying to consume expression but there is none. Represented by blank space
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub name: String,
    pub args: Vec<(Option<String>, Expression)>,
}

#[derive(Debug, Clone)]
pub struct ObjectCreation {
    pub expr: Expression,
}

#[derive(Debug, Clone)]
pub struct ArrayExpression {
    pub elements: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct StructExpression {
    pub elements: Vec<(String, Expression)>,
}

#[derive(Debug, Clone)]
pub struct LambdaExpression {
    pub parameters: Vec<String>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct BinaryExpression {
    pub left: Box<Expression>,
    pub op: BinaryOperator,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct UnaryExpression {
    pub op: UnaryOperator,
    pub expr: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct TernaryExpression {
    pub condition: Box<Expression>,
    pub true_expr: Box<Expression>,
    pub false_expr: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct GroupExpression {
    pub expr: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct MemberAccess {
    pub object: Box<Expression>,
    pub property: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct IndexAccess {
    pub object: Box<Expression>,
    pub index: Box<Expression>,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Number(f64),
    String(String),
    Boolean(bool),
    Null,
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
pub struct FunctionDefinition {
    pub access_modifier: Option<AccessModifier>,
    pub return_type: Option<String>,
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct ComponentDefinition {
    pub attributes: Vec<(String, Expression)>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum AccessModifier {
    Public,
    Private,
    Protected,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub required: bool,
    pub param_type: Option<String>,
    pub name: String,
    pub default_value: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct IfStatement {
    pub condition: Expression,
    pub body: Vec<Statement>,
    // Potentially contains another if statement
    pub else_body: Option<Vec<Statement>>,
}

#[derive(Debug, Clone)]
pub struct ForStatement {
    pub control: ForControl,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct WhileStatement {
    pub do_while: bool, // If is in form do { } while (condition), otherwise while (condition) { }
    pub condition: Expression,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct SwitchStatement {
    pub expression: Expression,
    pub cases: Vec<CaseStatement>,
}

#[derive(Debug, Clone)]
pub struct TryCatchStatement {
    pub try_body: Vec<Statement>,
    pub catch_var: String,
    pub catch_body: Vec<Statement>,
}

// Used to determine if For loop is traditional var i = 0, or for (i in array)
#[derive(Debug, Clone)]
pub enum ForControl {
    Increment {
        init: Expression,
        condition: Expression,
        increment: Expression,
    },
    LoopOver {
        variable: String,
        array: Expression,
    },
}

#[derive(Debug, Clone)]
pub struct CaseStatement {
    pub is_default: bool,
    pub condition: Option<Vec<Expression>>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum CfmlTag {
    CfSet(Rc<CfSet>),
    CfIf(Rc<CfIf>),
    CfQuery(Rc<CfQuery>),
    GeneralCfmlTag(Rc<GeneralCfmlTag>),
}

#[derive(Debug, Clone)]
pub struct CfSet {
    pub name: String,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct CfIf {
    pub condition: Expression,
    pub body: Vec<Statement>,
    pub else_if_blocks: Vec<(Expression, Vec<Statement>)>,
    pub else_body: Option<Vec<Statement>>,
}

#[derive(Debug, Clone)]
pub struct CfQuery {
    query: String,
}

#[derive(Debug, Clone)]
pub struct GeneralCfmlTag {
    pub name: String,
    pub attributes: Vec<(String, Expression)>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct Comment {
    pub content: String,
}
