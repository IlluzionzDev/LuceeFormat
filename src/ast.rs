use crate::lexer::Token;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Program {
    Statements(Vec<Statement>),
}

#[derive(Debug, Clone)]
pub enum Statement {
    ExpressionStmt(Expression),
    VariableDeclaration {
        name: String,
        value: Expression,
    },
    VariableAssignment {
        name: Expression,
        value: Expression,
    },
    ReturnStatement(Option<Expression>),
    FunctionDefinition(FunctionDefinition),
    ComponentDefinition(ComponentDefinition),
    LuceeFunction {
        attributes: Vec<(String, Expression)>,
        body: Option<Vec<Statement>>,
    },
    ControlStructure(ControlStructure),
    CfmlTag(CfmlTag),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    Identifier(String),
    FunctionCall {
        name: String,
        args: Vec<(Option<String>, Expression)>,
    },
    ObjectCreation(Box<Expression>),
    ArrayExpression(Vec<Expression>),
    StructExpression(Vec<(String, Expression)>),
    LambdaExpression {
        parameters: Vec<String>,
        body: Vec<Statement>,
    },
    BinaryExpression {
        left: Box<Expression>,
        op: BinaryOperator,
        right: Box<Expression>,
    },
    UnaryExpression {
        op: UnaryOperator,
        expr: Box<Expression>,
    },
    TernaryExpression {
        condition: Box<Expression>,
        true_expr: Box<Expression>,
        false_expr: Box<Expression>,
    },
    GroupExpression(Box<Expression>),
    MemberAccess {
        object: Box<Expression>,
        property: Box<Expression>,
    },
    IndexAccess {
        object: Box<Expression>,
        index: Box<Expression>,
    },
    None, // Trying to consume expression but there is none. Represented by blank space
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
pub enum ControlStructure {
    IfStatement {
        condition: Expression,
        body: Vec<Statement>,
        else_body: Option<Vec<Statement>>,
    },
    LoopStatement(LoopStatement),
    SwitchStatement {
        expression: Expression,
        cases: Vec<CaseStatement>,
    },
    TryCatchStatement {
        try_body: Vec<Statement>,
        catch_var: String,
        catch_body: Vec<Statement>,
    },
}

#[derive(Debug, Clone)]
pub enum LoopStatement {
    For {
        control: ForControl,
        body: Vec<Statement>,
    },
    While {
        condition: Expression,
        body: Vec<Statement>,
    },
    DoWhile {
        body: Vec<Statement>,
        condition: Expression,
    },
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
    CfSet {
        name: String,
        value: Expression,
    },
    CfIf {
        condition: Expression,
        body: Vec<Statement>,
        else_body: Option<Vec<Statement>>,
    },
    GeneralCfmlTag {
        name: String,
        attributes: Vec<(String, Expression)>,
        body: Vec<Statement>,
    },
}

#[derive(Debug, Clone)]
pub struct Comment {
    pub content: String,
}
