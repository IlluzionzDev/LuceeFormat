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

impl<'ast, T> Walkable<T> for AST<'ast> {
    fn walk<V: crate::visitor::Visitor<T>>(&self, visitor: &mut V) -> T {
        visitor.visit(self)
    }
}

#[derive(Debug, Clone)]
pub enum Statement<'ast> {
    ExpressionStmt(Rc<ExpressionStatement<'ast>>),
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

impl<'ast, T> Walkable<T> for Statement<'ast> {
    fn walk<V: crate::visitor::Visitor<T>>(&self, visitor: &mut V) -> T {
        visitor.visit_statement(self)
    }
}

#[derive(Debug, Clone)]
pub struct ExpressionStatement<'ast> {
    pub expression: Expression<'ast>,
    pub semicolon_token: Option<Token<'ast>>,
}

impl<'ast, T> Walkable<T> for ExpressionStatement<'ast> {
    fn walk<V: crate::visitor::Visitor<T>>(&self, visitor: &mut V) -> T {
        visitor.visit_expression_statement(self)
    }
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration<'ast> {
    pub var_token: Token<'ast>,
    pub name: Token<'ast>,
    pub equals_token: Token<'ast>,
    pub value: Expression<'ast>,
    pub semicolon_token: Option<Token<'ast>>,
}

impl<'ast, T> Walkable<T> for VariableDeclaration<'ast> {
    fn walk<V: crate::visitor::Visitor<T>>(&self, visitor: &mut V) -> T {
        visitor.visit_variable_declaration(self)
    }
}

#[derive(Debug, Clone)]
pub struct VariableAssignment<'ast> {
    pub name: Expression<'ast>,
    pub equals_token: Token<'ast>,
    pub value: Expression<'ast>,
    pub semicolon_token: Option<Token<'ast>>,
}

impl<'ast, T> Walkable<T> for VariableAssignment<'ast> {
    fn walk<V: crate::visitor::Visitor<T>>(&self, visitor: &mut V) -> T {
        visitor.visit_variable_assignment(self)
    }
}

#[derive(Debug, Clone)]
pub struct ReturnStatement<'ast> {
    pub return_token: Token<'ast>,
    pub value: Option<Expression<'ast>>,
    pub semicolon_token: Option<Token<'ast>>,
}

impl<'ast, T> Walkable<T> for ReturnStatement<'ast> {
    fn walk<V: crate::visitor::Visitor<T>>(&self, visitor: &mut V) -> T {
        visitor.visit_return_statement(self)
    }
}

#[derive(Debug, Clone)]
pub struct LuceeFunction<'ast> {
    pub name: Token<'ast>,
    pub attributes: Vec<(Token<'ast>, Expression<'ast>)>,
    pub body: Option<Vec<Statement<'ast>>>,
    pub left_brace: Option<Token<'ast>>,
    pub right_brace: Option<Token<'ast>>,
    pub semicolon_token: Option<Token<'ast>>,
}

impl<'ast, T> Walkable<T> for LuceeFunction<'ast> {
    fn walk<V: crate::visitor::Visitor<T>>(&self, visitor: &mut V) -> T {
        visitor.visit_lucee_function(self)
    }
}

#[derive(Debug, Clone)]
pub enum Expression<'ast> {
    Literal(Rc<Literal<'ast>>),
    Identifier(Rc<Token<'ast>>),
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

impl<'ast, T> Walkable<T> for Expression<'ast> {
    fn walk<V: crate::visitor::Visitor<T>>(&self, visitor: &mut V) -> T {
        visitor.visit_expression(self)
    }
}

#[derive(Debug, Clone)]
pub struct FunctionCall<'ast> {
    pub name: Expression<'ast>,
    pub left_paren: Token<'ast>,
    pub right_paren: Token<'ast>,
    // 1: Identifier, 2: Expression, 3: Optional comma
    pub args: Vec<(Option<Token<'ast>>, Expression<'ast>, Option<Token<'ast>>)>,
}

impl<'ast, T> Walkable<T> for FunctionCall<'ast> {
    fn walk<V: crate::visitor::Visitor<T>>(&self, visitor: &mut V) -> T {
        visitor.visit_function_call(self)
    }
}

#[derive(Debug, Clone)]
pub struct ObjectCreation<'ast> {
    pub new_token: Token<'ast>,
    pub expr: Expression<'ast>,
}

impl<'ast, T> Walkable<T> for ObjectCreation<'ast> {
    fn walk<V: crate::visitor::Visitor<T>>(&self, visitor: &mut V) -> T {
        visitor.visit_object_creation(self)
    }
}

#[derive(Debug, Clone)]
pub struct ArrayExpression<'ast> {
    pub left_bracket: Token<'ast>,
    pub right_bracket: Token<'ast>,
    // Each element may have a comma token after it (except the last one)
    pub elements: Vec<(Expression<'ast>, Option<Token<'ast>>)>,
}

impl<'ast, T> Walkable<T> for ArrayExpression<'ast> {
    fn walk<V: crate::visitor::Visitor<T>>(&self, visitor: &mut V) -> T {
        visitor.visit_array_expression(self)
    }
}

#[derive(Debug, Clone)]
pub struct StructExpression<'ast> {
    pub left_brace: Token<'ast>,
    pub right_brace: Token<'ast>,
    pub elements: Vec<(Token<'ast>, Expression<'ast>)>,
}

impl<'ast, T> Walkable<T> for StructExpression<'ast> {
    fn walk<V: crate::visitor::Visitor<T>>(&self, visitor: &mut V) -> T {
        visitor.visit_struct_expression(self)
    }
}

#[derive(Debug, Clone)]
pub struct LambdaExpression<'ast> {
    pub left_paren: Option<Token<'ast>>,
    pub right_paren: Option<Token<'ast>>,
    pub parameters: Vec<Token<'ast>>,
    pub lambda_token: Token<'ast>,
    pub left_brace: Option<Token<'ast>>,
    pub right_brace: Option<Token<'ast>>,
    pub body: Vec<Statement<'ast>>,
}

impl<'ast, T> Walkable<T> for LambdaExpression<'ast> {
    fn walk<V: crate::visitor::Visitor<T>>(&self, visitor: &mut V) -> T {
        visitor.visit_lambda_expression(self)
    }
}

#[derive(Debug, Clone)]
pub struct BinaryExpression<'ast> {
    pub left: Expression<'ast>,
    pub op: Token<'ast>,
    pub right: Expression<'ast>,
}

impl<'ast, T> Walkable<T> for BinaryExpression<'ast> {
    fn walk<V: crate::visitor::Visitor<T>>(&self, visitor: &mut V) -> T {
        visitor.visit_binary_expression(self)
    }
}

#[derive(Debug, Clone)]
pub struct UnaryExpression<'ast> {
    pub op: Token<'ast>,
    pub expr: Expression<'ast>,
}

impl<'ast, T> Walkable<T> for UnaryExpression<'ast> {
    fn walk<V: crate::visitor::Visitor<T>>(&self, visitor: &mut V) -> T {
        visitor.visit_unary_expression(self)
    }
}

#[derive(Debug, Clone)]
pub struct TernaryExpression<'ast> {
    pub condition: Expression<'ast>,
    pub question_token: Token<'ast>,
    pub true_expr: Expression<'ast>,
    pub colon_token: Token<'ast>,
    pub false_expr: Expression<'ast>,
}

impl<'ast, T> Walkable<T> for TernaryExpression<'ast> {
    fn walk<V: crate::visitor::Visitor<T>>(&self, visitor: &mut V) -> T {
        visitor.visit_ternary_expression(self)
    }
}

#[derive(Debug, Clone)]
pub struct GroupExpression<'ast> {
    pub left_paren: Token<'ast>,
    pub expr: Expression<'ast>,
    pub right_paren: Token<'ast>,
}

impl<'ast, T> Walkable<T> for GroupExpression<'ast> {
    fn walk<V: crate::visitor::Visitor<T>>(&self, visitor: &mut V) -> T {
        visitor.visit_group_expression(self)
    }
}

#[derive(Debug, Clone)]
pub struct MemberAccess<'ast> {
    pub object: Expression<'ast>,
    pub dot_token: Token<'ast>,
    pub property: Expression<'ast>,
}

impl<'ast, T> Walkable<T> for MemberAccess<'ast> {
    fn walk<V: crate::visitor::Visitor<T>>(&self, visitor: &mut V) -> T {
        visitor.visit_member_expression(self)
    }
}

#[derive(Debug, Clone)]
pub struct IndexAccess<'ast> {
    pub object: Expression<'ast>,
    pub left_bracket: Token<'ast>,
    pub index: Expression<'ast>,
    pub right_bracket: Token<'ast>,
}

impl<'ast, T> Walkable<T> for IndexAccess<'ast> {
    fn walk<V: crate::visitor::Visitor<T>>(&self, visitor: &mut V) -> T {
        visitor.visit_index_access(self)
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

impl<'ast, T> Walkable<T> for Literal<'ast> {
    fn walk<V: crate::visitor::Visitor<T>>(&self, visitor: &mut V) -> T {
        visitor.visit_literal(self)
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

impl PartialEq<Self> for BinaryOperator {
    fn eq(&self, other: &Self) -> bool {
        return self.to_lexeme().eq(other.to_lexeme());
    }
}

impl BinaryOperator {
    pub fn to_lexeme(&self) -> &'static str {
        match self {
            BinaryOperator::Add => "+",
            BinaryOperator::Subtract => "-",
            BinaryOperator::Multiply => "*",
            BinaryOperator::Divide => "/",
            BinaryOperator::Equal => "==",
            BinaryOperator::NotEqual => "!=",
            BinaryOperator::Less => "<",
            BinaryOperator::Greater => ">",
            BinaryOperator::LessEqual => "<=",
            BinaryOperator::GreaterEqual => ">=",
            BinaryOperator::And => "&&",
            BinaryOperator::Or => "||",
            BinaryOperator::Xor => "^",
            BinaryOperator::Contains => "contains",
            BinaryOperator::Eq => "eq",
            BinaryOperator::Neq => "neq",
            BinaryOperator::Lt => "lt",
            BinaryOperator::Gt => "gt",
            BinaryOperator::StringConcat => "&",
            BinaryOperator::LogicalAnd => "and",
            BinaryOperator::LogicalOr => "or",
            BinaryOperator::PlusEqual => "+=",
            BinaryOperator::DivideEqual => "/=",
            BinaryOperator::MultiplyEqual => "*=",
            BinaryOperator::MinusEqual => "-=",
            BinaryOperator::PlusPlus => "++",
            BinaryOperator::MinusMinus => "--",
            BinaryOperator::ConcatEqual => "&=",
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Not,
    Negate,
}

impl UnaryOperator {
    pub fn to_lexeme(&self) -> &'static str {
        match self {
            UnaryOperator::Not => "!",
            UnaryOperator::Negate => "-",
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDefinition<'ast> {
    pub access_modifier: Option<AccessModifier>,
    pub access_modifier_token: Option<Token<'ast>>,
    pub return_type: Option<Token<'ast>>,
    pub function_token: Token<'ast>,
    pub name: Token<'ast>,
    pub left_paren: Token<'ast>,
    pub parameters: Vec<Parameter<'ast>>,
    pub right_paren: Token<'ast>,
    pub body: Vec<Statement<'ast>>,
    pub left_brace: Token<'ast>,
    pub right_brace: Token<'ast>,
}

impl<'ast, T> Walkable<T> for FunctionDefinition<'ast> {
    fn walk<V: crate::visitor::Visitor<T>>(&self, visitor: &mut V) -> T {
        visitor.visit_function_definition(self)
    }
}

#[derive(Debug, Clone)]
pub struct ComponentDefinition<'ast> {
    pub component_token: Token<'ast>,
    pub attributes: Vec<(Token<'ast>, Expression<'ast>)>,
    pub body: Vec<Statement<'ast>>,
    pub left_brace: Token<'ast>,
    pub right_brace: Token<'ast>,
}

impl<'ast, T> Walkable<T> for ComponentDefinition<'ast> {
    fn walk<V: crate::visitor::Visitor<T>>(&self, visitor: &mut V) -> T {
        visitor.visit_component_definition(self)
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
    pub required: Option<Token<'ast>>,
    pub param_type: Option<Token<'ast>>,
    pub name: Token<'ast>,
    pub equals_token: Option<Token<'ast>>,
    pub default_value: Option<Expression<'ast>>,
}

#[derive(Debug, Clone)]
pub struct IfStatement<'ast> {
    pub if_token: Token<'ast>,
    pub left_paren: Token<'ast>,
    pub right_paren: Token<'ast>,
    pub condition: Expression<'ast>,
    pub body: Vec<Statement<'ast>>,
    pub left_brace: Option<Token<'ast>>,
    pub right_brace: Option<Token<'ast>>,
    // Potentially contains another if statement
    pub else_body: Option<Vec<Statement<'ast>>>,
    pub else_left_brace: Option<Token<'ast>>,
    pub else_right_brace: Option<Token<'ast>>,
    pub else_token: Option<Token<'ast>>,
}

impl<'ast, T> Walkable<T> for IfStatement<'ast> {
    fn walk<V: crate::visitor::Visitor<T>>(&self, visitor: &mut V) -> T {
        visitor.visit_if_statement(self)
    }
}

#[derive(Debug, Clone)]
pub struct ForStatement<'ast> {
    pub for_token: Token<'ast>,
    pub left_paren: Token<'ast>,
    pub right_paren: Token<'ast>,
    pub control: ForControl<'ast>,
    pub body: Vec<Statement<'ast>>,
    pub left_brace: Token<'ast>,
    pub right_brace: Token<'ast>,
}

// Used to determine if For loop is traditional var i = 0, or for (i in array)
#[derive(Debug, Clone)]
pub enum ForControl<'ast> {
    Increment {
        var_token: Option<Token<'ast>>,
        variable: Token<'ast>,
        equals_token: Token<'ast>,
        init: Expression<'ast>,
        condition: Expression<'ast>,
        increment: Expression<'ast>,
    },
    LoopOver {
        var_token: Option<Token<'ast>>,
        variable: Token<'ast>,
        in_token: Token<'ast>,
        array: Expression<'ast>,
    },
}

impl<'ast, T> Walkable<T> for ForStatement<'ast> {
    fn walk<V: crate::visitor::Visitor<T>>(&self, visitor: &mut V) -> T {
        visitor.visit_for_statement(self)
    }
}

#[derive(Debug, Clone)]
pub struct WhileStatement<'ast> {
    pub do_while: bool, // If is in form do { } while (condition), otherwise while (condition) { }
    pub do_token: Option<Token<'ast>>,
    pub while_token: Token<'ast>,
    pub condition: Expression<'ast>,
    pub left_paren: Token<'ast>,
    pub right_paren: Token<'ast>,
    pub body: Vec<Statement<'ast>>,
    pub left_brace: Token<'ast>,
    pub right_brace: Token<'ast>,
}

impl<'ast, T> Walkable<T> for WhileStatement<'ast> {
    fn walk<V: crate::visitor::Visitor<T>>(&self, visitor: &mut V) -> T {
        visitor.visit_while_statement(self)
    }
}

#[derive(Debug, Clone)]
pub struct SwitchStatement<'ast> {
    pub switch_token: Token<'ast>,
    pub left_paren: Token<'ast>,
    pub right_paren: Token<'ast>,
    pub expression: Expression<'ast>,
    pub left_brace: Token<'ast>,
    pub right_brace: Token<'ast>,
    pub cases: Vec<CaseStatement<'ast>>,
}

#[derive(Debug, Clone)]
pub struct CaseStatement<'ast> {
    pub is_default: bool,
    // List of conditions (case/default token, expression (option as default omits this), colon token)
    pub condition: Vec<(Token<'ast>, Expression<'ast>, Token<'ast>)>,
    pub body: Vec<Statement<'ast>>,
}

impl<'ast, T> Walkable<T> for SwitchStatement<'ast> {
    fn walk<V: crate::visitor::Visitor<T>>(&self, visitor: &mut V) -> T {
        visitor.visit_switch_statement(self)
    }
}

#[derive(Debug, Clone)]
pub struct TryCatchStatement<'ast> {
    pub try_token: Token<'ast>,
    pub try_body: Vec<Statement<'ast>>,
    pub try_left_brace: Token<'ast>,
    pub try_right_brace: Token<'ast>,
    pub catch_token: Token<'ast>,
    pub left_paren: Token<'ast>,
    pub right_paren: Token<'ast>,
    pub catch_var_token: Option<Token<'ast>>,
    pub catch_var_type: Option<Expression<'ast>>,
    pub catch_var: Token<'ast>,
    pub catch_body: Vec<Statement<'ast>>,
    pub catch_left_brace: Token<'ast>,
    pub catch_right_brace: Token<'ast>,
}

impl<'ast, T> Walkable<T> for TryCatchStatement<'ast> {
    fn walk<V: crate::visitor::Visitor<T>>(&self, visitor: &mut V) -> T {
        visitor.visit_try_catch_statement(self)
    }
}
