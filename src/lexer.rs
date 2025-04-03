use phf::phf_map;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Dot,
    Semicolon,
    Hash,

    // One or two character tokens
    Plus,
    PlusPlus,
    PlusEqual,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Lambda,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    LessSlash,
    Ampersand,
    AmpersandEqual,
    AmpersandAmpersand,
    Pipe,
    PipePipe,
    Question,
    QuestionColon,
    QuestionDot,
    Colon,
    ColonColon,
    Slash,
    SlashEqual,
    Star,
    StarEqual,
    Minus,
    MinusMinus,
    MinusEqual,

    // Literals: hold value representation
    Identifier,
    String,
    Number,

    // Keywords
    If,
    Else,
    For,
    While,
    Return,
    Function,
    Var,
    True,
    False,
    Null,
    New,
    Required,
    Component,
    This,
    Abstract,
    And,
    Break,
    Case,
    Catch,
    Continue,
    Contains,
    Default,
    Do,
    Eq,
    Eqv,
    Finally,
    Final,
    Gt,
    Gte,
    Import,
    Imp,
    In,
    Is,
    Interface,
    Lt,
    Lte,
    Neq,
    Not,
    Or,
    Switch,
    Try,
    Xor,

    Public,
    Private,
    Protected,

    // Special representations: often hold internal value
    EOF,
    NewLine,
    Comment,
}

/// Represents a literal token in the source code.
/// Includes what type of token, the string literal from the source,
/// and position data in the source
#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub token_type: TokenType,
    pub lexeme: &'a str,
    pub line: usize,
    pub column: usize,
    pub end_column: usize,
    pub span: SourceSpan,
    /// A comment token that might appear before this token.
    /// This is how we handle wacky inline comments and preserve comments in the AST
    /// This does mean in the AST we at minimum must store the tokens that make up
    /// statements. For example 'if (condition)' would store the token for 'if', '(', 'condition', and ')'
    pub comments: Option<Vec<Token<'a>>>,
}

/// Represents absolute position data of a token
#[derive(Debug, Clone)]
pub struct SourceSpan {
    pub start: usize,
    pub end: usize,
}

pub(crate) struct Lexer<'a> {
    pub source: &'a str,

    start: usize,
    current: usize,
    line: usize,

    // Represent current index in line
    column: usize,
    end_column: usize,

    // Current comments to append to next read token
    pub pop_comments: Option<Vec<Token<'a>>>,
}

static KEYWORDS: phf::Map<&'static str, TokenType> = phf_map! {
    "if" => TokenType::If,
    "else" => TokenType::Else,
    "for" => TokenType::For,
    "while" => TokenType::While,
    "return" => TokenType::Return,
    "function" => TokenType::Function,
    "var" => TokenType::Var,
    "new" => TokenType::New,
    "true" => TokenType::True,
    "false" => TokenType::False,
    "null" => TokenType::Null,
    "required" => TokenType::Required,
    "component" => TokenType::Component,
    "abstract" => TokenType::Abstract,
    "break" => TokenType::Break,
    "case" => TokenType::Case,
    "catch" => TokenType::Catch,
    "continue" => TokenType::Continue,
    "contains" => TokenType::Contains,
    "default" => TokenType::Default,
    "do" => TokenType::Do,
    "finally" => TokenType::Finally,
    "final" => TokenType::Final,
    "import" => TokenType::Import,
    "imp" => TokenType::Imp,
    "interface" => TokenType::Interface,
    "switch" => TokenType::Switch,
    "try" => TokenType::Try,
    "public" => TokenType::Public,
    "private" => TokenType::Private,
    "protected" => TokenType::Protected,
    "eqv" => TokenType::Eqv,
    "EQV" => TokenType::Eqv,
    "eq" => TokenType::Eq,
    "EQ" => TokenType::Eq,
    "gt" => TokenType::Gt,
    "GT" => TokenType::Gt,
    "gte" => TokenType::Gte,
    "GTE" => TokenType::Gte,
    "in" => TokenType::In,
    "IN" => TokenType::In,
    "lt" => TokenType::Lt,
    "LT" => TokenType::Lt,
    "lte" => TokenType::Lte,
    "LTE" => TokenType::Lte,
    "neq" => TokenType::Neq,
    "NEQ" => TokenType::Neq,
    "not" => TokenType::Not,
    "NOT" => TokenType::Not,
    "or" => TokenType::Or,
    "OR" => TokenType::Or,
    "and" => TokenType::And,
    "AND" => TokenType::And,
    "xor" => TokenType::Xor,
    "XOR" => TokenType::Xor,
};

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Lexer<'a> {
        Lexer {
            source,
            start: 0,
            current: 0,
            line: 1,
            column: 1,
            end_column: 1,
            pop_comments: None,
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= (&self.source).len()
    }

    pub fn scan_token(&mut self) -> Token<'a> {
        loop {
            self.start = self.current;
            self.column = self.end_column;

            if self.is_at_end() {
                return Token {
                    token_type: TokenType::EOF,
                    lexeme: "",
                    line: self.line,
                    column: self.current,
                    end_column: self.current,
                    span: SourceSpan {
                        start: self.start,
                        end: self.current,
                    },
                    comments: None,
                };
            }

            let c = self.advance();
            match c {
                '(' => return self.add_token(TokenType::LeftParen),
                ')' => return self.add_token(TokenType::RightParen),
                '{' => return self.add_token(TokenType::LeftBrace),
                '}' => return self.add_token(TokenType::RightBrace),
                '[' => return self.add_token(TokenType::LeftBracket),
                ']' => return self.add_token(TokenType::RightBracket),
                '#' => return self.add_token(TokenType::Hash),
                ',' => return self.add_token(TokenType::Comma),
                '.' => return self.add_token(TokenType::Dot),
                ';' => return self.add_token(TokenType::Semicolon),
                '*' => {
                    return if self.match_char('=') {
                        self.add_token(TokenType::StarEqual)
                    } else {
                        self.add_token(TokenType::Star)
                    }
                }
                '-' => {
                    return if self.match_char('=') {
                        self.add_token(TokenType::MinusEqual)
                    } else if self.match_char('-') {
                        self.add_token(TokenType::MinusMinus)
                    } else {
                        self.add_token(TokenType::Minus)
                    }
                }
                '+' => {
                    return if self.match_char('=') {
                        self.add_token(TokenType::PlusEqual)
                    } else if self.match_char('+') {
                        self.add_token(TokenType::PlusPlus)
                    } else {
                        self.add_token(TokenType::Plus)
                    }
                }
                ':' => {
                    return if self.match_char(':') {
                        self.add_token(TokenType::ColonColon)
                    } else {
                        self.add_token(TokenType::Colon)
                    }
                }
                '?' => {
                    return if self.match_char(':') {
                        self.add_token(TokenType::QuestionColon)
                    } else if self.match_char('.') {
                        self.add_token(TokenType::QuestionDot)
                    } else {
                        self.add_token(TokenType::Question)
                    }
                }
                '&' => {
                    return if self.match_char('=') {
                        self.add_token(TokenType::AmpersandEqual)
                    } else if self.match_char('&') {
                        self.add_token(TokenType::AmpersandAmpersand)
                    } else {
                        self.add_token(TokenType::Ampersand)
                    }
                }
                '|' => {
                    return if self.match_char('|') {
                        self.add_token(TokenType::PipePipe)
                    } else {
                        self.add_token(TokenType::Pipe)
                    }
                }
                '!' => {
                    return if self.match_char('=') {
                        self.add_token(TokenType::BangEqual)
                    } else {
                        self.add_token(TokenType::Bang)
                    }
                }
                '=' => {
                    return if self.match_char('>') {
                        self.add_token(TokenType::Lambda)
                    } else if self.match_char('=') {
                        self.add_token(TokenType::EqualEqual)
                    } else {
                        self.add_token(TokenType::Equal)
                    }
                }
                '<' => {
                    return if self.match_char('=') {
                        self.add_token(TokenType::LessEqual)
                    } else if self.match_char('/') {
                        self.add_token(TokenType::LessSlash)
                    } else {
                        self.add_token(TokenType::Less)
                    }
                }
                '>' => {
                    return if self.match_char('=') {
                        self.add_token(TokenType::GreaterEqual)
                    } else {
                        self.add_token(TokenType::Greater)
                    }
                }
                '/' => {
                    if self.match_char('=') {
                        return self.add_token(TokenType::SlashEqual);
                    } else if self.match_char('/') {
                        while self.peek() != '\n' && !self.is_at_end() {
                            self.advance();
                        }

                        let _ = &self.pop_comments.get_or_insert_with(Vec::new).push(Token {
                            token_type: TokenType::Comment,
                            lexeme: &self.source[self.start..self.current],
                            line: self.line,
                            column: self.column,
                            end_column: self.end_column,
                            span: SourceSpan {
                                start: self.start,
                                end: self.current,
                            },
                            comments: None,
                        });

                        continue;
                        // return self.add_token(TokenType::Comment);
                    } else if self.match_char('*') {
                        while (self.peek() != '*' || self.peek_next() != '/') && !self.is_at_end() {
                            if self.peek() == '\n' {
                                self.line += 1;
                                self.column = 1;
                                self.end_column = 1;
                            }
                            self.advance();
                        }
                        self.advance();
                        self.advance();

                        let _ = &self.pop_comments.get_or_insert_with(Vec::new).push(Token {
                            token_type: TokenType::Comment,
                            lexeme: &self.source[self.start..self.current],
                            line: self.line,
                            column: self.column,
                            end_column: self.end_column,
                            span: SourceSpan {
                                start: self.start,
                                end: self.current,
                            },
                            comments: None,
                        });

                        continue;
                        // return self.add_token(TokenType::Comment);
                    } else {
                        return self.add_token(TokenType::Slash);
                    }
                }
                '"' | '\'' => return self.string(),
                '0'..='9' => return self.number(),
                'a'..='z' | 'A'..='Z' | '_' => return self.identifier(),
                '\t' => {
                    // Tabs are 4 spaces, so add extra 3 onto the 1
                    self.end_column += 3;
                    continue;
                }
                '\r' | ' ' => continue,
                '\n' => {
                    self.line += 1;
                    self.column = 1;
                    self.end_column = 1;
                    continue;
                }
                _ => panic!(
                    "Unexpected character {0} at {1}:{2}",
                    self.source[self.start..self.current].to_string(),
                    self.line,
                    self.column
                ),
            };
        }
    }

    fn identifier(&mut self) -> Token<'a> {
        while (self.peek().is_alphanumeric() || self.peek() == '_') && !self.is_at_end() {
            self.advance();
        }

        let text = &self.source[self.start..self.current];
        let token_type = KEYWORDS.get(text).copied().unwrap_or(TokenType::Identifier);

        self.add_token(token_type)
    }

    fn string(&mut self) -> Token<'a> {
        let quote = self
            .source
            .as_bytes()
            .get(self.start)
            .map_or('\0', |&b| b as char);

        // Strings have some special properties in lucee, if you use a raw '#" inside
        // a string, it allows you to define code in place until encountering another #"
        // Need to account for this and parse past this, as in ignore terminal quotes if inside a hash

        let mut in_hash = false;
        while (self.peek() != quote && !self.is_at_end()) || in_hash {
            if self.peek() == '\n' {
                self.line += 1;
                self.column = 1;
                self.end_column = 1;
            }
            if self.peek() == '#' {
                in_hash = !in_hash;
            }
            self.advance();
        }

        if self.is_at_end() {
            panic!("Unterminated string at {0}:{1}", self.line, self.current);
        }

        self.advance();

        let value = &self.source[self.start + 1..self.current - 1];
        self.add_token_full(TokenType::String, value)
    }

    fn number(&mut self) -> Token<'a> {
        while self.peek().is_digit(10) {
            self.advance();
        }

        if self.peek() == '.' && self.peek_next().is_digit(10) {
            self.advance();

            while self.peek().is_digit(10) {
                self.advance();
            }
        }

        let value = &self.source[self.start..self.current];
        self.add_token_full(TokenType::Number, value)
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.peek() != expected {
            return false;
        }

        self.current += 1;
        self.end_column += 1;
        true
    }

    fn peek(&self) -> char {
        self.source
            .as_bytes()
            .get(self.current)
            .map_or('\0', |&b| b as char)
    }

    fn peek_next(&self) -> char {
        self.source
            .as_bytes()
            .get(self.current + 1)
            .map_or('\0', |&b| b as char)
    }

    fn advance(&mut self) -> char {
        let ch = self.peek();
        self.current += 1;
        self.end_column += 1;
        ch
    }

    fn add_token(&mut self, token_type: TokenType) -> Token<'a> {
        let text = &self.source[self.start..self.current];
        self.add_token_full(token_type, text)
    }

    fn add_token_full(&mut self, token_type: TokenType, literal: &'a str) -> Token<'a> {
        let mut comments = None;
        match &self.pop_comments {
            Some(pop) => {
                // Pop comment off
                comments = Some(pop.clone());
                self.pop_comments = None;
            }
            _ => {}
        }
        Token {
            token_type,
            lexeme: literal,
            line: self.line,
            column: self.column,
            end_column: self.end_column - 1,
            span: SourceSpan {
                start: self.start,
                end: self.current,
            },
            comments,
        }
    }
}
