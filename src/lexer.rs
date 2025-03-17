use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
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
    Minus,
    Semicolon,
    Slash,
    Star,
    Hash,

    // One or two character tokens
    Plus,
    PlusPlus,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Lambda,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Ampersand,
    AmpersandAmpersand,
    Pipe,
    PipePipe,
    Question,
    QuestionColon,
    QuestionDot,
    Colon,
    ColonColon,

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

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: u32,
    pub column: u32,
}

pub(crate) struct Lexer {
    source: String,
    pub(crate) tokens: Vec<Token>,
    keywords: HashMap<String, TokenType>,

    start: usize,
    current: usize,
    line: u32,
    column: u32,
}

impl Lexer {
    pub fn new(source: String) -> Lexer {
        let mut keywords = HashMap::new();

        keywords.insert(String::from("if"), TokenType::If);
        keywords.insert(String::from("else"), TokenType::Else);
        keywords.insert(String::from("for"), TokenType::For);
        keywords.insert(String::from("while"), TokenType::While);
        keywords.insert(String::from("return"), TokenType::Return);
        keywords.insert(String::from("function"), TokenType::Function);
        keywords.insert(String::from("var"), TokenType::Var);
        keywords.insert(String::from("new"), TokenType::New);
        keywords.insert(String::from("true"), TokenType::True);
        keywords.insert(String::from("false"), TokenType::False);
        keywords.insert(String::from("null"), TokenType::Null);
        keywords.insert(String::from("required"), TokenType::Required);
        keywords.insert(String::from("component"), TokenType::Component);
        // keywords.insert(String::from("this"), TokenType::This);
        keywords.insert(String::from("abstract"), TokenType::Abstract);
        keywords.insert(String::from("break"), TokenType::Break);
        keywords.insert(String::from("case"), TokenType::Case);
        keywords.insert(String::from("catch"), TokenType::Catch);
        keywords.insert(String::from("continue"), TokenType::Continue);
        keywords.insert(String::from("contains"), TokenType::Contains);
        keywords.insert(String::from("default"), TokenType::Default);
        keywords.insert(String::from("do"), TokenType::Do);
        keywords.insert(String::from("finally"), TokenType::Finally);
        keywords.insert(String::from("final"), TokenType::Final);
        keywords.insert(String::from("import"), TokenType::Import);
        keywords.insert(String::from("imp"), TokenType::Imp);
        keywords.insert(String::from("interface"), TokenType::Interface);
        keywords.insert(String::from("switch"), TokenType::Switch);
        keywords.insert(String::from("try"), TokenType::Try);

        keywords.insert(String::from("public"), TokenType::Public);
        keywords.insert(String::from("private"), TokenType::Private);
        keywords.insert(String::from("protected"), TokenType::Protected);

        keywords.insert(String::from("eqv"), TokenType::Eqv);
        keywords.insert(String::from("EQV"), TokenType::Eqv);
        keywords.insert(String::from("eq"), TokenType::Eq);
        keywords.insert(String::from("EQ"), TokenType::Eq);
        keywords.insert(String::from("gt"), TokenType::Gt);
        keywords.insert(String::from("GT"), TokenType::Gt);
        keywords.insert(String::from("gte"), TokenType::Gte);
        keywords.insert(String::from("GTE"), TokenType::Gte);
        keywords.insert(String::from("in"), TokenType::In);
        keywords.insert(String::from("IN"), TokenType::In);
        keywords.insert(String::from("lt"), TokenType::Lt);
        keywords.insert(String::from("LT"), TokenType::Lt);
        keywords.insert(String::from("lte"), TokenType::Lte);
        keywords.insert(String::from("LTE"), TokenType::Lte);
        keywords.insert(String::from("neq"), TokenType::Neq);
        keywords.insert(String::from("NEQ"), TokenType::Neq);
        keywords.insert(String::from("not"), TokenType::Not);
        keywords.insert(String::from("NOT"), TokenType::Not);
        keywords.insert(String::from("or"), TokenType::Or);
        keywords.insert(String::from("OR"), TokenType::Or);
        keywords.insert(String::from("and"), TokenType::And);
        keywords.insert(String::from("AND"), TokenType::And);
        keywords.insert(String::from("xor"), TokenType::Xor);
        keywords.insert(String::from("XOR"), TokenType::Xor);

        Lexer {
            source,
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
            column: 0,
            keywords,
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    pub(crate) fn scan_tokens(&mut self) {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token {
            token_type: TokenType::EOF,
            lexeme: String::from(""),
            line: self.line,
            column: self.current as u32,
        });
    }

    fn scan_token(&mut self) {
        let c = self.advance();
        match c {
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            '[' => self.add_token(TokenType::LeftBracket),
            ']' => self.add_token(TokenType::RightBracket),
            '#' => self.add_token(TokenType::Hash),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            '-' => self.add_token(TokenType::Minus),
            ';' => self.add_token(TokenType::Semicolon),
            '*' => self.add_token(TokenType::Star),
            '+' => {
                if self.match_char('+') {
                    self.add_token(TokenType::PlusPlus);
                } else {
                    self.add_token(TokenType::Plus);
                }
            }
            ':' => {
                if self.match_char(':') {
                    self.add_token(TokenType::ColonColon);
                } else {
                    self.add_token(TokenType::Colon);
                }
            }
            '?' => {
                if self.match_char(':') {
                    self.add_token(TokenType::QuestionColon);
                } else if self.match_char('.') {
                    self.add_token(TokenType::QuestionDot);
                } else {
                    self.add_token(TokenType::Question);
                }
            }
            '&' => {
                if self.match_char('&') {
                    self.add_token(TokenType::AmpersandAmpersand);
                } else {
                    self.add_token(TokenType::Ampersand);
                }
            }
            '|' => {
                if self.match_char('|') {
                    self.add_token(TokenType::PipePipe);
                } else {
                    self.add_token(TokenType::Pipe);
                }
            }
            '!' => {
                if self.match_char('=') {
                    self.add_token(TokenType::BangEqual);
                } else {
                    self.add_token(TokenType::Bang);
                }
            }
            '=' => {
                if self.match_char('>') {
                    self.add_token(TokenType::Lambda);
                } else if self.match_char('=') {
                    self.add_token(TokenType::EqualEqual);
                } else {
                    self.add_token(TokenType::Equal);
                }
            }
            '<' => {
                if self.match_char('=') {
                    self.add_token(TokenType::LessEqual);
                } else {
                    self.add_token(TokenType::Less);
                }
            }
            '>' => {
                if self.match_char('=') {
                    self.add_token(TokenType::GreaterEqual);
                } else {
                    self.add_token(TokenType::Greater);
                }
            }
            '/' => {
                if self.match_char('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                    // self.add_token(TokenType::Comment);
                } else if self.match_char('*') {
                    while (self.peek() != '*' || self.peek_next() != '/') && !self.is_at_end() {
                        if self.peek() == '\n' {
                            self.line += 1;
                            self.column = 0;
                        }
                        self.advance();
                    }
                    self.advance();
                    self.advance();
                    // self.add_token(TokenType::Comment);
                } else {
                    self.add_token(TokenType::Slash);
                }
            }
            '"' | '\'' => self.string(),
            '0'..='9' => self.number(),
            'a'..='z' | 'A'..='Z' | '_' => self.identifier(),
            ' ' | '\r' | '\t' => (),
            '\n' => {
                // self.add_token(TokenType::NewLine);
                self.line += 1;
                self.column = 0;
            }
            _ => println!(
                "Unexpected character {0} at {1}:{2}",
                self.source[self.start..self.current].to_string(),
                self.line,
                self.column
            ),
        }
    }

    fn identifier(&mut self) {
        while (self.peek().is_alphanumeric() || self.peek() == '_') && !self.is_at_end() {
            self.advance();
        }

        let text = self.source[self.start..self.current].to_string();

        let token_type = self.keywords
            .get(&text)
            .unwrap_or(&TokenType::Identifier)
            .clone();
        
        self.add_token(token_type);
    }

    fn string(&mut self) {
        let quote = self.source.chars().nth(self.start).unwrap();
        while self.peek() != quote && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
                self.column = 0;
            }
            self.advance();
        }

        if self.is_at_end() {
            println!("Unterminated string at {0}:{1}", self.line, self.current);
            return;
        }

        self.advance();

        let value = self.source[self.start + 1..self.current - 1].to_string();
        self.add_token_full(TokenType::String, value);
    }

    fn number(&mut self) {
        while self.peek().is_digit(10) {
            self.advance();
        }

        if self.peek() == '.' && self.peek_next().is_digit(10) {
            self.advance();

            while self.peek().is_digit(10) {
                self.advance();
            }
        }

        let value = self.source[self.start..self.current].to_string();
        self.add_token_full(TokenType::Number, value);
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }

        if self.source.chars().nth(self.current).unwrap() != expected {
            return false;
        }

        self.current += 1;
        self.column += 1;
        true
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }

        self.source.chars().nth(self.current).unwrap()
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            return '\0';
        }

        self.source.chars().nth(self.current + 1).unwrap()
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.column += 1;
        self.source.chars().nth(self.current - 1).unwrap()
    }

    fn add_token(&mut self, token_type: TokenType) {
        let text = self.source[self.start..self.current].to_string();
        self.add_token_full(token_type, text);
    }

    fn add_token_full(&mut self, token_type: TokenType, literal: String) {
        self.tokens.push(Token {
            token_type,
            lexeme: literal,
            line: self.line,
            column: self.column as u32,
        });
    }
}
