pub mod lexer;

use phf::{phf_map, Map};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenKind {
    ASSIGN,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LBRACKET,
    RBRACKET,
    SEMICOLON,
    COMMA,
    COLON,
    PLUS,
    MINUS,
    MULTIPLY,
    DIVIDE,
    MODULUS,
    POINT,
    INCREMENT,
    DECREMENT,
    AND,
    OR,
    NOT,
    GT,
    LT,
    GTE,
    LTE,
    NEQ,
    EQ,
    BAND,
    BOR,
    BNOT,
    VAR,
    IDENTIFIER,
    NULL,
    COMMENT,
    TRUE,
    FALSE,
    NUMBER,
    STRING,
    FUNCTION,
    IF,
    ELSE,
    THROW,
    RETURN,
    CONTINUE,
    BREAK,
    FOR,
    NEWLINE,
    EOF,
    NEW,
    CLASS,
    EXTENDS,
    TRY,
    CATCH,
}

static KEYWORDS: Map<&'static str, TokenKind> = phf_map! {
    "var" => TokenKind::VAR,
    "true" => TokenKind::TRUE,
    "false" => TokenKind::FALSE,
    "null" => TokenKind::NULL,
    "function" => TokenKind::FUNCTION,
    "if" => TokenKind::IF,
    "else" => TokenKind::ELSE,
    "throw" => TokenKind::THROW,
    "return" => TokenKind::RETURN,
    "continue" => TokenKind::CONTINUE,
    "break" => TokenKind::BREAK,
    "for" => TokenKind::FOR,
    "try" => TokenKind::TRY,
    "catch" => TokenKind::CATCH,
    "new" => TokenKind::NEW,
    "class" => TokenKind::CLASS,
    "extends" => TokenKind::EXTENDS,
};

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub value: String,
    pub line: usize,
    pub column: usize,
}
