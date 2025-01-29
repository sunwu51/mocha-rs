use serde_json::Value;
use regex::Regex;
use crate::lexer::{Token, TokenKind, KEYWORDS};

pub fn lex(input: &str) -> Vec<Token> {
    let input: Vec<char> = input.chars().collect();
    let len = input.len();
    let mut position = 0;
    let mut cur_line_start = position;
    let mut line = 1;
    let mut tokens = vec![];
    while position < len {
        // println!("process position:{} value:{}", position, input[position]);
        match input[position] {
            '=' => {
                if input[position + 1] == '=' {
                    tokens.push(Token {
                        kind: TokenKind::EQ,
                        value: "==".to_string(),
                        line,
                        column: position - cur_line_start + 1,
                    });
                    position += 2;
                } else {
                    tokens.push(Token {
                        kind: TokenKind::ASSIGN,
                        value: "=".to_string(),
                        line,
                        column: position - cur_line_start + 1,
                    });
                    position += 1;
                }
            }
            '(' => {
                tokens.push(Token {
                    kind: TokenKind::LPAREN,
                    value: "(".to_string(),
                    line,
                    column: position - cur_line_start + 1,
                });
                position += 1;
            }
            ')' => {
                tokens.push(Token {
                    kind: TokenKind::RPAREN,
                    value: ")".to_string(),
                    line,
                    column: position - cur_line_start + 1,
                });
                position += 1;
            }
            '[' => {
                tokens.push(Token {
                    kind: TokenKind::LBRACKET,
                    value: "[".to_string(),
                    line,
                    column: position - cur_line_start + 1,
                });
                position += 1;
            }
            ']' => {
                tokens.push(Token {
                    kind: TokenKind::RBRACKET,
                    value: "]".to_string(),
                    line,
                    column: position - cur_line_start + 1,
                });
                position += 1;
            }
            '{' => {
                tokens.push(Token {
                    kind: TokenKind::LBRACE,
                    value: "{".to_string(),
                    line,
                    column: position - cur_line_start + 1,
                });
                position += 1;
            }
            '}' => {
                tokens.push(Token {
                    kind: TokenKind::RBRACE,
                    value: "}".to_string(),
                    line,
                    column: position - cur_line_start + 1,
                });
                position += 1;
            }
            '+' => {
                if input[position + 1] == '+' {
                    tokens.push(Token {
                        kind: TokenKind::INCREMENT,
                        value: "++".to_string(),
                        line,
                        column: position - cur_line_start + 1,
                    });
                    position += 2;
                } else {
                    tokens.push(Token {
                        kind: TokenKind::PLUS,
                        value: "+".to_string(),
                        line,
                        column: position - cur_line_start + 1,
                    });
                    position += 1;
                }
            }
            '-' => {
                if input[position + 1] == '-' {
                    tokens.push(Token {
                        kind: TokenKind::DECREMENT,
                        value: "--".to_string(),
                        line,
                        column: position - cur_line_start + 1,
                    });
                    position += 2;
                } else {
                    tokens.push(Token {
                        kind: TokenKind::MINUS,
                        value: "-".to_string(),
                        line,
                        column: position - cur_line_start + 1,
                    });
                    position += 1;
                }
            }
            '*' => {
                tokens.push(Token {
                    kind: TokenKind::MULTIPLY,
                    value: "*".to_string(),
                    line,
                    column: position - cur_line_start + 1,
                });
                position += 1;
            }
            '/' => {
                if input[position + 1] == '/' {
                    let start = position;
                    while input[2 + position] != '\n' && position + 1 < input.len() {
                        position += 1;
                    }
                    tokens.push(Token {
                        kind: TokenKind::COMMENT,
                        value: String::from_iter(&input[start..position + 2]),
                        line,
                        column: position - cur_line_start + 1,
                    });
                    position += 2;
                } else {
                    tokens.push(Token {
                        kind: TokenKind::DIVIDE,
                        value: "/".to_string(),
                        line,
                        column: position - cur_line_start + 1,
                    });
                    position += 1;
                }
            }
            '%' => {
                tokens.push(Token {
                    kind: TokenKind::MODULUS,
                    value: "%".to_string(),
                    line,
                    column: position - cur_line_start + 1,
                });
                position += 1;
            }
            '#' => {
                while input[position + 1] != '\n' && position + 1 < input.len() {
                    position += 1;
                }
                tokens.push(Token {
                    kind: TokenKind::COMMENT,
                    value: String::from_iter(&input[position..position + 1]),
                    line,
                    column: position - cur_line_start + 1,
                });
                position += 1;
            }
            '.' => {
                tokens.push(Token {
                    kind: TokenKind::POINT,
                    value: ".".to_string(),
                    line,
                    column: position - cur_line_start + 1,
                });
                position += 1;
            }

            '~' => {
                tokens.push(Token {
                    kind: TokenKind::BNOT,
                    value: "~".to_string(),
                    line,
                    column: position - cur_line_start + 1,
                });
                position += 1;
            }
            '|' => {
                if input[position + 1] == '|' {
                    tokens.push(Token {
                        kind: TokenKind::OR,
                        value: "||".to_string(),
                        line,
                        column: position - cur_line_start + 1,
                    });
                    position += 2;
                } else {
                    tokens.push(Token {
                        kind: TokenKind::BOR,
                        value: "|".to_string(),
                        line,
                        column: position - cur_line_start + 1,
                    });
                    position += 1;
                }
            }
            '&' => {
                if input[position + 1] == '&' {
                    tokens.push(Token {
                        kind: TokenKind::AND,
                        value: "&&".to_string(),
                        line,
                        column: position - cur_line_start + 1,
                    });
                    position += 2;
                } else {
                    tokens.push(Token {
                        kind: TokenKind::BAND,
                        value: "&".to_string(),
                        line,
                        column: position - cur_line_start + 1,
                    });
                    position += 1;
                }
            }
            '!' => {
                if input[position + 1] == '=' {
                    tokens.push(Token {
                        kind: TokenKind::NEQ,
                        value: "!=".to_string(),
                        line,
                        column: position - cur_line_start + 1,
                    });
                    position += 2;
                } else {
                    tokens.push(Token {
                        kind: TokenKind::NOT,
                        value: "!".to_string(),
                        line,
                        column: position - cur_line_start + 1,
                    });
                    position += 1;
                }
            }
            '<' => {
                if input[position + 1] == '=' {
                    tokens.push(Token {
                        kind: TokenKind::LTE,
                        value: "<=".to_string(),
                        line,
                        column: position - cur_line_start + 1,
                    });
                    position += 2;
                } else {
                    tokens.push(Token {
                        kind: TokenKind::LT,
                        value: "<".to_string(),
                        line,
                        column: position - cur_line_start + 1,
                    });
                    position += 1;
                }
            }
            '>' => {
                if input[position + 1] == '=' {
                    tokens.push(Token {
                        kind: TokenKind::GTE,
                        value: ">=".to_string(),
                        line,
                        column: position - cur_line_start + 1,
                    });
                    position += 2;
                } else {
                    tokens.push(Token {
                        kind: TokenKind::GT,
                        value: ">".to_string(),
                        line,
                        column: position - cur_line_start + 1,
                    });
                    position += 1;
                }
            }
            ':' => {
                tokens.push(Token {
                    kind: TokenKind::COLON,
                    value: ":".to_string(),
                    line,
                    column: position - cur_line_start + 1,
                });
                position += 1;
            }
            ';' => {
                tokens.push(Token {
                    kind: TokenKind::SEMICOLON,
                    value: ";".to_string(),
                    line,
                    column: position - cur_line_start + 1,
                });
                position += 1;
            }
            ',' => {
                tokens.push(Token {
                    kind: TokenKind::COMMA,
                    value: ",".to_string(),
                    line,
                    column: position - cur_line_start + 1,
                });
                position += 1;
            }
            '\n' => {
                line += 1;
                position += 1;
                cur_line_start = position;
            }
            ' ' => {
                position += 1;
            }
            '\t' => {
                position += 1;
            }
            '\r' => {
                position += 1;
            }
            '\'' => {
                let start = position;
                loop {
                    position += 1;
                    // 字符中间不能有回车
                    if position >= len {
                        let err_info = format!("Unterminated string at line: {:b}:{:b}",
                                               line,
                                               position - cur_line_start + 1);
                        eprintln!("{}", err_info);
                        panic!("{}", err_info);
                    }
                    if input[position] == '\n' {
                        let err_info = format!("Enter is not allowed in string at line: {:b}:{:b}",
                                               line,
                                               (position - cur_line_start + 1));
                        eprintln!("{}", err_info);
                        panic!("{}", err_info);
                    }
                    if input[position] == '\'' && input[position - 1] != '\\' {
                        let s = escape_quotes(&String::from_iter(&input[start + 1..position]));
                        tokens.push(Token {
                            kind: TokenKind::STRING,
                            value: unescape(&s),
                            line,
                            column: position - cur_line_start + 1,
                        });
                        position += 1;
                        break;
                    }
                }
            }
            '"' => {
                let start = position;
                loop {
                    position += 1;
                    // 字符中间不能有回车
                    if position >= len {
                        let err_info = format!("Unterminated string at line: {:b}:{:b}",
                                               line,
                                               position - cur_line_start + 1);
                        eprintln!("{}", err_info);
                        panic!("{}", err_info);
                    }
                    if input[position] == '\n' {
                        let err_info = format!("Enter is not allowed in string at line: {:b}:{:b}",
                                               line,
                                               (position - cur_line_start + 1));
                        eprintln!("{}", err_info);
                        panic!("{}", err_info);
                    }
                    if input[position] == '"' && input[position - 1] != '\\' {
                        tokens.push(Token {
                            kind: TokenKind::STRING,
                            value: unescape(&String::from_iter(&input[start + 1..position])),
                            line,
                            column: position - cur_line_start + 1,
                        });
                        position += 1;
                        break;
                    }
                }
            }
            '0'..='9' => {
                let start = position;
                while (input[position] >= '0' && input[position] <= '9') || input[position] == '.' {
                    position += 1;
                }
                tokens.push(Token {
                    kind: TokenKind::NUMBER,
                    value: String::from_iter(&input[start..position]),
                    line,
                    column: position - cur_line_start + 1,
                });
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let start = position;
                while input[position] >= 'a' && input[position] <= 'z'
                    || input[position] >= 'A' && input[position] <= 'Z'
                    || input[position] >= '0' && input[position] <= '9'
                    || input[position] == '_'
                {
                    position += 1;
                }
                let value = String::from_iter(&input[start..position]);
                match KEYWORDS.get(&value) {
                    Some(kind) => {
                        tokens.push(Token {
                            kind: *kind,
                            value: value.to_string(),
                            line,
                            column: position - cur_line_start + 1,
                        });
                    }
                    None => {
                        tokens.push(Token {
                            kind: TokenKind::IDENTIFIER,
                            value: value.to_string(),
                            line,
                            column: position - cur_line_start + 1,
                        });
                    }
                }
            }
            _ => {
                let err_info = format!("Invalid character at line: {}:{}",
                                       line,
                                       (position - cur_line_start + 1));
                eprintln!("{}", err_info);
                panic!("{}", err_info);
            }
        }
    }
    tokens.push(Token {
        kind: TokenKind::EOF,
        value: "EOF".to_string(),
        line,
        column: position - cur_line_start + 1,
    });
    return tokens;
}

fn unescape(input: &str) -> String {
    let json_str = format!("\"{}\"", input);
    let value: Value = serde_json::from_str(&json_str).unwrap();
    value.as_str().unwrap_or("").to_string()
}
fn escape_quotes(input: &str) -> String {
    let mut escaped = String::new();
    let mut chars = input.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '"' {
            // Check if the previous character is not a backslash
            if let Some('\\') = escaped.chars().last() {
                escaped.push('\\');
                escaped.push('\\');
                escaped.push(c); // Just append the quote as is
            } else {
                escaped.push('\\');
                escaped.push(c);
            }
        } else {
            escaped.push(c);
        }
    }

    escaped
}

