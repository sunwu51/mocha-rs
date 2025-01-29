use super::{
    ArrayDeclarationAstNode, AstNode, BlockStatement, BooleanAstNode, BreakStatement,
    ClassStatement, ContinueStatement, EmptyStatement, ExpressionStatement, ForStatement,
    FunctionArgsAstNode, FunctionCallAstNode, FunctionDeclarationAstNode, GroupAstNode,
    IdentifierAstNode, IfStatement, IndexAstNode, InfixOperatorAstNode,
    MapObjectDeclarationAstNode, NewAstNode, NullAstNode, NumberAstNode, OperatorAstNode,
    PostfixOperatorAstNode, PrefixOperatorAstNode, ReturnStatement, Statement, StringAstNode,
    ThrowStatement, TryCatchStatement, VarStatement, POSTFIX_PRECEDENCE_MAP, PRECENDENCE_MAP,
    PREFIX_PRECEDENCE_MAP,
};
use crate::lexer::Token;
use crate::lexer::TokenKind;
use crate::lexer::TokenKind::*;
use std::collections::HashMap;

pub struct Parser {
    tokens: Vec<Token>,
    cursor: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, cursor: 0 }
    }
    pub fn parse(&mut self) -> Vec<Statement> {
        let mut statements = Vec::new();
        while self.cursor < self.tokens.len() {
            let statement = self.parse_statement();
            match statement {
                Statement::None() => break,
                Statement::Empty(_) => continue,
                _ => statements.push(statement),
            }
        }
        statements
    }

    fn parse_statement(&mut self) -> Statement {
        if self.cursor >= self.tokens.len() {
            return Statement::None();
        }
        let token = &self.tokens[self.cursor];
        if token.kind == SEMICOLON {
            self.cursor += 1;
            Statement::Empty(EmptyStatement {
                token: token.clone(),
            })
        } else if token.kind == EOF || token.kind == RBRACE || token.kind == RPAREN {
            return Statement::None();
        } else if token.kind == VAR {
            return Statement::Var(self.parse_var_statement());
        } else if token.kind == RETURN {
            return Statement::Return(self.parse_return_statement());
        } else if token.kind == THROW {
            return Statement::Throw(self.parse_throw_statement());
        } else if token.kind == LBRACE {
            return Statement::Block(self.parse_block_statement());
        } else if token.kind == IF {
            return Statement::If(self.parse_if_statement());
        } else if token.kind == FOR {
            return Statement::For(self.parse_for_statement());
        } else if token.kind == BREAK {
            self.cursor += 1;
            return Statement::Break(BreakStatement {
                token: token.clone(),
            });
        } else if token.kind == CONTINUE {
            self.cursor += 1;
            return Statement::Continue(ContinueStatement {
                token: token.clone(),
            });
        } else if token.kind == CLASS {
            return Statement::Class(self.parse_class_statement());
        } else if token.kind == TRY {
            return Statement::TryCatch(self.parse_try_catch_statement());
        } else if token.kind == COMMENT {
            let res = Statement::Empty(EmptyStatement {
                token: token.clone(),
            });
            self.cursor += 1;
            return res;
        } else {
            return Statement::Exp(self.parse_expression_statement());
        }
    }
    fn parse_var_statement(&mut self) -> VarStatement {
        assert_eq(self.tokens[self.cursor].kind, VAR, None);
        let token = self.tokens[self.cursor].clone();
        self.cursor += 1;
        let name_token = self.tokens[self.cursor].clone();
        assert_eq(name_token.kind, IDENTIFIER, None);
        self.cursor += 1;
        assert_eq(self.tokens[self.cursor].kind, ASSIGN, None);
        let name = IdentifierAstNode { token: name_token };
        self.cursor += 1;
        let value = self.parse_expression();
        if self.tokens[self.cursor - 1].kind != RBRACE {
            assert(self.tokens[self.cursor].kind == EOF || self.tokens[self.cursor].kind == SEMICOLON || self.tokens[self.cursor - 1].kind == RBRACE, "Expect ';' or EOF");
            self.cursor += 1;
        }
        VarStatement { token, name, value }
    }

    fn parse_return_statement(&mut self) -> ReturnStatement {
        let token = self.tokens[self.cursor].clone();
        self.assert_token_type(
            self.cursor,
            RETURN,
            "return statement should starts with return",
        );
        self.cursor += 1;
        let value = self.parse_expression();
        ReturnStatement { token, value }
    }

    fn parse_throw_statement(&mut self) -> ThrowStatement {
        let token = self.tokens[self.cursor].clone();
        self.assert_token_type(
            self.cursor,
            THROW,
            "throw statement should starts with throw",
        );
        self.cursor += 1;
        let value = self.parse_expression();
        ThrowStatement { token, value }
    }

    fn parse_expression_statement(&mut self) -> ExpressionStatement {
        let token = self.tokens[self.cursor].clone();
        let expression = self.parse_expression();
        assert(self.tokens[self.cursor].kind == EOF || self.tokens[self.cursor].kind == SEMICOLON, "Expect ';' or EOF");
        self.cursor += 1;
        ExpressionStatement { token, expression }
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let start_token = self.tokens[self.cursor].clone();
        self.assert_token_type(self.cursor, LBRACE, "brace not open for block statement");
        self.cursor += 1;
        let result = BlockStatement {
            token: start_token,
            statements: self.parse(),
        };
        self.assert_token_type(self.cursor, RBRACE, "brace not close for block statement");
        self.cursor += 1;
        result
    }

    fn parse_if_statement(&mut self) -> IfStatement {
        let start_token = self.tokens[self.cursor].clone();
        self.assert_token_type(self.cursor, IF, "if statement need a if");
        self.cursor += 1;
        self.assert_token_type(self.cursor, LPAREN, "if statement need a LPAREN follow if");
        self.cursor += 1;
        let condition = self.parse_expression();
        self.assert_token_type(
            self.cursor,
            RPAREN,
            "if statement need a RPAREN follow condition",
        );
        self.cursor += 1;
        let if_body = self.parse_block_statement();
        let else_body = if self.tokens[self.cursor].kind == ELSE {
            self.cursor += 1;
            Some(self.parse_block_statement())
        } else {
            None
        };
        IfStatement {
            token: start_token,
            condition,
            if_body,
            else_body,
        }
    }

    fn parse_for_statement(&mut self) -> ForStatement {
        let start_token = self.tokens[self.cursor].clone();
        self.assert_token_type(self.cursor, FOR, "for statement need a for"); // for
        self.cursor += 1;
        self.assert_token_type(
            self.cursor,
            LPAREN,
            "for statement need a LPAREN follow for",
        ); // (
        self.cursor += 1;
        let init = self.parse_statement(); // init
        self.assert_token_type(
            self.cursor - 1,
            SEMICOLON,
            "for statement error need a SEMICOLON after init",
        ); // ;
        let condition = self.parse_statement(); // condition
        self.assert_token_type(
            self.cursor - 1,
            SEMICOLON,
            "for statement error need a SEMICOLON after condition",
        ); //;
        let step = self.parse_expression(); // step
        self.assert_token_type(
            self.cursor,
            RPAREN,
            "for statement need a RPAREN follow condition",
        ); // )
        self.cursor += 1;
        let body = self.parse_block_statement(); // body
        ForStatement {
            token: start_token,
            init: Box::new(init),
            condition: Box::new(condition),
            step: if step.is_none() { None } else { Some(step) },
            body,
        }
    }

    fn parse_try_catch_statement(&mut self) -> TryCatchStatement {
        let start_token = self.tokens[self.cursor].clone();
        self.assert_token_type(self.cursor, TRY, "try statement need a try"); // try
        self.cursor += 1;
        let body = self.parse_block_statement(); // body
        self.assert_token_type(self.cursor, CATCH, "try statement need a catch"); // catch
        self.cursor += 1;
        self.assert_token_type(self.cursor, LPAREN, "parse catch error");
        self.cursor += 1;
        let AstNode::Ident(identifier) = self.parse_expression() else {
            eprintln!("parse catch error");
            panic!("parse catch error");
        }; // identifier
        self.assert_token_type(self.cursor, RPAREN, "parse catch error");
        self.cursor += 1;
        let catch_body = self.parse_block_statement(); // catch body
        TryCatchStatement {
            token: start_token,
            body,
            identifier,
            catch_body,
        }
    }

    fn parse_class_statement(&mut self) -> ClassStatement {
        let token = self.tokens[self.cursor].clone();
        self.assert_token_type(
            self.cursor,
            CLASS,
            "class statement should start with class keyword",
        ); // class
        self.cursor += 1;
        self.assert_token_type(
            self.cursor,
            IDENTIFIER,
            "class statement need a IDENTIFIER for class name",
        );
        let AstNode::Ident(class_name) = self.parse_expression() else {
            eprintln!("name should be identifier");
            panic!("name should be identifier")
        }; // name
        let parent = if self.tokens[self.cursor].kind == EXTENDS {
            // extends
            self.cursor += 1;
            self.assert_token_type(
                self.cursor,
                IDENTIFIER,
                "class statement need a IDENTIFIER for parent class name",
            );
            self.cursor += 1;
            Some(IdentifierAstNode {
                token: self.tokens[self.cursor - 1].clone(),
            }) // parent
        } else {
            None
        };
        self.assert_token_type(
            self.cursor,
            LBRACE,
            "class statement need a LBRACE for body",
        );
        self.cursor += 1;

        // 类中的属性都是 ident;或ident=expr;形式
        let mut fields = vec![];
        let mut methods = HashMap::new();

        let mut constructor = AstNode::None();

        while self.tokens[self.cursor].kind != RBRACE {
            self.assert_token_type(
                self.cursor,
                IDENTIFIER,
                "class statement need a IDENTIFIER for property name",
            );
            let mut assign = self.parse_expression();
            // 如果只有一个ident例如a，需要把他转换成a=null
            match &assign {
                AstNode::Ident(ident) => {
                    let temp = AstNode::Operator(OperatorAstNode::InfixOperatorAstNode(
                        InfixOperatorAstNode {
                            token: Token {
                                kind: ASSIGN,
                                value: "=".to_string(),
                                line: ident.token.line,
                                column: ident.token.column,
                            },
                            left: Some(Box::new(AstNode::Ident(ident.clone()))),
                            right: Some(Box::new(AstNode::Null(NullAstNode {
                                token: Token {
                                    kind: NULL,
                                    value: "null".to_string(),
                                    line: ident.token.line,
                                    column: ident.token.column,
                                },
                            }))),
                            precedence: *PRECENDENCE_MAP.get("=").unwrap(),
                        },
                    ));
                    assign = temp;
                }
                AstNode::Operator(op) => match &op {
                    OperatorAstNode::InfixOperatorAstNode(p) => {
                        assert_eq(p.token.kind, ASSIGN, None);
                    }
                    _ => {
                        eprintln!("only assign is allowed in class body");
                        panic!("only assign is allowed in class body")
                    },
                },
                _ => {
                    eprintln!("only assign is allowed in class body");
                    panic!("only assign is allowed in class body")
                },
            }

            if let AstNode::Operator(op_node) = assign {
                // 把字段的赋值 age=1 改为 this.age = 1
                match op_node {
                    OperatorAstNode::InfixOperatorAstNode(mut infix) => {
                        if infix.token.kind == ASSIGN {
                            if let right = infix.right.as_ref().unwrap() {
                                match right.as_ref() {
                                    AstNode::FunctionDeclaration(function_declaration) => {
                                        let name = infix.left.as_ref().unwrap().to_string();
                                        if name == "constructor" {
                                            constructor = AstNode::FunctionDeclaration(
                                                function_declaration.clone(),
                                            );
                                        } else {
                                            methods.insert(name, function_declaration.clone());
                                        }
                                    }
                                    _ => {
                                        let mut point = AstNode::Operator(
                                            OperatorAstNode::InfixOperatorAstNode(
                                                crate::parser::InfixOperatorAstNode {
                                                    token: Token {
                                                        kind: POINT,
                                                        value: ".".to_string(),
                                                        line: infix.token.line,
                                                        column: infix.token.column,
                                                    },
                                                    left: Some(Box::new(AstNode::Ident(
                                                        IdentifierAstNode {
                                                            token: Token {
                                                                kind: IDENTIFIER,
                                                                value: "this".to_string(),
                                                                line: infix.token.line,
                                                                column: infix.token.column,
                                                            },
                                                        },
                                                    ))),
                                                    right: infix.left.clone(),
                                                    precedence: *PRECENDENCE_MAP.get(".").unwrap(),
                                                },
                                            ),
                                        );
                                        infix.left = Some(Box::new(point));
                                        fields.push(Statement::Exp(ExpressionStatement {
                                            token: infix.token.clone(),
                                            expression: AstNode::Operator(
                                                OperatorAstNode::InfixOperatorAstNode(infix),
                                            ),
                                        }));
                                    }
                                }
                            }
                        } else {
                            eprintln!("Unexpected token: {:?}", infix.token.kind);
                            panic!("Unexpected token: {:?}", infix.token.kind);
                        }
                    }
                    _ => {
                        eprintln!("Unexpected token: {:?}", op_node.get_token().kind);
                        panic!("Unexpected token: {:?}", op_node.get_token().kind)
                    },
                }
            } else {
                eprintln!("only assign is allowed in class body");
                panic!("only assign is allowed in class body")
            };

            if self.tokens[self.cursor].kind == SEMICOLON {
                self.cursor += 1;
            }
        }
        self.cursor += 1;

        match constructor {
            AstNode::FunctionDeclaration(mut function_declaration) => {
                // super() => super.constructor()
                let first = function_declaration.body.statements[0].clone();
                if let Statement::Exp(ExpressionStatement {
                    expression:
                        AstNode::Operator(OperatorAstNode::InfixOperatorAstNode(mut super_call_node)),
                    token,
                }) = first
                {
                    let s = super_call_node.left.unwrap();
                    if s.to_string() == "super" {
                        super_call_node.left = Some(Box::from(AstNode::Operator(
                            OperatorAstNode::InfixOperatorAstNode(InfixOperatorAstNode {
                                token: Token {
                                    kind: POINT,
                                    value: ".".to_string(),
                                    line: token.line,
                                    column: token.column,
                                },
                                left: Some(s.clone()),
                                right: Some(Box::new(AstNode::Ident(IdentifierAstNode {
                                    token: Token {
                                        kind: IDENTIFIER,
                                        value: "constructor".to_string(),
                                        line: token.line,
                                        column: token.column,
                                    },
                                }))),
                                precedence: *PRECENDENCE_MAP.get(".").unwrap(),
                            }),
                        )));
                        function_declaration.body.statements.remove(0);
                        function_declaration.body.statements.insert(
                            0,
                            Statement::Exp(ExpressionStatement {
                                expression: AstNode::Operator(
                                    OperatorAstNode::InfixOperatorAstNode(super_call_node.clone()),
                                ),
                                token: token.clone(),
                            }),
                        );
                    } else {
                        eprintln!("super must be called in first statement of constructor");
                        panic!("super must be called in first statement of constructor");
                    }
                }
                function_declaration.body.statements.append(&mut fields);
                methods.insert("constructor".to_string(), function_declaration);
            }
            AstNode::None() => {
                let left = Some(Box::from(AstNode::Operator(
                    OperatorAstNode::InfixOperatorAstNode(InfixOperatorAstNode {
                        token: Token {
                            kind: POINT,
                            value: ".".to_string(),
                            line: token.line,
                            column: token.column,
                        },
                        left: Some(Box::new(AstNode::Ident(IdentifierAstNode {
                            token: Token {
                                kind: IDENTIFIER,
                                value: "super".to_string(),
                                line: token.line,
                                column: token.column,
                            },
                        }))),
                        right: Some(Box::new(AstNode::Ident(IdentifierAstNode {
                            token: Token {
                                kind: IDENTIFIER,
                                value: "constructor".to_string(),
                                line: token.line,
                                column: token.column,
                            },
                        }))),
                        precedence: *PRECENDENCE_MAP.get(".").unwrap(),
                    }),
                )));
                let super_call = Statement::Exp(ExpressionStatement {
                    token: Token {
                        kind: LPAREN,
                        value: "(".to_string(),
                        line: token.line,
                        column: token.column,
                    },
                    expression: AstNode::Operator(OperatorAstNode::InfixOperatorAstNode(
                        InfixOperatorAstNode {
                            token: Token {
                                kind: LPAREN,
                                value: "(".to_string(),
                                line: token.line,
                                column: token.column,
                            },
                            left: left,
                            right: Some(Box::new(AstNode::FunctionArgs(FunctionArgsAstNode {
                                token: Token {
                                    kind: IDENTIFIER,
                                    value: "constructor".to_string(),
                                    line: token.line,
                                    column: token.column,
                                },
                                args: vec![],
                            }))),
                            precedence: *PRECENDENCE_MAP.get("(").unwrap(),
                        },
                    )),
                });
                let mut function_declaration = FunctionDeclarationAstNode {
                    token: Token {
                        kind: FUNCTION,
                        value: "function".to_string(),
                        line: token.line,
                        column: token.column,
                    },
                    params: vec![],
                    body: BlockStatement {
                        token: Token {
                            kind: LBRACE,
                            value: "{".to_string(),
                            line: 0,
                            column: 0,
                        },
                        statements: vec![super_call],
                    },
                };

                function_declaration.body.statements.append(&mut fields);
                methods.insert("constructor".to_string(), function_declaration);
            }
            _ => {
                eprintln!("constructor should be a funciton");
                panic!("constructor should be a funciton")
            },
        }
        ClassStatement {
            token,
            name: class_name,
            parent,
            methods: methods.into_iter().collect(),
        }
    }

    fn parse_expression(&mut self) -> AstNode {
        let mut stack: Vec<OperatorAstNode> = vec![];
        let mut mid: AstNode = AstNode::None();
        loop {
            let token = &self.tokens[self.cursor];
            let stack_top_precedence = if stack.is_empty() {
                0
            } else {
                stack[stack.len() - 1].get_precedence()
            };
            if mid.is_none() {
                // 如果是前缀运算符，不需要设置left，直接塞到stack
                if let Some(precedence) = PREFIX_PRECEDENCE_MAP.get(&token.value) {
                    if *precedence > 0 {
                        stack.push(OperatorAstNode::PrefixOperatorAstNode(
                            PrefixOperatorAstNode::new(token.clone()),
                        ));
                        self.cursor += 1;
                        continue;
                    }
                }
                mid = self.next_unary_node();
            }
            let mut op_node = self.get_eof_or_operate_node(&self.tokens[self.cursor]);
            if op_node.get_precedence() == 0 && stack_top_precedence == 0 {
                return mid;
            }
            let need_pop = if op_node.get_token().kind == ASSIGN {
                op_node.get_precedence() < stack_top_precedence
            } else {
                op_node.get_precedence() <= stack_top_precedence
            };

            if need_pop {
                if let Some(mut top) = stack.pop() {
                    match &mut top {
                        OperatorAstNode::PrefixOperatorAstNode(ref mut s) => {
                            s.right = Some(Box::new(mid.clone()))
                        }
                        OperatorAstNode::InfixOperatorAstNode(ref mut s) => {
                            s.right = Some(Box::new(mid.clone()))
                        }
                        OperatorAstNode::PostfixOperatorAstNode(_) => {
                            eprintln!("PostfixOperatorAstNode should not be in stack");
                            panic!("PostfixOperatorAstNode should not be in stack")
                        }
                    }
                    mid = AstNode::Operator(top);
                }
            } else {
                match &mut op_node {
                    OperatorAstNode::PrefixOperatorAstNode(_) => {
                        eprintln!("PrefixOperatorAstNode should not be here");
                        panic!("PrefixOperatorAstNode should not be here")
                    }
                    OperatorAstNode::InfixOperatorAstNode(s) => {
                        s.left = Some(Box::new(mid.clone()))
                    }
                    OperatorAstNode::PostfixOperatorAstNode(s) => {
                        s.left = Some(Box::new(mid.clone()))
                    }
                }
                // 如果是后缀运算符，不需要设置right直接continue
                match &op_node {
                    OperatorAstNode::PostfixOperatorAstNode(postfix) => {
                        mid = AstNode::Operator(OperatorAstNode::PostfixOperatorAstNode(
                            postfix.clone(),
                        ));
                        self.cursor += 1;
                        continue;
                    }
                    _ => (),
                }

                if op_node.get_token().kind != LPAREN && op_node.get_token().kind != LBRACKET {
                    self.cursor += 1;
                } else {
                    match &mut op_node {
                        OperatorAstNode::PrefixOperatorAstNode(ref mut s) => s.precedence = 999,
                        OperatorAstNode::InfixOperatorAstNode(ref mut s) => s.precedence = 999,
                        OperatorAstNode::PostfixOperatorAstNode(_) => {
                            eprintln!("PostfixOperatorAstNode should  not be in stack");
                            panic!("PostfixOperatorAstNode should  not be in stack")
                        }
                    }
                }
                stack.push(op_node);
                mid = AstNode::None();
            }
        }
    }
    fn next_unary_node(&mut self) -> AstNode {
        match self.tokens[self.cursor].kind {
            NUMBER => {
                let res = AstNode::Number(NumberAstNode {
                    token: self.tokens[self.cursor].clone(),
                });
                self.cursor += 1;
                res
            }
            STRING => {
                let res = AstNode::Str(StringAstNode {
                    token: self.tokens[self.cursor].clone(),
                });
                self.cursor += 1;
                res
            }
            TRUE | FALSE => {
                let res = AstNode::Boolean(BooleanAstNode {
                    token: self.tokens[self.cursor].clone(),
                });
                self.cursor += 1;
                res
            }
            NULL => {
                let res = AstNode::Null(NullAstNode {
                    token: self.tokens[self.cursor].clone(),
                });
                self.cursor += 1;
                res
            }
            IDENTIFIER => {
                let res = AstNode::Ident(IdentifierAstNode {
                    token: self.tokens[self.cursor].clone(),
                });
                self.cursor += 1;
                res
            }
            LPAREN => {
                // 函数入参
                let start_token = self.tokens[self.cursor].clone();
                if self.cursor - 1 >= 0
                    && (self.tokens[self.cursor - 1].kind == IDENTIFIER
                        || self.tokens[self.cursor - 1].kind == RBRACE
                        || self.tokens[self.cursor - 1].kind == RPAREN)
                {
                    self.cursor += 1;
                    let mut args = vec![];
                    while self.tokens[self.cursor].kind != RPAREN {
                        args.push(self.parse_expression());
                        if self.tokens[self.cursor].kind == COMMA {
                            self.cursor += 1;
                        }
                    }
                    self.assert_token_type(self.cursor, RPAREN, "function args must end with )");
                    self.cursor += 1;
                    AstNode::FunctionArgs(FunctionArgsAstNode {
                        token: start_token,
                        args,
                    })
                }
                // 分组
                else {
                    let start_token = self.tokens[self.cursor].clone();
                    // 递归解析(后面的即可，因为遇到)的时候，parseExpression无法识别，就会结束解析
                    self.cursor += 1;
                    // GroupAstNode其实可有可无
                    let res = AstNode::Group(GroupAstNode {
                        token: start_token,
                        expression: Box::new(self.parse_expression()),
                    });
                    self.assert_token_type(self.cursor, RPAREN, "group not closed");
                    self.cursor += 1;
                    res
                }
            }
            LBRACKET => {
                // []用于数组声明和数组索引
                // 数组索引
                let start_token = self.tokens[self.cursor].clone();
                if self.cursor - 1 >= 0
                    && (self.tokens[self.cursor - 1].kind == IDENTIFIER
                        || self.tokens[self.cursor - 1].kind == RBRACKET
                        || self.tokens[self.cursor - 1].kind == RPAREN
                        || self.tokens[self.cursor - 1].kind == STRING)
                {
                    self.cursor += 1;
                    let index = self.parse_expression();
                    self.assert_token_type(self.cursor, RBRACKET, "array index must end with ]");
                    self.cursor += 1;
                    AstNode::Index(IndexAstNode {
                        token: start_token,
                        index: Box::new(index),
                    })
                } else {
                    // 数组声明
                    self.cursor += 1;
                    let mut items = vec![];
                    while self.tokens[self.cursor].kind != RBRACKET {
                        let item = self.parse_expression();
                        items.push(item);
                        if self.tokens[self.cursor].kind == COMMA {
                            self.cursor += 1;
                        }
                    }
                    self.assert_token_type(
                        self.cursor,
                        RBRACKET,
                        "array declaration must end with ]",
                    );
                    self.cursor += 1;
                    AstNode::ArrayDeclaration(ArrayDeclarationAstNode {
                        token: start_token,
                        items,
                    })
                }
            }
            LBRACE => {
                // {}用于声明普通对象
                let mut items = vec![];
                let start_token = self.tokens[self.cursor].clone();
                self.cursor += 1;
                while self.tokens[self.cursor].kind != RBRACE {
                    let key = self.parse_expression();
                    match &key {
                        AstNode::Ident(_) => (),
                        AstNode::Str(_) => (),
                        _ => {
                            eprintln!("object declaration must have identifier as key");
                            panic!("object declaration must have identifier as key")
                        },
                    }
                    self.assert_token_type(self.cursor, COLON, "object declaration must have :");
                    self.cursor += 1;
                    let value = self.parse_expression();
                    items.push((key, value));
                    if self.tokens[self.cursor].kind == COMMA {
                        self.cursor += 1;
                    }
                }
                self.assert_token_type(self.cursor, RBRACE, "object declaration must end with }");
                self.cursor += 1;
                AstNode::MapObjectDeclaration(MapObjectDeclarationAstNode {
                    token: start_token,
                    pairs: items,
                })
            }
            FUNCTION => {
                let start_token = self.tokens[self.cursor].clone();
                self.assert_token_type(self.cursor + 1, LPAREN, "function need a lparen");
                self.cursor += 2;
                let mut params = vec![];
                while self.tokens[self.cursor].kind != RPAREN {
                    self.assert_token_type(
                        self.cursor,
                        IDENTIFIER,
                        "function declaration must have identifier as param",
                    );
                    params.push(IdentifierAstNode {
                        token: self.tokens[self.cursor].clone(),
                    });
                    self.cursor += 1;
                    if self.tokens[self.cursor].kind == COMMA {
                        self.cursor += 1;
                    }
                }
                self.cursor += 1;
                let body = self.parse_block_statement();
                AstNode::FunctionDeclaration(FunctionDeclarationAstNode {
                    token: start_token,
                    params,
                    body,
                })
            }
            NEW => {
                let class_name_token = self.tokens[self.cursor].clone();
                self.assert_token_type(
                    self.cursor + 1,
                    IDENTIFIER,
                    "new should be followed by a identifier",
                );
                self.cursor += 1;
                let class_name = IdentifierAstNode {
                    token: self.tokens[self.cursor].clone(),
                };
                self.cursor += 1;
                self.assert_token_type(self.cursor, LPAREN, "class create need a lparen");
                self.cursor += 1;
                let mut args = vec![];
                while self.tokens[self.cursor].kind != RPAREN {
                    args.push(self.parse_expression());
                    if self.tokens[self.cursor].kind == COMMA {
                        self.cursor += 1;
                    }
                }
                self.cursor += 1;
                AstNode::New(NewAstNode {
                    token: class_name_token,
                    class: class_name,
                    args,
                })
            }
            _ => {

                eprintln!(
                    "unexpected token in nextUnary: {:?}",
                    self.tokens[self.cursor].kind
                );
                panic!(
                    "unexpected token in nextUnary: {:?}",
                    self.tokens[self.cursor].kind
                )
            },
        }
    }
    fn get_eof_or_operate_node(&self, token: &Token) -> OperatorAstNode {
        let eof = OperatorAstNode::InfixOperatorAstNode(InfixOperatorAstNode::new(Token {
            kind: EOF,
            value: "EOF".to_string(),
            line: 0,
            column: 0,
        }));
        if let Some(precedence) = PRECENDENCE_MAP.get(&token.value) {
            return OperatorAstNode::InfixOperatorAstNode(InfixOperatorAstNode::new(token.clone()));
        }
        if let Some(precedence) = POSTFIX_PRECEDENCE_MAP.get(&token.value) {
            return OperatorAstNode::PostfixOperatorAstNode(PostfixOperatorAstNode::new(
                token.clone(),
            ));
        }
        eof
    }

    fn assert_token_type(&self, cursor: usize, kind: TokenKind, msg: &str) {
        if self.tokens[cursor].kind != kind {
            eprintln!("{} at line: {}:{}",
                      msg, self.tokens[cursor].line, self.tokens[cursor].column);
            panic!(
                "{} at line: {}:{}",
                msg, self.tokens[cursor].line, self.tokens[cursor].column
            );
        }
    }
}

fn assert(cond: bool, msg: &str) {
    if !cond {
        eprintln!("{}", msg);
        panic!("{}", msg);
    }
}

fn assert_eq(left: TokenKind, right: TokenKind, msg: Option<&'static str>) {
    if left != right {
        if msg.is_some() {
            eprintln!("{:?} != {:?}, {}", left, right, msg.unwrap());
            panic!("{:?} != {:?}, {}", left, right, msg.unwrap());
        }
        eprintln!("{:?} != {:?}", left, right);
        panic!("{:?} != {:?}", left, right);
    }
}