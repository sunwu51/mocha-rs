pub mod parser;

use crate::lexer::TokenKind::{LBRACKET, LPAREN};
use crate::lexer::{Token, TokenKind};
use as_any::AsAny;
use phf::{phf_map, Map};
use std::ops::Index;

#[derive(Clone, Debug)]
pub enum Statement {
    Var(VarStatement),
    Return(ReturnStatement),
    Throw(ThrowStatement),
    Exp(ExpressionStatement),
    Block(BlockStatement),
    If(IfStatement),
    For(ForStatement),
    Break(BreakStatement),
    Continue(ContinueStatement),
    TryCatch(TryCatchStatement),
    Class(ClassStatement),
    Empty(EmptyStatement),
    None(),
}

impl Statement {
    pub fn is_none(&self) -> bool {
        match self {
            Statement::None() => true,
            _ => false,
        }
    }
    pub fn get_token(&self) -> Token {
        match self {
            Statement::Var(s) => s.token.clone(),
            Statement::Return(s) => s.token.clone(),
            Statement::Throw(s) => s.token.clone(),
            Statement::Exp(s) => s.token.clone(),
            Statement::Block(s) => s.token.clone(),
            Statement::If(s) => s.token.clone(),
            Statement::For(s) => s.token.clone(),
            Statement::Break(s) => s.token.clone(),
            Statement::Continue(s) => s.token.clone(),
            Statement::Class(s) => s.token.clone(),
            Statement::Empty(s) => s.token.clone(),
            Statement::TryCatch(s) => s.token.clone(),
            Statement::None() => Token {
                kind: TokenKind::EOF,
                value: "EOF".to_string(),
                line: 0,
                column: 0,
            },
        }
    }
    pub fn to_string(&self) -> String {
        match self {
            Statement::Var(s) => s.to_string(),
            Statement::Return(s) => s.to_string(),
            Statement::Throw(s) => s.to_string(),
            Statement::Exp(s) => s.to_string(),
            Statement::Block(s) => s.to_string(),
            Statement::If(s) => s.to_string(),
            Statement::For(s) => s.to_string(),
            Statement::Break(s) => s.to_string(),
            Statement::Continue(s) => s.to_string(),
            Statement::Class(s) => s.to_string(),
            Statement::Empty(s) => s.to_string(),
            Statement::TryCatch(s) => s.to_string(),
            Statement::None() => "".to_string(),
        }
    }
}
impl VarStatement {
    fn to_string(&self) -> String {
        format!(
            "var {} = {};",
            self.name.token.value,
            self.value.to_string()
        )
    }
}
impl ReturnStatement {
    fn to_string(&self) -> String {
        format!("return {};", self.value.to_string())
    }
}
impl ThrowStatement {
    fn to_string(&self) -> String {
        format!("throw {};", self.value.to_string())
    }
}
impl ExpressionStatement {
    fn to_string(&self) -> String {
        format!("{};", self.expression.to_string())
    }
}
impl BlockStatement {
    fn to_string(&self) -> String {
        let body = self
            .statements
            .iter()
            .map(|s| format!("\t{}", s.to_string()))
            .collect::<Vec<String>>()
            .join("\n");
        format!("{{\n{}\n}}", body)
    }
}
impl IfStatement {
    fn to_string(&self) -> String {
        let else_body = match &self.else_body {
            Some(else_body) => format!("else {}", else_body.to_string()),
            None => String::new(),
        };
        format!(
            "if ({}) \n {} {}",
            self.condition.to_string(),
            self.if_body.to_string(),
            else_body
        )
    }
}
impl ForStatement {
    fn to_string(&self) -> String {
        let init = match self.init.as_ref() {
            Statement::None() => ";".to_string(),
            _ => format!("{}", self.init.as_ref().to_string()),
        };
        let condition = match self.condition.as_ref() {
            Statement::None() => ";".to_string(),
            _ => format!("{}", self.condition.as_ref().to_string()),
        };
        let step = match &self.step {
            None => String::new(),
            Some(step) => format!("{}", step.to_string()),
        };
        format!(
            "for ({}{}{}) {}",
            init,
            condition,
            step,
            self.body.to_string()
        )
    }
}
impl BreakStatement {
    fn to_string(&self) -> String {
        format!("break;")
    }
}
impl ContinueStatement {
    fn to_string(&self) -> String {
        format!("continue;")
    }
}
impl ClassStatement {
    fn to_string(&self) -> String {
        let parent = match &self.parent {
            Some(parent) => format!(" extends {}", parent.token.value),
            None => String::new(),
        };
        let methods = self
            .methods
            .iter()
            .map(|(name, func)| format!("{} = {}", name, func.to_string()))
            .collect::<Vec<String>>()
            .join("\n");
        format!(
            "class {}{} {{\n{}\n}}",
            self.name.token.value, parent, methods
        )
    }
}
impl TryCatchStatement {
    fn to_string(&self) -> String {
        let identifier = self.identifier.token.value.to_string();
        let catch_body = self.catch_body.to_string();
        format!(
            "try {{\n{}\n}} catch ({}) {{\n{}\n}}",
            self.body.to_string(),
            identifier,
            catch_body
        )
    }
}
impl EmptyStatement {
    fn to_string(&self) -> String {
        ";".to_string()
    }
}

#[derive(Clone, Debug)]
pub struct VarStatement {
    pub token: Token,
    pub name: IdentifierAstNode,
    pub value: AstNode,
}

#[derive(Clone, Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub value: AstNode,
}
#[derive(Clone, Debug)]
pub struct ThrowStatement {
    pub token: Token,
    pub value: AstNode,
}
#[derive(Clone, Debug)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: AstNode,
}
#[derive(Clone, Debug)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}
#[derive(Clone, Debug)]
pub struct IfStatement {
    pub token: Token,
    pub condition: AstNode,
    pub if_body: BlockStatement,
    pub else_body: Option<BlockStatement>,
}
#[derive(Clone, Debug)]
pub struct ForStatement {
    pub token: Token,
    pub init: Box<Statement>,
    pub condition: Box<Statement>,
    pub step: Option<AstNode>,
    pub body: BlockStatement,
}
#[derive(Clone, Debug)]
pub struct BreakStatement {
    pub token: Token,
}
#[derive(Clone, Debug)]
pub struct ContinueStatement {
    pub token: Token,
}
#[derive(Clone, Debug)]
pub struct ClassStatement {
    pub token: Token,
    pub name: IdentifierAstNode,
    pub parent: Option<IdentifierAstNode>,
    pub methods: Vec<(String, FunctionDeclarationAstNode)>,
}
#[derive(Clone, Debug)]
pub struct TryCatchStatement {
    pub token: Token,
    pub body: BlockStatement,
    pub identifier: IdentifierAstNode,
    pub catch_body: BlockStatement,
}
#[derive(Clone, Debug)]
pub struct EmptyStatement {
    pub token: Token,
}

#[derive(Clone, Debug)]
pub enum AstNode {
    Ident(IdentifierAstNode),
    Number(NumberAstNode),
    Boolean(BooleanAstNode),
    Null(NullAstNode),
    Str(StringAstNode),
    MapObjectDeclaration(MapObjectDeclarationAstNode),
    Operator(OperatorAstNode),
    FunctionDeclaration(FunctionDeclarationAstNode),
    FunctionCall(FunctionCallAstNode),
    ArrayDeclaration(ArrayDeclarationAstNode),
    Index(IndexAstNode),
    Group(GroupAstNode),
    New(NewAstNode),
    FunctionArgs(FunctionArgsAstNode),
    None(),
}

#[derive(Clone, Debug)]
pub enum OperatorAstNode {
    PrefixOperatorAstNode(PrefixOperatorAstNode),
    InfixOperatorAstNode(InfixOperatorAstNode),
    PostfixOperatorAstNode(PostfixOperatorAstNode),
}

impl AstNode {
    pub fn is_none(&self) -> bool {
        match self {
            AstNode::None() => true,
            _ => false,
        }
    }
    pub fn get_token(&self) -> Token {
        match self {
            AstNode::Ident(s) => s.get_token(),
            AstNode::Number(s) => s.get_token(),
            AstNode::Boolean(s) => s.get_token(),
            AstNode::Null(s) => s.get_token(),
            AstNode::Str(s) => s.get_token(),
            AstNode::MapObjectDeclaration(s) => s.get_token(),
            AstNode::Operator(s) => s.get_token(),
            AstNode::FunctionDeclaration(s) => s.get_token(),
            AstNode::FunctionCall(s) => s.get_token(),
            AstNode::ArrayDeclaration(s) => s.get_token(),
            AstNode::Index(s) => s.get_token(),
            AstNode::Group(s) => s.get_token(),
            AstNode::New(s) => s.get_token(),
            AstNode::FunctionArgs(s) => s.get_token(),
            AstNode::None() => Token {
                kind: TokenKind::EOF,
                value: "EOF".to_string(),
                line: 0,
                column: 0,
            },
        }
    }
    pub fn to_string(&self) -> String {
        match self {
            AstNode::Ident(s) => s.to_string(),
            AstNode::Number(s) => s.to_string(),
            AstNode::Boolean(s) => s.to_string(),
            AstNode::Null(s) => s.to_string(),
            AstNode::Str(s) => s.to_string(),
            AstNode::MapObjectDeclaration(s) => s.to_string(),
            AstNode::Operator(s) => s.to_string(),
            AstNode::FunctionDeclaration(s) => s.to_string(),
            AstNode::FunctionCall(s) => s.to_string(),
            AstNode::ArrayDeclaration(s) => s.to_string(),
            AstNode::Index(s) => s.to_string(),
            AstNode::Group(s) => s.to_string(),
            AstNode::New(s) => s.to_string(),
            AstNode::FunctionArgs(s) => s.to_string(),
            AstNode::None() => "".to_string(),
        }
    }
}
impl IdentifierAstNode {
    pub fn get_token(&self) -> Token {
        self.token.clone()
    }
    pub fn to_string(&self) -> String {
        self.token.value.to_string()
    }
}
impl NumberAstNode {
    pub fn get_token(&self) -> Token {
        self.token.clone()
    }
    pub fn to_string(&self) -> String {
        self.token.value.to_string()
    }
}
impl StringAstNode {
    pub fn get_token(&self) -> Token {
        self.token.clone()
    }
    pub fn to_string(&self) -> String {
        format!("{}", self.token.value)
    }
}
impl BooleanAstNode {
    pub fn get_token(&self) -> Token {
        self.token.clone()
    }
    pub fn to_string(&self) -> String {
        self.token.value.to_string()
    }
}

impl NullAstNode {
    pub fn get_token(&self) -> Token {
        self.token.clone()
    }
    pub fn to_string(&self) -> String {
        self.token.value.to_string()
    }
}
impl FunctionDeclarationAstNode {
    pub fn to_string(&self) -> String {
        let params = self
            .params
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        let body = self.body.to_string();
        format!("function({}) {}", params, body)
    }
    pub fn get_token(&self) -> Token {
        self.token.clone()
    }
}
impl FunctionCallAstNode {
    pub fn to_string(&self) -> String {
        let args = self
            .args
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        format!("{}({})", self.function.to_string(), args)
    }
    pub fn get_token(&self) -> Token {
        self.token.clone()
    }
}

impl MapObjectDeclarationAstNode {
    pub fn to_string(&self) -> String {
        let pairs = self
            .pairs
            .iter()
            .map(|(k, v)| format!("{}: {}", k.to_string(), v.to_string()))
            .collect::<Vec<String>>()
            .join(", ");
        format!("{{{}}}", pairs)
    }
    pub fn get_token(&self) -> Token {
        self.token.clone()
    }
}
impl ArrayDeclarationAstNode {
    pub fn to_string(&self) -> String {
        let items = self
            .items
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        format!("[{}]", items)
    }
    pub fn get_token(&self) -> Token {
        self.token.clone()
    }
}
impl IndexAstNode {
    pub fn to_string(&self) -> String {
        format!("[{}]", self.index.to_string())
    }
    pub fn get_token(&self) -> Token {
        self.token.clone()
    }
}
impl GroupAstNode {
    pub fn to_string(&self) -> String {
        format!("{}", self.expression.to_string())
    }
    pub fn get_token(&self) -> Token {
        self.token.clone()
    }
}
impl NewAstNode {
    pub fn to_string(&self) -> String {
        format!(
            "new {}({})",
            self.class.to_string(),
            self.args
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
    pub fn get_token(&self) -> Token {
        self.token.clone()
    }
}
impl FunctionArgsAstNode {
    pub fn to_string(&self) -> String {
        format!(
            "({})",
            self.args
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
    pub fn get_token(&self) -> Token {
        self.token.clone()
    }
}

impl OperatorAstNode {
    pub fn get_token(&self) -> Token {
        match self {
            OperatorAstNode::PrefixOperatorAstNode(s) => s.token.clone(),
            OperatorAstNode::InfixOperatorAstNode(s) => s.token.clone(),
            OperatorAstNode::PostfixOperatorAstNode(s) => s.token.clone(),
        }
    }
    pub fn to_string(&self) -> String {
        match self {
            OperatorAstNode::PrefixOperatorAstNode(s) => format!(
                "({}{})",
                s.token.value,
                s.right.as_ref().unwrap().to_string()
            ),
            OperatorAstNode::InfixOperatorAstNode(s) => {
                if s.token.kind == LPAREN || s.token.kind == LBRACKET {
                    format!(
                        "({} {})",
                        s.left.as_ref().unwrap().to_string(),
                        s.right.as_ref().unwrap().to_string()
                    )
                } else {
                    format!(
                        "({} {} {})",
                        s.left.as_ref().unwrap().to_string(),
                        s.token.value,
                        s.right.as_ref().unwrap().to_string()
                    )
                }
            }
            OperatorAstNode::PostfixOperatorAstNode(s) => format!(
                "({} {})",
                s.token.value,
                s.left.as_ref().unwrap().to_string()
            ),
        }
    }
    pub fn get_precedence(&self) -> u16 {
        match self {
            OperatorAstNode::PrefixOperatorAstNode(s) => s.precedence,
            OperatorAstNode::InfixOperatorAstNode(s) => s.precedence,
            OperatorAstNode::PostfixOperatorAstNode(s) => s.precedence,
        }
    }
}
/// 标识符表达式
#[derive(Clone, Debug)]
pub struct IdentifierAstNode {
    pub token: Token,
}
/// 数字表达式
#[derive(Clone, Debug)]
pub struct NumberAstNode {
    pub token: Token,
}
/// 布尔表达式
#[derive(Clone, Debug)]
pub struct BooleanAstNode {
    pub token: Token,
}
/// null表达式
#[derive(Clone, Debug)]
pub struct NullAstNode {
    pub token: Token,
}
/// 字符串表达式
#[derive(Clone, Debug)]
pub struct StringAstNode {
    pub token: Token,
}
/// Map对象声明节点例如 {a: 1, b: 2}
#[derive(Clone, Debug)]
pub struct MapObjectDeclarationAstNode {
    // pairs: [{key: ident/string, value: astNode}, {key: ident/string, value: astNode}, ....]
    pub token: Token,
    pub pairs: Vec<(AstNode, AstNode)>,
}

/// 前缀表达式
#[derive(Clone, Debug)]
pub struct PrefixOperatorAstNode {
    pub token: Token,
    pub right: Option<Box<AstNode>>,
    pub precedence: u16,
}
/// 中缀表达式
#[derive(Clone, Debug)]
pub struct InfixOperatorAstNode {
    pub token: Token,
    pub left: Option<Box<AstNode>>,
    pub right: Option<Box<AstNode>>,
    pub precedence: u16,
}
/// 后缀表达式
#[derive(Clone, Debug)]
pub struct PostfixOperatorAstNode {
    pub token: Token,
    pub left: Option<Box<AstNode>>,
    pub precedence: u16,
}
/// 函数声明
#[derive(Clone, Debug)]
pub struct FunctionDeclarationAstNode {
    pub token: Token,
    pub params: Vec<IdentifierAstNode>,
    pub body: BlockStatement,
}
/// 函数调用
#[derive(Clone, Debug)]
pub struct FunctionCallAstNode {
    pub token: Token,
    pub function: Box<AstNode>,
    pub args: Vec<AstNode>,
}
/// 数组声明
#[derive(Clone, Debug)]
pub struct ArrayDeclarationAstNode {
    pub token: Token,
    pub items: Vec<AstNode>,
}
/// 索引表达式
/// 例如 a[1]
#[derive(Clone, Debug)]
pub struct IndexAstNode {
    pub token: Token,
    pub index: Box<AstNode>,
}
/// 分组
#[derive(Clone, Debug)]
pub struct GroupAstNode {
    pub token: Token,
    pub expression: Box<AstNode>,
}

/// New
#[derive(Clone, Debug)]
pub struct NewAstNode {
    pub token: Token,
    pub class: IdentifierAstNode,
    pub args: Vec<AstNode>,
}
/// 函数参数
#[derive(Clone, Debug)]
pub struct FunctionArgsAstNode {
    pub token: Token,
    pub args: Vec<AstNode>,
}

pub static PRECENDENCE_MAP: Map<&'static str, u16> = phf_map! {
    "EOF" =>  0,
    "=" => 10,
    "||" => 11, "&&" => 12,
    "==" => 14, "!=" => 14,
    "<" => 15, "<="=> 15, ">"=> 15, ">="=> 15,
    "<<" => 16, ">>"=> 16, ">>>"=> 16,
    "+" => 17, "-"=> 17,
    "*" => 18, "/"=> 18, "%"=> 18,
    "(" => 19, // 函数调用
    "[" => 300, // 数组索引
    "." => 300,
};

pub static POSTFIX_PRECEDENCE_MAP: Map<&'static str, u16> = phf_map! {
    "++"=> 200, "--"=> 200,
};

pub static PREFIX_PRECEDENCE_MAP: Map<&'static str, u16> = phf_map! {
    "-"=> 100, "!"=> 100, "~"=> 100, "+"=> 100, "++"=> 100, "--"=> 100,
};

impl PrefixOperatorAstNode {
    pub fn new(token: Token) -> PrefixOperatorAstNode {
        PrefixOperatorAstNode {
            right: None,
            precedence: PREFIX_PRECEDENCE_MAP.get(&token.value).unwrap().clone(),
            token,
        }
    }
}

impl PostfixOperatorAstNode {
    pub fn new(token: Token) -> PostfixOperatorAstNode {
        PostfixOperatorAstNode {
            left: None,
            precedence: POSTFIX_PRECEDENCE_MAP.get(&token.value).unwrap().clone(),
            token,
        }
    }
}

impl InfixOperatorAstNode {
    pub fn new(token: Token) -> InfixOperatorAstNode {
        InfixOperatorAstNode {
            left: None,
            precedence: PRECENDENCE_MAP.get(&token.value).unwrap().clone(),
            right: None,
            token,
        }
    }
}
