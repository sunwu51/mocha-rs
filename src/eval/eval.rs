use crate::eval::{
    get_false, get_null, get_true, Context, Element, ErrorElement, ErrorInfo, SimpleError,
    PROTO_TYPE,
};
use crate::lexer::TokenKind;
use crate::lexer::TokenKind::{DECREMENT, INCREMENT, LBRACKET, MINUS, NOT, PLUS, POINT};
use crate::parser::{
    AstNode, BlockStatement, FunctionCallAstNode, IdentifierAstNode, IndexAstNode,
    InfixOperatorAstNode, OperatorAstNode, PostfixOperatorAstNode, PrefixOperatorAstNode,
    Statement,
};
use log::debug;
use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::format;
use std::panic;
use std::panic::{panic_any, AssertUnwindSafe};
use std::rc::Rc;

pub(crate) fn eval_expression(
    ast_node: &AstNode,
    mut ctx: Rc<RefCell<Context>>,
) -> Rc<RefCell<Element>> {
    let null = get_null();
    match ast_node {
        AstNode::Ident(ident_ast_node) => ctx.borrow_mut().get(&ident_ast_node.to_string()),
        AstNode::Number(number_ast_node) => {
            let str = number_ast_node.to_string();
            match str.parse::<f64>() {
                Ok(num) => Rc::new(RefCell::new(Element::new_number(num))),
                Err(e) => panic_any(SimpleError::new(&format!("{}", format!("Failed to parse number: {}", e)))),
            }
        }
        AstNode::Boolean(bool_ast_node) => {
            let str = bool_ast_node.to_string();
            match str.parse::<bool>() {
                Ok(b) => Rc::new(RefCell::new(Element::new_boolean(b))),
                Err(e) => panic_any(SimpleError::new(&format!("{}", format!("Failed to parse bool: {}", e)))),
            }
        }
        AstNode::Null(null_ast_node) => null,
        AstNode::Str(str_ast_node) => {
            let str = str_ast_node.to_string();
            Rc::new(RefCell::new(Element::new_string(str)))
        }
        AstNode::MapObjectDeclaration(obj) => {
            let mut res = Element::new();
            for (k, v) in &obj.pairs {
                let k = k.to_string();
                let v = eval_expression(&v, ctx.clone());
                res.set(&k, v);
            }
            Rc::new(RefCell::new(res))
        }
        AstNode::Operator(opt) => match opt {
            OperatorAstNode::PrefixOperatorAstNode(pre) => eval_prefix_operator(pre, ctx),
            OperatorAstNode::InfixOperatorAstNode(inf) => eval_infix_operator(inf, ctx),
            OperatorAstNode::PostfixOperatorAstNode(post) => eval_postfix_operator(post, ctx),
        },
        AstNode::FunctionDeclaration(function_declaration) => {
            let params = function_declaration
                .params
                .iter()
                .map(|it| it.to_string())
                .collect::<Vec<String>>();
            let element =
                Element::new_function(params, function_declaration.body.clone(), ctx.clone());
            Rc::new(RefCell::new(element))
        }
        AstNode::FunctionCall(function_call_ast_node) => {
            let mut function_expression = function_call_ast_node.function.as_ref();
            // 去掉冗余的组
            match function_expression {
                AstNode::Group(group) => {
                    function_expression = group.expression.as_ref();
                }
                _ => { /* do nothing */ }
            }
            let mut function_name: Option<String> = None;
            let mut this = None;
            let mut su = None;
            let mut function_element = get_null();
            match function_expression {
                // 全局方法
                AstNode::Ident(ident) => {
                    function_name = Some(ident.to_string());
                    if ident.to_string() == "print" {
                        let str = function_call_ast_node
                            .args
                            .iter()
                            .map(|e| {
                                let mut arg_res = eval_expression(e, ctx.clone());
                                format!("{}", arg_res.borrow().to_string())
                            })
                            .collect::<Vec<String>>()
                            .join(",");
                        println!("{}", str);
                        return get_null();
                    }
                    if ident.to_string() == "error" {
                        if function_call_ast_node.args.len() == 0 {
                            panic_any(SimpleError::new("error() takes at least 1 argument"))
                        }
                        let msg = eval_expression(&function_call_ast_node.args[0], ctx.clone());
                        let msg = msg.borrow().to_string();
                        return Rc::new(RefCell::new(Element::Error(ErrorElement::new(
                            &msg,
                            vec![],
                        ))));
                    }
                    function_element = eval_expression(function_expression, ctx.clone());
                }
                // 对象方法
                AstNode::Operator(OperatorAstNode::InfixOperatorAstNode(
                    InfixOperatorAstNode {
                        token, left, right, ..
                    },
                )) => {
                    // xx.method() => 先对xx求值，结果赋值给_this；然后找到method这个functionElement
                    let kind = function_expression.get_token().kind;
                    let right = right.as_ref().unwrap().as_ref();
                    let left = left.as_ref().unwrap().as_ref();
                    let mut obj_method = false;
                    if let AstNode::Ident(_) = right {
                        if kind == POINT {
                            obj_method = true;
                        }
                    }
                    if let AstNode::Str(_) = right {
                        if kind == LBRACKET {
                            obj_method = true;
                        }
                    }
                    if obj_method {
                        let this_ele = eval_expression(left, ctx.clone());
                        function_name = Some(right.to_string());
                        function_element = this_ele
                            .borrow()
                            .get(&right.to_string())
                            .unwrap_or(get_null());
                        let cur_cls_pro = this_ele.borrow().get(PROTO_TYPE);
                        this = Some(this_ele);

                        su = if cur_cls_pro.is_some() {
                            cur_cls_pro.unwrap().clone().borrow().get(PROTO_TYPE)
                        } else {
                            None
                        };
                        // super比较特殊，调用super.xx的时候，父类方法中的this指向自身，而是指向当前的对象
                        if left.to_string() == "super" {
                            let this_ele = ctx.borrow().get("this");
                            this = Some(this_ele.clone());
                        }
                    }
                }
                _ => ()
            }

            // 其他形式，例如 "b()()",函数的返回值也是个函数，直接去调用
            if function_element.borrow().is_none() {
                function_element = eval_expression(function_expression, ctx.clone());
            }
            if function_name.is_none() {
                function_name = Some("<anonymous>".to_string());
            }

            let function_element_clone = function_element.clone();

            let mut is_func = false;
            let mut is_native_func = false;
            if let Element::Function { .. } = &*function_element.borrow() {
                is_func = true;
            } else if let Element::NativeFunction { .. } = &*function_element.borrow() {
                is_func = true;
                is_native_func = true;
            }

            if is_func {
                let args = function_call_ast_node
                    .args
                    .iter()
                    .map(|it| eval_expression(it, ctx.clone()))
                    .collect::<Vec<Rc<RefCell<Element>>>>();
                if is_native_func {
                    return function_element_clone.borrow_mut().native_function_call(
                        &function_name.unwrap(),
                        args,
                        this,
                        su,
                        &format!(
                            "{}:{}",
                            function_call_ast_node.token.line, function_call_ast_node.token.column
                        ),
                    );
                } else {
                    return function_element_clone.borrow().function_call(
                        &function_name.unwrap(),
                        args,
                        this,
                        su,
                        &format!(
                            "{}:{}",
                            function_call_ast_node.token.line, function_call_ast_node.token.column
                        ),
                    );
                }
            }

            if let AstNode::Operator(OperatorAstNode::InfixOperatorAstNode(
                InfixOperatorAstNode {
                    token, left, right, ..
                },
            )) = function_expression
            {
                if right.is_some() && right.as_ref().unwrap().to_string() == "constructor" {
                    return get_null();
                }
            }
            panic_any(SimpleError::new(
                &format!("{} is not a function", function_expression.to_string())
            ));
        }
        AstNode::ArrayDeclaration(array_declaration) => {
            let vec = array_declaration
                .items
                .iter()
                .map(|item| eval_expression(item, ctx.clone()))
                .collect::<Vec<Rc<RefCell<Element>>>>();
            Rc::new(RefCell::new(Element::new_array(vec)))
        }
        AstNode::Group(group) => eval_expression(group.expression.as_ref(), ctx),
        AstNode::New(new_ast_node) => {
            let class_name = new_ast_node.class.to_string();
            let args = new_ast_node
                .args
                .iter()
                .map(|it| eval_expression(it, ctx.clone()))
                .collect::<Vec<Rc<RefCell<Element>>>>();
            let cls_element = ctx.borrow().get(class_name.as_str());

            let borrow_ref = &*cls_element.borrow();

            if let Element::ProtoType { common, .. } = borrow_ref {
                // 1 创建空对象
                let mut this_element = Element::new();
                // 2 当前对象原型 指向 类的原型
                this_element.set_proto_type(cls_element.borrow().get_proto_type());
                // 3 this指向空对象，super指向一个只有父类方法（原型）的对象，这样super.只能调用父类方法
                let mut su: Option<Rc<RefCell<Element>>> = cls_element.borrow().get(PROTO_TYPE);

                if su.is_some() {
                    su = su.unwrap().borrow().get(PROTO_TYPE);
                }

                // 4 运行构造方法，原型链一直往上找constructor构造方法，如果全都没有的话，就不执行任何操作
                let proto = cls_element.borrow().get_proto_type();

                let constructor = proto.borrow().get("constructor");

                let this = Rc::new(RefCell::new(this_element));

                if constructor.is_some() {
                    let constructor = constructor.unwrap().clone();
                    match &*constructor.borrow() {
                        Element::Function { .. } => {}
                        _ => panic_any(SimpleError::new("constructor is not a function")),
                    };
                    constructor.borrow().function_call(
                        "constructor",
                        args,
                        Some(this.clone()),
                        su,
                        &format!(
                            "{}:{}",
                            new_ast_node.token.line, new_ast_node.token.column
                        ),
                    );
                }
                debug!("new对象，{} {}", class_name, this.borrow().to_string());
                this
            } else {
                panic_any(SimpleError::new(&format!("{} is not a class", new_ast_node.class.to_string())));
            }
        }
        // AstNode::None() => {},
        _ => null,
    }
}

fn eval_prefix_operator(
    pre: &PrefixOperatorAstNode,
    mut ctx: Rc<RefCell<Context>>,
) -> Rc<RefCell<Element>> {
    let right = eval_expression(pre.right.as_ref().unwrap(), ctx);

    if pre.token.kind == PLUS {
        match &*right.borrow() {
            Element::Number { value, .. } => {
                return Rc::new(RefCell::new(Element::new_number(*value)));
            }
            _ => panic_any(SimpleError::new("prefix operator + should before number")),
        };
    } else if pre.token.kind == MINUS {
        match &*right.borrow() {
            Element::Number { value, .. } => {
                return Rc::new(RefCell::new(Element::new_number(-*value)));
            }
            _ => panic_any(SimpleError::new("prefix operator + should before number")),
        };
    } else if pre.token.kind == NOT {
        return Rc::new(RefCell::new(Element::new_boolean(right.borrow().to_bool())));
    } else if pre.token.kind == INCREMENT {
        match &mut *right.borrow_mut() {
            Element::Number { value, .. } => {
                *value += 1 as f64;
                return Rc::new(RefCell::new(Element::new_number(*value)));
            }
            _ => panic_any(SimpleError::new("prefix operator + should before number")),
        };
    } else if pre.token.kind == DECREMENT {
        match &mut *right.borrow_mut() {
            Element::Number { value, .. } => {
                *value -= 1 as f64;
                return Rc::new(RefCell::new(Element::new_number(*value)));
            }
            _ => panic_any(SimpleError::new("prefix operator + should before number")),
        };
    }
    panic_any(SimpleError::new(&format!("operator {} not supported now", pre.token.value)))
}
fn eval_infix_operator(
    infix: &InfixOperatorAstNode,
    mut ctx: Rc<RefCell<Context>>,
) -> Rc<RefCell<Element>> {
    let mut res = get_null();
    match infix.token.kind {
        TokenKind::ASSIGN => {
            let left = infix.left.as_ref().unwrap().as_ref();
            match left {
                AstNode::Ident(name) => {
                    let value =
                        eval_expression(infix.right.as_ref().unwrap().as_ref(), ctx.clone());
                    ctx.borrow_mut().update(&left.to_string(), value);
                }
                AstNode::Operator(op) => match op {
                    OperatorAstNode::InfixOperatorAstNode(InfixOperatorAstNode {
                        token,
                        left: this,
                        right: property,
                        ..
                    }) => {
                        if token.kind == POINT {
                            let property = property.clone().unwrap();
                            match property.as_ref() {
                                AstNode::Ident(ident) => {
                                    let this = eval_expression(&this.clone().unwrap(), ctx.clone());
                                    let property = property.to_string();
                                    let value = eval_expression(
                                        infix.right.as_ref().unwrap().as_ref(),
                                        ctx.clone(),
                                    );
                                    this.borrow_mut().set(&property, value);
                                }
                                _ => panic_any(SimpleError::new("Not supported")),
                            }
                        } else if token.kind == LBRACKET {
                            let property = property.clone().unwrap();
                            match property.as_ref() {
                                AstNode::Index(index) => {
                                    let property = index.clone().index;
                                    let property: Rc<RefCell<Element>> =
                                        eval_expression(&property, ctx.clone());
                                    match &*property.borrow_mut() {
                                        Element::Number { value: index, .. } => {
                                            let this: Rc<RefCell<Element>> = eval_expression(
                                                &this.clone().unwrap(),
                                                ctx.clone(),
                                            );
                                            let index = index.clone() as usize;
                                            match &mut *this.borrow_mut() {
                                                Element::Array { ref mut array, .. } => {
                                                    if index >= array.len() {
                                                        panic_any(SimpleError::new("Index out of bounds"));
                                                    }
                                                    let value = eval_expression(
                                                        infix.right.as_ref().unwrap().as_ref(),
                                                        ctx.clone(),
                                                    );
                                                    array[index] = value;
                                                }
                                                _ => panic_any(SimpleError::new("Not supported")),
                                            };
                                        }
                                        Element::String {
                                            value: property, ..
                                        } => {
                                            let this = eval_expression(
                                                &this.clone().unwrap(),
                                                ctx.clone(),
                                            );
                                            let value = eval_expression(
                                                infix.right.as_ref().unwrap().as_ref(),
                                                ctx.clone(),
                                            );
                                            this.borrow_mut().set(property, value);
                                        }
                                        _ => panic_any(SimpleError::new("Not supported")),
                                    };
                                }
                                _ => panic_any(SimpleError::new("Not supported")),
                            }
                        } else {
                            panic_any(SimpleError::new("Not supported"));
                        }
                    }
                    _ => panic_any(SimpleError::new("Not supported")),
                },
                _ => panic_any(SimpleError::new("Not supported")),
            };
        }
        TokenKind::LPAREN => {
            let right = infix.right.as_ref().unwrap();
            if let AstNode::FunctionArgs(args) = right.as_ref() {
                let args = args.args.clone();
                res = eval_expression(
                    &AstNode::FunctionCall(FunctionCallAstNode {
                        token: infix.token.clone(),
                        function: infix.left.as_ref().unwrap().clone(),
                        args,
                    }),
                    ctx.clone(),
                );
            } else {
                panic_any(SimpleError::new("Infix operator ( doesn't have arguments"));
            }
        }
        TokenKind::LBRACKET => {
            let right = infix.right.clone().unwrap();
            match right.as_ref() {
                AstNode::Index(IndexAstNode { index, .. }) => {
                    let index = eval_expression(index, ctx.clone());
                    let this = eval_expression(&infix.left.clone().unwrap(), ctx.clone());
                    match &*index.borrow() {
                        Element::Number { value: num, .. } => {
                            let num = num.clone() as usize;
                            match &*this.borrow() {
                                Element::String { value: str, .. } => match str.chars().nth(num) {
                                    None => panic_any(SimpleError::new("Index out of bounds")),
                                    Some(c) => {
                                        res = Rc::new(RefCell::new(Element::new_string(
                                            c.to_string(),
                                        )))
                                    }
                                },
                                Element::Array { array, .. } => match array.iter().nth(num) {
                                    None => panic_any(SimpleError::new("Index out of bounds")),
                                    Some(item) => res = item.clone(),
                                },
                                _ => panic_any(SimpleError::new("Not supported")),
                            }
                        }
                        Element::String { value: str, .. } => {
                            res = this.borrow().get(str).unwrap_or(get_null());
                        }
                        _ => panic_any(SimpleError::new("Not supported")),
                    };
                }
                _ => panic_any(SimpleError::new("Not supported")),
            }
        }
        TokenKind::PLUS => {
            let l: Rc<RefCell<Element>> =
                eval_expression(infix.left.as_ref().unwrap(), ctx.clone());
            let r: Rc<RefCell<Element>> =
                eval_expression(infix.right.as_ref().unwrap(), ctx.clone());
            match (l.borrow().clone(), r.borrow().clone()) {
                (Element::Number { value: l, .. }, Element::Number { value: r, .. }) => {
                    res = Rc::new(RefCell::new(Element::new_number(l + r)))
                }
                (Element::String { value: l, .. }, _) => {
                    res = Rc::new(RefCell::new(Element::new_string(format!(
                        "{}{}",
                        l,
                        r.borrow().to_string()
                    ))))
                }
                (_, Element::String { value: r, .. }) => {
                    res = Rc::new(RefCell::new(Element::new_string(format!(
                        "{}{}",
                        l.borrow().to_string(),
                        r
                    ))))
                }
                (_, _) => panic_any(SimpleError::new("+ should not be here")),
            };
        }
        TokenKind::MINUS => {
            let l: Rc<RefCell<Element>> =
                eval_expression(infix.left.as_ref().unwrap(), ctx.clone());
            let r: Rc<RefCell<Element>> =
                eval_expression(infix.right.as_ref().unwrap(), ctx.clone());
            match (l.borrow().clone(), r.borrow().clone()) {
                (Element::Number { value: l, .. }, Element::Number { value: r, .. }) => {
                    res = Rc::new(RefCell::new(Element::new_number(l - r)))
                }
                (_, _) => panic_any(SimpleError::new("- should not be here")),
            };
        }
        TokenKind::MULTIPLY => {
            let l: Rc<RefCell<Element>> =
                eval_expression(infix.left.as_ref().unwrap(), ctx.clone());
            let r: Rc<RefCell<Element>> =
                eval_expression(infix.right.as_ref().unwrap(), ctx.clone());
            match (l.borrow().clone(), r.borrow().clone()) {
                (Element::Number { value: l, .. }, Element::Number { value: r, .. }) => {
                    res = Rc::new(RefCell::new(Element::new_number(l * r)))
                }
                (_, _) => panic_any(SimpleError::new("* should not be here")),
            };
        }
        TokenKind::DIVIDE => {
            let l: Rc<RefCell<Element>> =
                eval_expression(infix.left.as_ref().unwrap(), ctx.clone());
            let r: Rc<RefCell<Element>> =
                eval_expression(infix.right.as_ref().unwrap(), ctx.clone());
            match (l.borrow().clone(), r.borrow().clone()) {
                (Element::Number { value: l, .. }, Element::Number { value: r, .. }) => {
                    res = Rc::new(RefCell::new(Element::new_number(l / r)))
                }
                (_, _) => panic_any(SimpleError::new("/ should not be here")),
            };
        }
        TokenKind::MODULUS => {
            let l: Rc<RefCell<Element>> =
                eval_expression(infix.left.as_ref().unwrap(), ctx.clone());
            let r: Rc<RefCell<Element>> =
                eval_expression(infix.right.as_ref().unwrap(), ctx.clone());
            match (l.borrow().clone(), r.borrow().clone()) {
                (Element::Number { value: l, .. }, Element::Number { value: r, .. }) => {
                    res = Rc::new(RefCell::new(Element::new_number(l % r)))
                }
                (_, _) => panic_any(SimpleError::new("% should not be here")),
            };
        }
        TokenKind::POINT => {
            let l = eval_expression(infix.left.as_ref().unwrap(), ctx.clone());
            let ident = infix.right.as_ref().unwrap();
            match ident.as_ref() {
                AstNode::Ident(ident) => {
                    let property = ident.to_string();
                    return l.borrow().get(&property).unwrap_or(get_null());
                }
                _ => panic_any(SimpleError::new("Not supported")),
            }
        }
        TokenKind::AND => {
            let l = eval_expression(infix.left.as_ref().unwrap(), ctx.clone());
            let r = eval_expression(infix.right.as_ref().unwrap(), ctx.clone());
            res = if l.borrow().to_bool() && r.borrow().to_bool() {
                get_true()
            } else {
                get_false()
            }
        }
        TokenKind::OR => {
            let l = eval_expression(infix.left.as_ref().unwrap(), ctx.clone());
            let r = eval_expression(infix.right.as_ref().unwrap(), ctx.clone());
            res = if l.borrow().to_bool() || r.borrow().to_bool() {
                get_true()
            } else {
                get_false()
            }
        }
        TokenKind::GT => {
            let l = eval_expression(infix.left.as_ref().unwrap(), ctx.clone());
            let r = eval_expression(infix.right.as_ref().unwrap(), ctx.clone());
            match (l.borrow().clone(), r.borrow().clone()) {
                (Element::Number { value: l, .. }, Element::Number { value: r, .. }) => {
                    res = Rc::new(RefCell::new(Element::new_boolean(l > r)))
                }
                (_, _) => panic_any(SimpleError::new("> should not be here")),
            };
        }
        TokenKind::LT => {
            let l = eval_expression(infix.left.as_ref().unwrap(), ctx.clone());
            let r = eval_expression(infix.right.as_ref().unwrap(), ctx.clone());
            match (l.borrow().clone(), r.borrow().clone()) {
                (Element::Number { value: l, .. }, Element::Number { value: r, .. }) => {
                    res = if l < r { get_true() } else { get_false() }
                }
                (_, _) => panic_any(SimpleError::new("< should not be here")),
            };
        }
        TokenKind::GTE => {
            let l = eval_expression(infix.left.as_ref().unwrap(), ctx.clone());
            let r = eval_expression(infix.right.as_ref().unwrap(), ctx.clone());
            match (l.borrow().clone(), r.borrow().clone()) {
                (Element::Number { value: l, .. }, Element::Number { value: r, .. }) => {
                    res = if l >= r { get_true() } else { get_false() }
                }
                (_, _) => panic_any(SimpleError::new(">= should not be here")),
            };
        }
        TokenKind::LTE => {
            let l = eval_expression(infix.left.as_ref().unwrap(), ctx.clone());
            let r = eval_expression(infix.right.as_ref().unwrap(), ctx.clone());
            match (l.borrow().clone(), r.borrow().clone()) {
                (Element::Number { value: l, .. }, Element::Number { value: r, .. }) => {
                    res = if l <= r { get_true() } else { get_false() }
                }
                (_, _) => panic_any(SimpleError::new("<= should not be here")),
            };
        }
        TokenKind::NEQ => {
            let l = eval_expression(infix.left.as_ref().unwrap(), ctx.clone());
            let r = eval_expression(infix.right.as_ref().unwrap(), ctx.clone());
            match (l.borrow().clone(), r.borrow().clone()) {
                (Element::Number { value: l, .. }, Element::Number { value: r, .. }) => {
                    res = if l != r { get_true() } else { get_false() }
                }
                (Element::String { value: l, .. }, Element::String { value: r, .. }) => {
                    res = if l != r { get_true() } else { get_false() }
                }
                (Element::Boolean { value: l, .. }, Element::Boolean { value: r, .. }) => {
                    res = if l != r { get_true() } else { get_false() }
                }
                (Element::Null { .. }, Element::Null { .. }) => res = get_false(),
                (_, _) => {
                    res = if l.borrow().get_id() != r.borrow().get_id() {
                        get_true()
                    } else {
                        get_false()
                    }
                }
            };
        }
        TokenKind::EQ => {
            let l = eval_expression(infix.left.as_ref().unwrap(), ctx.clone());
            let r = eval_expression(infix.right.as_ref().unwrap(), ctx.clone());
            match (l.borrow().clone(), r.borrow().clone()) {
                (Element::Number { value: l, .. }, Element::Number { value: r, .. }) => {
                    res = if l == r { get_true() } else { get_false() }
                }
                (Element::String { value: l, .. }, Element::String { value: r, .. }) => {
                    res = if l == r { get_true() } else { get_false() }
                }
                (Element::Boolean { value: l, .. }, Element::Boolean { value: r, .. }) => {
                    res = if l == r { get_true() } else { get_false() }
                }
                (Element::Null { .. }, Element::Null { .. }) => res = get_true(),
                (_, _) => {
                    res = if l.borrow().get_id() == r.borrow().get_id() {
                        get_true()
                    } else {
                        get_false()
                    }
                }
            };
        }
        _ => {
            panic_any(SimpleError::new(&format!("operator {} not supported now", infix.token.value)))
        }
    }

    res
}
fn eval_postfix_operator(
    post: &PostfixOperatorAstNode,
    mut ctx: Rc<RefCell<Context>>,
) -> Rc<RefCell<Element>> {
    let mut cur_value = 0.0;
    if post.token.kind == TokenKind::INCREMENT {
        let mut left = eval_expression(post.left.as_ref().unwrap(), ctx.clone());
        match &mut *left.borrow_mut() {
            Element::Number { value, .. } => {
                cur_value = *value;
                *value = *value + 1.0;
            }
            _ => panic_any(SimpleError::new("++ need a number")),
        };
        Rc::new(RefCell::new(Element::new_number(cur_value)))
    } else if post.token.kind == TokenKind::DECREMENT {
        let left = eval_expression(post.left.as_ref().unwrap(), ctx.clone());
        match &mut *left.borrow_mut() {
            Element::Number { value, .. } => {
                cur_value = *value;
                *value = *value - 1.0;
            }
            _ => panic_any(SimpleError::new("-- need a number")),
        }
        Rc::new(RefCell::new(Element::new_number(cur_value)))
    } else {
        panic_any(SimpleError::new(&format!("operator {} not supported now", post.token.value)))
    }
}

fn eval_statement(statement: &Statement, mut ctx: Rc<RefCell<Context>>) -> Rc<RefCell<Element>> {
    debug!("Now eval statement: {:?}", statement.to_string());
    match statement {
        Statement::None() => get_null(),
        Statement::Var(var_statement) => {
            let v = eval_expression(&var_statement.value, ctx.clone());
            ctx.borrow_mut().set(&var_statement.name.to_string(), v);
            get_null()
        }
        Statement::Exp(exp_statement) => eval_expression(&exp_statement.expression, ctx),
        Statement::Return(return_statement) => {
            let return_ele = eval_expression(&return_statement.value, ctx.clone());
            ctx.borrow_mut().set_return_element(return_ele);
            get_null()
        }
        Statement::Block(block_statement) => {
            let new_ctx = Context::new(Some(ctx.clone()));
            eval_block(block_statement, Rc::new(RefCell::new(new_ctx)))
        }
        Statement::If(if_statement) => {
            let cond = eval_expression(&if_statement.condition, ctx.clone());
            let cond = cond.borrow().to_bool();
            if cond {
                eval_block(&if_statement.if_body, ctx.clone());
            } else {
                if if_statement.else_body.is_some() {
                    eval_block(&if_statement.else_body.clone().unwrap(), ctx.clone());
                }
            }
            get_null()
        }
        Statement::For(for_statement) => {
            // init
            eval_statement(for_statement.init.as_ref(), ctx.clone());
            loop {
                // cond
                let cond = for_statement.condition.is_none()
                    || eval_statement(&for_statement.condition, ctx.clone())
                        .borrow()
                        .to_bool();
                if !cond {
                    return get_null();
                }
                let mut new_ctx = Context::new(Some(ctx.clone()));
                new_ctx.for_ctx.in_for = true;

                // body
                let new_ctx = Rc::new(RefCell::new(new_ctx));
                eval_block(&for_statement.body, new_ctx.clone());

                // break/return
                if new_ctx.borrow().for_ctx.is_break
                    || new_ctx.borrow().fun_ctx.return_element.is_some()
                {
                    break;
                }

                // step
                if for_statement.step.is_some() {
                    eval_expression(&for_statement.step.clone().unwrap(), ctx.clone());
                }
            }
            get_null()
        }
        Statement::Break(_) => {
            ctx.borrow_mut().set_break();
            get_null()
        }
        Statement::Continue(_) => {
            ctx.borrow_mut().set_continue();
            get_null()
        }
        Statement::Throw(throw_statement) => {
            let err = eval_expression(&throw_statement.value, ctx.clone());
            let x = match &mut *err.borrow_mut() {
                Element::Error(ref mut error_element) => {
                    error_element.push_stack(ErrorInfo {
                        function_name: ctx.borrow().get_function_name(),
                        position: format!(
                            "{}:{}",
                            statement.get_token().line,
                            statement.get_token().column
                        ),
                    });
                    error_element.to_thread_safe()
                }
                _ => panic_any(SimpleError::new("Throw error should be an error element")),
            };
            panic_any(x);
        }
        Statement::TryCatch(try_catch_statement) => {
            let try_res = panic::catch_unwind(AssertUnwindSafe(|| {
                eval_block(
                    &try_catch_statement.body,
                    Rc::new(RefCell::new(Context::new(Some(ctx.clone())))),
                );
            }));

            match try_res {
                Ok(_) => {}
                Err(e) => {
                    let res = e.downcast::<SimpleError>();
                    match res {
                        Ok(simple_err) => {
                            // 捕捉到异常，不再向上抛出
                            let err = *simple_err;
                            let err_element = Element::Error(ErrorElement::new(&err.msg, err.stack));
                            let mut catch_ctx = Context::new(Some(ctx.clone()));
                            catch_ctx.set(&try_catch_statement.identifier.to_string(), Rc::new(RefCell::new(err_element)));
                            eval_block(&try_catch_statement.catch_body, Rc::new(RefCell::new(catch_ctx)));
                        }
                        Err(e) => panic_any(e),
                    }
                }
            };

            get_null()
        }
        Statement::Class(class_statement) => {
            let parent = match &class_statement.parent {
                None => None,
                Some(parent) => {
                    let el = ctx.borrow().get(&parent.to_string());
                    let el_clone = el.clone();
                    let mut res = None;
                    match &*el.borrow() {
                        Element::ProtoType { .. } => {
                            res = Some(el_clone);
                        }
                        _ => panic_any(SimpleError::new(
                            &format!("parent class {} must be a class", parent.to_string()))
                        ),
                    };
                    res
                }
            };

            let class_name = class_statement.name.to_string();

            let mut methods = HashMap::new();

            for (name, declare_ast_node) in &class_statement.methods {
                let function_element = eval_expression(
                    &AstNode::FunctionDeclaration(declare_ast_node.clone()),
                    ctx.clone(),
                );
                let function_clone = function_element.clone();
                let borrow_ref = &*function_element.borrow();
                if let Element::Function { .. } = borrow_ref {
                    methods.insert(name.to_string(), function_clone);
                } else {
                    panic_any(SimpleError::new(&format!("{}#{} must be a function", class_name, name)));
                }
            }
            // 在语法分析中，我们已经把类中的字段赋值的语法糖写法，转为了在constructor中赋值，所以类中只有方法。
            let proto_type = Element::new_prototype(&class_name, parent, methods);
            debug!("声明类 {} : {}", class_name, proto_type.to_string());
            ctx.borrow_mut()
                .set(&class_name, Rc::new(RefCell::new(proto_type)));
            get_null()
        }
        _ => get_null(),
    }
}

pub fn eval_block(block: &BlockStatement, new_ctx: Rc<RefCell<Context>>) -> Rc<RefCell<Element>> {
    eval_statements(&block.statements, new_ctx, true)
}

pub fn eval_statements(
    statements: &Vec<Statement>,
    mut ctx: Rc<RefCell<Context>>,
    exit_when_panic: bool,
) -> Rc<RefCell<Element>> {
    let mut res = get_null();
    for x in statements.iter() {
        if ctx.borrow().fun_ctx.return_element.is_some()
            || ctx.borrow().for_ctx.is_break
            || ctx.borrow().for_ctx.is_continue
        {
            break;
        }
        let result = panic::catch_unwind(AssertUnwindSafe(|| eval_statement(x, ctx.clone())));

        match result {
            Ok(r) => res = r,
            Err(e) => {
                let res = e.downcast::<SimpleError>();
                match res {
                    Ok(simple_err) => {
                        let msg = simple_err.msg.to_string();
                        let mut stack = simple_err.stack.clone();
                        let length = stack.len();
                        if stack[length - 1].position == "" {
                            stack[length - 1].position =
                                format!("{}:{}", x.get_token().line, x.get_token().column);
                        }
                        if ctx.borrow().parent.is_none() {
                            println!("Uncaught error: {}", msg);
                            stack.iter().for_each(|it| {
                                let function_name = it.function_name.to_string();
                                let function_name = if function_name == "" {
                                    "__root__"
                                } else {
                                    function_name.as_ref()
                                };
                                println!("  at {} {}", function_name, it.position)
                            });
                            if exit_when_panic {
                                std::process::exit(1);
                            }
                        } else {
                            panic_any(SimpleError {
                                msg: msg.to_string(),
                                stack,
                            });
                        }
                    }
                    Err(e) => panic_any(e),
                }
            }
        }
    }
    res
}
