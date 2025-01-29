pub mod eval;

use crate::eval::eval::{eval_block, eval_expression};
use crate::parser::{BlockStatement, Statement};
use log::debug;
use std::cell::{Ref, RefCell};
use std::collections::{HashMap, VecDeque};
use std::f32::consts::E;
use std::ops::{DerefMut, Index};
use std::panic::{panic_any, AssertUnwindSafe};
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, LockResult, Mutex};
use std::{fmt, panic};
use std::num::ParseFloatError;

static EL_ID: AtomicU64 = AtomicU64::new(100);

pub static PROTO_TYPE: &str = "$$pro$$";

#[derive(Clone, Debug)]
pub struct ElementCommon {
    pub el_id: u64,
    // 特殊key $$pro$$ 为父类信息
    pub map: HashMap<String, Rc<RefCell<Element>>>,
}
#[derive(Clone, Debug)]
pub enum Element {
    Object(ElementCommon),
    Number {
        common: ElementCommon,
        value: f64,
    },
    String {
        common: ElementCommon,
        value: String,
    },
    Boolean {
        common: ElementCommon,
        value: bool,
    },
    Null {
        common: ElementCommon,
    },
    Array {
        common: ElementCommon,
        array: VecDeque<Rc<RefCell<Element>>>,
    },
    Error(ErrorElement),
    ProtoType {
        common: ElementCommon,
    },
    Function {
        common: ElementCommon,
        params: Vec<String>,
        body: BlockStatement,
        closure_ctx: Rc<RefCell<Context>>,
    },
    NativeFunction {
        common: ElementCommon,
        name: String,
        param_count: usize,
        native_func: fn(&mut Self, Vec<Rc<RefCell<Element>>>) -> Rc<RefCell<Element>>,
    },
}
impl ElementCommon {
    fn to_string(&self) -> String {
        format!(
            "{{ {} }}",
            self.map
                .iter()
                .map(|(k, v)| format!("{}:{}", k, v.borrow().to_string()))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
    fn new() -> ElementCommon {
        ElementCommon {
            el_id: EL_ID.fetch_add(1, Ordering::SeqCst),
            map: HashMap::new(),
        }
    }
    pub fn get(&self, key: &str) -> Option<Rc<RefCell<Element>>> {
        // println!("get key={}", key);
        let from_map = self.map.get(key).clone();
        if !from_map.is_none() {
            // 自身属性
            return Some(from_map.unwrap().clone());
        }
        let proto_type = self.map.get(PROTO_TYPE);
        if proto_type.is_some() {
            let proto_type = proto_type.unwrap().clone();
            let from_proto_type = proto_type.borrow().get(key);
            if from_proto_type.is_some() {
                // 原型链属性
                return proto_type.borrow().get(key);
            }
        }
        None
    }

    pub fn get_from_cur(&self, key: &str) -> Option<Rc<RefCell<Element>>> {
        // println!("get key={}", key);
        let from_map = self.map.get(key).clone();
        if !from_map.is_none() {
            // 自身属性
            return Some(from_map.unwrap().clone());
        }
        None
    }

    pub fn set(&mut self, key: &str, value: Rc<RefCell<Element>>) {
        self.map.insert(key.to_string(), value);
    }
}

impl Element {
    pub fn new() -> Element {
        Element::Object(ElementCommon {
            el_id: EL_ID.fetch_add(1, Ordering::SeqCst),
            map: Default::default(),
        })
    }
    pub fn new_obj(class_name: &str) -> Element {
        let mut common = ElementCommon::new();
        common.set(
            "type",
            Rc::new(RefCell::new(Element::new_string(class_name.to_string()))),
        );
        Element::Object(common)
    }
    pub fn new_number(value: f64) -> Element {
        Element::Number {
            common: ElementCommon::new(),
            value,
        }
    }
    pub fn new_string(value: String) -> Element {
        let mut common = ElementCommon::new();
        common.set(PROTO_TYPE, get_string_proto());
        Element::String {
            common,
            value,
        }
    }
    pub fn new_boolean(value: bool) -> Element {
        Element::Boolean {
            common: ElementCommon::new(),
            value,
        }
    }
    pub fn new_null() -> Element {
        Element::Null {
            common: ElementCommon::new(),
        }
    }
    pub fn new_array(array: Vec<Rc<RefCell<Element>>>) -> Element {
        let mut common = ElementCommon::new();
        common.set(PROTO_TYPE, get_array_proto());
        Element::Array {
            common,
            array: VecDeque::from(array),
        }
    }
    pub fn new_error(message: String) -> Element {
        let mut res = Element::Error(ErrorElement {
            common: ElementCommon::new(),
        });
        res.set("msg", Rc::new(RefCell::new(Element::new_string(message))));
        res.set("stack", Rc::new(RefCell::new(Element::new_array(vec![]))));
        res
    }
    pub fn new_prototype(
        class_name: &str,
        parent: Option<Rc<RefCell<Element>>>,
        methods: HashMap<String, Rc<RefCell<Element>>>,
    ) -> Element {
        let mut common = ElementCommon::new();
        let mut proto_type = Element::new();


        if class_name != "" {
            proto_type.set(
                "type",
                Rc::new(RefCell::new(Element::new_string(class_name.to_string()))),
            )
        }
        methods.iter().for_each(|(k, v)| {
            proto_type.set(k, v.clone());
        });
        match parent {
            None => {}
            Some(parent) => {
                proto_type.set(PROTO_TYPE, parent.clone().borrow().get_proto_type());
            }
        };
        common
            .map
            .insert(PROTO_TYPE.to_string(), Rc::new(RefCell::new(proto_type)));

        Element::ProtoType { common }
    }
    pub fn new_function(
        params: Vec<String>,
        body: BlockStatement,
        context: Rc<RefCell<Context>>,
    ) -> Element {
        Element::Function {
            common: ElementCommon::new(),
            params,
            body,
            closure_ctx: context,
        }
    }
    pub fn new_native_function(name: String, param_count: usize, native_func: fn(&mut Self, Vec<Rc<RefCell<Element>>>) -> Rc<RefCell<Element>>) -> Element {
        Element::NativeFunction {
            common: ElementCommon::new(),
            name,
            param_count,
            native_func,
        }
    }
    pub fn is_none(&self) -> bool {
        match self {
            Element::Null { .. } => true,
            _ => false,
        }
    }
    pub fn get_id(&self) -> u64 {
        match self {
            Element::Object(common) => common.el_id,
            Element::Number { common, .. } => common.el_id,
            Element::String { common, .. } => common.el_id,
            Element::Boolean { common, .. } => common.el_id,
            Element::Null { common, .. } => common.el_id,
            Element::Array { common, .. } => common.el_id,
            Element::Error(ErrorElement { common, .. }) => common.el_id,
            Element::ProtoType { common, .. } => common.el_id,
            Element::Function { common, .. } => common.el_id,
            Element::NativeFunction { common, .. } => common.el_id,
        }
    }
    pub fn to_string(&self) -> String {
        match self {
            Element::Object(common) => common.to_string(),
            Element::Number { value, .. } => value.to_string(),
            Element::String { value, .. } => value.to_string(),
            Element::Boolean { value, .. } => value.to_string(),
            Element::Null { .. } => "null".to_string(),
            Element::Array { array, .. } => format!(
                "[{}]",
                array
                    .iter()
                    .map(|e| e.borrow().to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Element::Error(ErrorElement { common, .. }) => {
                format!("msg => {}; stack => {}", common.get("msg").unwrap_or(get_null()).borrow().to_string(),
                        common.get("stack").unwrap_or(get_null()).borrow().to_string())
            }
            Element::ProtoType { common, .. } => format!("{{ {} }}", common.to_string()),
            Element::Function { body, .. } => "FUNCTION".to_string(), //Statement::Block(body.clone()).to_string(),
            Element::NativeFunction { .. } => "NATIVEFUNC".to_string(),
        }
    }
    pub fn get(&self, key: &str) -> Option<Rc<RefCell<Element>>> {
        debug!("get {}", key);
        match self {
            Element::Object(common) => common.get(key),
            Element::Number { common, .. } => if key == "type" {Some(Rc::new(RefCell::new(Element::new_string("number".to_string())))) } else { common.get(key)},
            Element::String { common, .. } => if key == "type" {Some(Rc::new(RefCell::new(Element::new_string("string".to_string())))) } else { common.get(key)},
            Element::Boolean { common, .. } => if key == "type" {Some(Rc::new(RefCell::new(Element::new_string("boolean".to_string())))) } else { common.get(key)},
            Element::Null { common, .. } => if key == "constructor" || key == "$$pro$$" { None } else { panic_any(SimpleError::new("null point exception"))},
            Element::Array { common, .. } => if key == "type" {Some(Rc::new(RefCell::new(Element::new_string("array".to_string())))) } else { common.get(key)},
            Element::Error(ErrorElement { common, .. }) => if key == "type" {Some(Rc::new(RefCell::new(Element::new_string("error".to_string())))) } else { common.get(key)},
            Element::ProtoType { common, .. } => common.get(key),
            Element::Function { common, .. } => if key == "type" {Some(Rc::new(RefCell::new(Element::new_string("function".to_string())))) } else { common.get(key)},
            Element::NativeFunction { common, .. } => if key == "type" {Some(Rc::new(RefCell::new(Element::new_string("function".to_string())))) } else { common.get(key)},
        }
    }
    pub fn get_from_cur(&self, key: &str) -> Option<Rc<RefCell<Element>>> {
        match self {
            Element::Object(common) => common.get_from_cur(key),
            Element::Number { common, .. } => common.get_from_cur(key),
            Element::String { common, .. } => common.get_from_cur(key),
            Element::Boolean { common, .. } => common.get_from_cur(key),
            Element::Null { common, .. } => common.get_from_cur(key),
            Element::Array { common, .. } => common.get_from_cur(key),
            Element::Error(ErrorElement { common, .. }) => common.get_from_cur(key),
            Element::ProtoType { common, .. } => common.get_from_cur(key),
            Element::Function { common, .. } => common.get_from_cur(key),
            Element::NativeFunction { common, .. } => common.get_from_cur(key),
        }
    }
    pub fn set(&mut self, key: &str, val: Rc<RefCell<Element>>) {
        match self {
            Element::Object(common) => common.set(key, val),
            Element::Number { common, .. } => common.set(key, val),
            Element::String { common, .. } => common.set(key, val),
            Element::Boolean { common, .. } => common.set(key, val),
            Element::Null { common, .. } => common.set(key, val),
            Element::Array { common, .. } => common.set(key, val),
            Element::Error(ErrorElement { common, .. }) => common.set(key, val),
            Element::ProtoType { common, .. } => common.set(key, val),
            Element::Function { common, .. } => common.set(key, val),
            Element::NativeFunction { common, .. } => common.set(key, val),
        }
    }

    pub fn get_proto_type(&self) -> Rc<RefCell<Element>> {
        match self {
            Element::Object(common) => common.get(PROTO_TYPE).unwrap_or(get_null()),
            Element::Number { common, .. } => common.get(PROTO_TYPE).unwrap_or(get_null()),
            Element::String { common, .. } => common.get(PROTO_TYPE).unwrap_or(get_null()),
            Element::Boolean { common, .. } => common.get(PROTO_TYPE).unwrap_or(get_null()),
            Element::Null { common, .. } => common.get(PROTO_TYPE).unwrap_or(get_null()),
            Element::Array { common, .. } => common.get(PROTO_TYPE).unwrap_or(get_null()),
            Element::Error(ErrorElement { common, .. }) => {
                common.get(PROTO_TYPE).unwrap_or(get_null())
            }
            Element::ProtoType { common, .. } => common.get(PROTO_TYPE).unwrap_or(get_null()),
            Element::Function { common, .. } => common.get(PROTO_TYPE).unwrap_or(get_null()),
            Element::NativeFunction { common, .. } => common.get(PROTO_TYPE).unwrap_or(get_null()),
        }
    }

    pub fn set_proto_type(&mut self, proto_type: Rc<RefCell<Element>>) {
        match self {
            Element::Object(common) => common.set(PROTO_TYPE, proto_type),
            Element::Number { common, .. } => common.set(PROTO_TYPE, proto_type),
            Element::String { common, .. } => common.set(PROTO_TYPE, proto_type),
            Element::Boolean { common, .. } => common.set(PROTO_TYPE, proto_type),
            Element::Null { common, .. } => common.set(PROTO_TYPE, proto_type),
            Element::Array { common, .. } => common.set(PROTO_TYPE, proto_type),
            Element::Error(ErrorElement { common, .. }) => common.set(PROTO_TYPE, proto_type),
            Element::ProtoType { common, .. } => common.set(PROTO_TYPE, proto_type),
            Element::Function { common, .. } => common.set(PROTO_TYPE, proto_type),
            Element::NativeFunction { common, .. } => common.set(PROTO_TYPE, proto_type),
        }
    }
    pub fn to_bool(&self) -> bool {
        match self {
            Element::Number { value, .. } => *value != 0.0,
            Element::Boolean { value, .. } => value.clone(),
            Element::Null { .. } => false,
            _ => true,
        }
    }

    pub fn function_call(
        &self,
        name: &str,
        args: Vec<Rc<RefCell<Element>>>,
        this: Option<Rc<RefCell<Element>>>,
        su: Option<Rc<RefCell<Element>>>,
        position: &str,
    ) -> Rc<RefCell<Element>> {
        match self {
            Element::Function {
                params,
                body,
                closure_ctx,
                ..
            } => {
                let parent_ctx = closure_ctx.clone(); // 按照闭包所在的上下文作为parent

                let mut new_ctx = Context::new(Some(parent_ctx));

                debug!(
                    "{}",
                    format!(
                        "function body = {}",
                        Statement::Block(body.clone()).to_string()
                    )
                );
                if this.is_some() {
                    let this = this.unwrap();
                    debug!("this={}", this.borrow().to_string());
                    new_ctx.set("this", this);
                }
                if su.is_some() {
                    let su = su.unwrap();
                    debug!("super={}", su.borrow().to_string());
                    new_ctx.set("super", su);
                }

                new_ctx.fun_ctx.name = Some(name.to_string());
                // 设置入参到变量上下文
                for (i, param) in params.iter().enumerate() {
                    let arg = args.iter().nth(i);
                    if let Some(arg) = arg {
                        new_ctx.set(param, (*arg).clone());
                    } else {
                        new_ctx.set(param, get_null());
                    }
                }

                let new_ctx = Rc::new(RefCell::new(new_ctx));

                // 执行函数语句
                let eval_res =
                    panic::catch_unwind(AssertUnwindSafe(|| eval_block(body, new_ctx.clone())));

                match eval_res {
                    Ok(_) => {}
                    Err(e) => {
                        let res = e.downcast::<SimpleError>();
                        match res {
                            Ok(simple_err) => {
                                let msg = simple_err.msg.to_string();
                                let mut stack = simple_err.stack.clone();
                                let length = stack.len();
                                let mut last = &mut stack[length - 1];
                                if last.function_name == "" {
                                    last.function_name = name.to_string();
                                }
                                stack.push(ErrorInfo {
                                    function_name: "".to_string(),
                                    position: position.to_string(),
                                });
                                panic_any(SimpleError { msg, stack })
                            }
                            Err(e) => panic_any(e),
                        }
                    }
                }

                // 从上下文中获取返回值
                let res = match new_ctx.borrow().fun_ctx.return_element {
                    None => get_null(),
                    Some(ref res) => res.clone(),
                };
                debug!("function {} return {}", name, res.borrow().to_string());
                res
            }
            _ => panic_any(SimpleError::new("Not callable")),
        }
    }

    pub fn native_function_call(&mut self,
                                name: &str,
                                args: Vec<Rc<RefCell<Element>>>,
                                this: Option<Rc<RefCell<Element>>>,
                                su: Option<Rc<RefCell<Element>>>,
                                position: &str,
    ) -> Rc<RefCell<Element>> {
        match self {
            Element::NativeFunction { name, param_count, native_func,.. } => {
                let mut args_clone = args.clone();
                while args_clone.len() < *param_count {
                    args_clone.push(get_null());
                }
                return native_func(&mut this.unwrap_or(get_null()).borrow_mut(), args_clone);
            }
            _ => panic_any(SimpleError::new("Not callable")),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ErrorElement {
    common: ElementCommon,
}

impl ErrorElement {
    pub fn new(msg: &str, stack: Vec<ErrorInfo>) -> Self {
        let mut common = ElementCommon::new();
        common.set(
            "msg",
            Rc::new(RefCell::new(Element::new_string(msg.to_string()))),
        );
        let stack = stack
            .iter()
            .map(|e| Rc::new(RefCell::new(e.to_element())))
            .collect::<Vec<Rc<RefCell<Element>>>>();
        common.set("stack", Rc::new(RefCell::new(Element::new_array(stack))));
        ErrorElement { common }
    }
    pub fn push_stack(&mut self, info: ErrorInfo) {
        let stack = self.common.get("stack").unwrap();
        let mut stack = stack.borrow_mut();
        match &mut *stack {
            Element::Array { array, .. } => array.push_back(Rc::new(RefCell::new(info.to_element()))),
            _ => panic_any(SimpleError::new("Stack element is not an array")),
        }
    }
    pub fn update_function_name(&mut self, name: &str) {
        self.common.set(
            "function_name",
            Rc::new(RefCell::new(Element::new_string(name.to_string()))),
        );
    }

    pub fn to_thread_safe(&self) -> SimpleError {
        let stack: Rc<RefCell<Element>> = self.common.get("stack").unwrap();
        let x = if let Element::Array { array, .. } = &*stack.borrow() {
            SimpleError {
                msg: self.common.get("msg").unwrap().borrow().to_string(),
                stack: array
                    .iter()
                    .map(|it| ErrorInfo::from_element(&it.borrow()))
                    .collect::<Vec<_>>(),
            }
        } else {
            panic_any(SimpleError::new("Stack element is not an array"))
        };
        x
    }

}

#[derive(Clone, Debug)]
pub struct SimpleError {
    pub(crate) msg: String,
    pub(crate) stack: Vec<ErrorInfo>,
}
#[derive(Clone, Debug)]
pub struct ErrorInfo {
    function_name: String,
    position: String,
}
impl SimpleError {
    pub fn new(msg: &str) -> Self {
        SimpleError {
            msg: msg.to_string(),
            stack: vec![ErrorInfo {
                function_name: "".to_string(),
                position: "".to_string(),
            }]
        }
    }
}
impl ErrorInfo {
    pub fn new(function_name: String, position: String) -> ErrorInfo {
        ErrorInfo {
            function_name,
            position,
        }
    }

    pub fn to_element(&self) -> Element {
        let mut common = ElementCommon::new();
        common.set(
            "function_name",
            Rc::new(RefCell::new(Element::new_string(
                self.function_name.to_string(),
            ))),
        );
        common.set(
            "position",
            Rc::new(RefCell::new(Element::new_string(self.position.to_string()))),
        );
        Element::Object({ common })
    }

    pub fn from_element(element: &Element) -> ErrorInfo {
        let function_name = element.get("function_name").unwrap().borrow().to_string();
        let position = element.get("position").unwrap().borrow().to_string();
        ErrorInfo {
            function_name,
            position,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Context {
    pub parent: Option<Rc<RefCell<Context>>>,
    pub variables: HashMap<String, Rc<RefCell<Element>>>,
    pub fun_ctx: FunCtx,
    pub for_ctx: ForCtx,
}
#[derive(Clone, Debug)]
pub struct FunCtx {
    pub name: Option<String>,
    pub return_element: Option<Rc<RefCell<Element>>>,
}
#[derive(Clone, Debug)]
pub struct ForCtx {
    pub in_for: bool,
    pub is_break: bool,
    pub is_continue: bool,
}
impl Context {
    pub fn new(parent: Option<Rc<RefCell<Context>>>) -> Context {
        Context {
            parent,
            variables: HashMap::new(),
            fun_ctx: FunCtx {
                name: None,
                return_element: None,
            },
            for_ctx: ForCtx {
                in_for: false,
                is_break: false,
                is_continue: false,
            },
        }
    }

    pub fn set(&mut self, key: &str, val: Rc<RefCell<Element>>) {
        self.variables.insert(key.to_string(), val);
    }

    pub fn update(&mut self, key: &str, val: Rc<RefCell<Element>>) {
        if self.variables.contains_key(key) {
            self.variables.insert(key.to_string(), val);
        } else if self.parent.is_some() {
            self.parent.clone().unwrap().borrow_mut().update(key, val);
        } else {
            panic_any(SimpleError::new(&format!("Undefined variable {}", key)));
        }
    }

    pub fn get(&self, key: &str) -> Rc<RefCell<Element>> {
        let cur = self.variables.get(key);
        if let Some(cur) = cur {
            cur.clone()
        } else if self.parent.is_none() {
            return get_null();
        } else {
            let mut parent = self.parent.as_ref().unwrap().borrow_mut();
            parent.get(key)
        }
    }

    pub fn get_function_name(&self) -> String {
        match &self.fun_ctx.name {
            None => {
                if self.parent.is_some() {
                    self.parent
                        .as_ref()
                        .unwrap()
                        .borrow()
                        .get_function_name()
                } else {
                    "__root__".to_string()
                }
            }
            Some(name) => return name.clone(),
        }
    }

    pub fn set_return_element(&mut self, val: Rc<RefCell<Element>>) {
        debug!("set return_element {}", val.borrow().to_string());
        self.fun_ctx.return_element = Some(val.clone());
        if self.fun_ctx.name.is_none() {
            match &self.parent {
                None => panic_any(SimpleError::new("return should in function")),
                Some(parent) => {
                    parent.borrow_mut().set_return_element(val.clone());
                }
            }
        }
    }

    pub fn set_break(&mut self) {
        self.for_ctx.is_break = true;
        if !self.for_ctx.in_for {
            if self.fun_ctx.name.is_some() {
                panic_any(SimpleError::new("break should in for"));
            }
            match &self.parent {
                None => panic_any(SimpleError::new("break should in for")),
                Some(parent) => {
                    parent.borrow_mut().set_break();
                }
            }
        }
    }

    pub fn set_continue(&mut self) {
        self.for_ctx.is_continue = true;
        if !self.for_ctx.in_for {
            if self.fun_ctx.name.is_some() {
                panic_any(SimpleError::new("continue should in for"));
            }
            match &self.parent {
                None => panic_any(SimpleError::new("continue should in for")),
                Some(parent) => {
                    parent.borrow_mut().set_continue();
                }
            }
        }
    }
}
pub fn get_null() -> Rc<RefCell<Element>> {
    Rc::new(RefCell::new(Element::new_null()))
}

pub fn get_true() -> Rc<RefCell<Element>> {
    Rc::new(RefCell::new(Element::new_boolean(true)))
}

pub fn get_false() -> Rc<RefCell<Element>> {
    Rc::new(RefCell::new(Element::new_boolean(false)))
}
pub fn get_array_proto() -> Rc<RefCell<Element>> {
    let mut methods = HashMap::new();
    let length = |this: &mut Element, vec: Vec<Rc<RefCell<Element>>>| match this {
        Element::Array{ array, .. } => Rc::new(RefCell::new(Element::new_number(array.len() as f64))),
        _ => get_null(),
    };
    let push = |this: &mut Element, vec: Vec<Rc<RefCell<Element>>>| {
        match this {
            Element::Array{ array, .. } => { array.push_back(vec[0].clone()); },
            _ => ()
        }
        get_null()
    };
    let pop = |this: &mut Element, vec: Vec<Rc<RefCell<Element>>>| {
        let item = match this {
            Element::Array{ array, .. } => array.pop_back(),
            _ => None
        };
        match item {
            Some(item) => item,
            _ => get_null()
        }
    };
    let unshift = |this: &mut Element, vec: Vec<Rc<RefCell<Element>>>| {
        match this {
            Element::Array{ array, .. } => { array.push_front(vec[0].clone()); },
            _ => ()
        }
        get_null()
    };
    let shift = |this: &mut Element, vec: Vec<Rc<RefCell<Element>>>| {
        let item = match this {
            Element::Array{ array, .. } => array.pop_front(),
            _ => None
        };
        match item {
            Some(item) => item,
            _ => get_null()
        }
    };
    let join = |this: &mut Element, vec: Vec<Rc<RefCell<Element>>>| {
        match this {
            Element::Array{ array, .. } => Rc::new(RefCell::new(Element::new_string(array.iter().map(|it| it.borrow().to_string()).collect::<Vec<String>>().join(vec[0].borrow().to_string().as_str())))),
            _ => get_null()
        }
    };
    methods.insert("length".to_string(), Rc::new(RefCell::new(Element::new_native_function("length".to_string(), 0, length))));
    methods.insert("push".to_string(), Rc::new(RefCell::new(Element::new_native_function("push".to_string(), 1, push))));
    methods.insert("pop".to_string(), Rc::new(RefCell::new(Element::new_native_function("pop".to_string(), 0, pop))));
    methods.insert("unshift".to_string(), Rc::new(RefCell::new(Element::new_native_function("unshift".to_string(), 1, unshift))));
    methods.insert("shift".to_string(), Rc::new(RefCell::new(Element::new_native_function("shift".to_string(), 0, shift))));
    methods.insert("join".to_string(), Rc::new(RefCell::new(Element::new_native_function("join".to_string(), 1, join))));
    Rc::new(RefCell::new(Element::new_prototype("", None, methods)))
}

pub fn get_string_proto() -> Rc<RefCell<Element>> {
    let mut methods = HashMap::new();
    let length = |this: &mut Element, vec: Vec<Rc<RefCell<Element>>>| match this {
        Element::String{ value, .. } => Rc::new(RefCell::new(Element::new_number(value.chars().collect::<Vec<char>>().len() as f64))),
        _ => get_null(),
    };
    let split = |this: &mut Element, vec: Vec<Rc<RefCell<Element>>>| match this {
        Element::String{ value, .. } => {
            let arr = value.split(&vec[0].borrow().to_string()).collect::<Vec<&str>>();
            let array = arr.iter().map(|it| Rc::new(RefCell::new(Element::new_string(it.to_string())))).collect::<Vec<Rc<RefCell<Element>>>>();
            Rc::new(RefCell::new(Element::new_array(array)))
        },
        _ => get_null(),
    };
    let index_of = |this: &mut Element, vec: Vec<Rc<RefCell<Element>>>| match this {
        Element::String{ value, .. } => {
            match value.find(&vec[0].borrow().to_string().as_str()) {
                None => Rc::new(RefCell::new(Element::new_number(-1 as f64))),
                Some(index) =>Rc::new(RefCell::new(Element::new_number(index as f64)))
            }
        },
        _ => get_null(),
    };
    let starts_with = |this: &mut Element, vec: Vec<Rc<RefCell<Element>>>| match this {
        Element::String{ value, .. } => {
            if value.starts_with(&vec[0].borrow().to_string().as_str()) {
                get_true()
            } else {
                get_false()
            }
        },
        _ => get_null(),
    };
    let ends_with = |this: &mut Element, vec: Vec<Rc<RefCell<Element>>>| match this {
        Element::String{ value, .. } => {
            if value.ends_with(&vec[0].borrow().to_string().as_str()) {
                get_true()
            } else {
                get_false()
            }
        },
        _ => get_null(),
    };
    let replace_all = |this: &mut Element, vec: Vec<Rc<RefCell<Element>>>| match this {
        Element::String{ value, .. } => {
            let res = value.replace(&vec[0].borrow().to_string().as_str(), &vec[1].borrow().to_string().as_str());
            Rc::new(RefCell::new(Element::new_string(res)))
        },
        _ => get_null(),
    };
    let substring = |this: &mut Element, vec: Vec<Rc<RefCell<Element>>>| match this {
        Element::String{ value, .. } => {
            let value = value.chars().collect::<Vec<char>>();
            match (&*vec[0].borrow(), &*vec[1].borrow()) {
                (Element::Number {value: start,..}, Element::Number {value: end,..}) => {
                    let res = &value[(*start as usize) ..(*end as usize)];
                    let res = String::from_iter(res.iter());
                    Rc::new(RefCell::new(Element::new_string(res)))
                }
                (_,_) => {
                    panic_any(SimpleError::new("substring need 2 number params"))
                }
            }
        },
        _ => get_null(),
    };
    let to_uppercase = |this: &mut Element, vec: Vec<Rc<RefCell<Element>>>| match this {
        Element::String{ value, .. } => {
            Rc::new(RefCell::new(Element::new_string(value.to_uppercase().to_string())))
        },
        _ => get_null(),
    };
    let to_lowercase = |this: &mut Element, vec: Vec<Rc<RefCell<Element>>>| match this {
        Element::String{ value, .. } => {
            Rc::new(RefCell::new(Element::new_string(value.to_lowercase().to_string())))
        },
        _ => get_null(),
    };
    let trim = |this: &mut Element, vec: Vec<Rc<RefCell<Element>>>| match this {
        Element::String{ value, .. } => {
            Rc::new(RefCell::new(Element::new_string(value.trim().to_string())))
        },
        _ => get_null(),
    };
    let trim_left = |this: &mut Element, vec: Vec<Rc<RefCell<Element>>>| match this {
        Element::String{ value, .. } => {
            Rc::new(RefCell::new(Element::new_string(value.trim_start().to_string())))
        },
        _ => get_null(),
    };
    let trim_right = |this: &mut Element, vec: Vec<Rc<RefCell<Element>>>| match this {
        Element::String{ value, .. } => {
            Rc::new(RefCell::new(Element::new_string(value.trim_end().to_string())))
        },
        _ => get_null(),
    };
    let to_number = |this: &mut Element, vec: Vec<Rc<RefCell<Element>>>| match this {
        Element::String{ value, .. } => {
            match value.parse::<f64>() {
                Ok(n) => {
                    Rc::new(RefCell::new(Element::new_number(n)))
                }
                Err(_) => {
                    panic_any(SimpleError::new("string cannot be parsed as number"))
                }
            }
        },
        _ => get_null(),
    };
    let char_at = |this: &mut Element, vec: Vec<Rc<RefCell<Element>>>| match this {
        Element::String{ value, .. } => {
            match &*vec[0].borrow() {
                Element::Number{ value : index, .. } => {
                    let v: Vec<char> = value.chars().collect::<Vec<char>>();
                    match v.iter().nth(*index as usize) {
                        None => {
                            get_null()
                        }
                        Some(c) => {
                            Rc::new(RefCell::new(Element::new_string(format!("{}", c))))
                        }
                    }
                }
                _ => panic_any(SimpleError::new("charAt should use a number"))
            }
        },
        _ => get_null(),
    };
    methods.insert("length".to_string(), Rc::new(RefCell::new(Element::new_native_function("length".to_string(), 0, length))));
    methods.insert("split".to_string(), Rc::new(RefCell::new(Element::new_native_function("split".to_string(), 1, split))));
    methods.insert("indexOf".to_string(), Rc::new(RefCell::new(Element::new_native_function("indexOf".to_string(), 1, index_of))));
    methods.insert("charAt".to_string(), Rc::new(RefCell::new(Element::new_native_function("charAt".to_string(), 1, char_at))));
    methods.insert("replaceAll".to_string(), Rc::new(RefCell::new(Element::new_native_function("replaceAll".to_string(), 2, replace_all))));
    methods.insert("startsWith".to_string(), Rc::new(RefCell::new(Element::new_native_function("startsWith".to_string(), 1, starts_with))));
    methods.insert("endsWith".to_string(), Rc::new(RefCell::new(Element::new_native_function("endsWith".to_string(), 1, ends_with))));
    methods.insert("substring".to_string(), Rc::new(RefCell::new(Element::new_native_function("substring".to_string(), 2, substring))));
    methods.insert("toUpperCase".to_string(), Rc::new(RefCell::new(Element::new_native_function("toUpperCase".to_string(), 0, to_uppercase))));
    methods.insert("toLowerCase".to_string(), Rc::new(RefCell::new(Element::new_native_function("toLowerCase".to_string(), 0, to_lowercase))));
    methods.insert("trim".to_string(), Rc::new(RefCell::new(Element::new_native_function("trim".to_string(), 0, trim))));
    methods.insert("trimLeft".to_string(), Rc::new(RefCell::new(Element::new_native_function("trimLeft".to_string(), 0, trim_left))));
    methods.insert("trimRight".to_string(), Rc::new(RefCell::new(Element::new_native_function("trimRight".to_string(), 0, trim_right))));
    methods.insert("toNumber".to_string(), Rc::new(RefCell::new(Element::new_native_function("toNumber".to_string(), 0, to_number))));
    Rc::new(RefCell::new(Element::new_prototype("", None, methods)))
}