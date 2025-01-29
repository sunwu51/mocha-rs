use std::cell::{Ref, RefCell};
use std::collections::HashMap;
use std::fmt::format;
use std::{fs, io};
use std::fs::{File, OpenOptions};
use std::io::Write;
use std::panic::panic_any;
use std::path::Path;
use std::rc::Rc;
use std::sync::Mutex;
use chrono::Utc;
use lazy_static::lazy_static;
use rand::random;
use serde_json::Value;
use crate::eval::{get_false, get_null, get_true, Context, Element, SimpleError, PROTO_TYPE};
use reqwest::blocking::{Client, Request, RequestBuilder};
use reqwest::{header, Method, Url};
use reqwest::header::HeaderMap;

pub fn init(mut ctx: Context) -> Context {
    // Math库
    {
        let mut math_methods = HashMap::new();
        let random = |this: &mut Element, vec: Vec<Rc<RefCell<Element>>>| Rc::new(RefCell::new(Element::new_number(random::<f64>())));
        let floor = |this: &mut Element, vec: Vec<Rc<RefCell<Element>>>| {
            match &*vec[0].borrow() {
                Element::Number { value, .. } => Rc::new(RefCell::new(Element::new_number(value.floor()))),
                _ => panic_any("floor param should be a number")
            }
        };
        let ceil = |this: &mut Element, vec: Vec<Rc<RefCell<Element>>>| {
            match &*vec[0].borrow() {
                Element::Number { value, .. } => Rc::new(RefCell::new(Element::new_number(value.ceil()))),
                _ => panic_any("ceil param should be a number")
            }
        };
        let abs = |this: &mut Element, vec: Vec<Rc<RefCell<Element>>>| {
            match &*vec[0].borrow() {
                Element::Number { value, .. } => Rc::new(RefCell::new(Element::new_number(value.abs()))),
                _ => panic_any("abs param should be a number")
            }
        };
        math_methods.insert("random".to_string(), Rc::new(RefCell::new(Element::new_native_function("random".to_string(), 0, random))));
        math_methods.insert("floor".to_string(), Rc::new(RefCell::new(Element::new_native_function("floor".to_string(), 1, floor))));
        math_methods.insert("ceil".to_string(), Rc::new(RefCell::new(Element::new_native_function("ceil".to_string(), 1, ceil))));
        math_methods.insert("abs".to_string(), Rc::new(RefCell::new(Element::new_native_function("ceil".to_string(), 1, abs))));
        let math = Element::new_prototype("Math", None, math_methods);
        ctx.set("Math", Rc::new(RefCell::new(math)));
    }
    // Time库
    {
        let mut time_methods = HashMap::new();
        let now = |this: &mut Element, vec: Vec<Rc<RefCell<Element>>>| Rc::new(RefCell::new(Element::new_number(Utc::now().timestamp_millis() as f64)));
        let sleep = |this: &mut Element, vec: Vec<Rc<RefCell<Element>>>| {
            match &*vec[0].borrow() {
                Element::Number { value, .. } => {std::thread::sleep(std::time::Duration::from_millis(*value as u64))},
                _ => panic_any("ceil param should be a number")
            }
            get_null()
        };
        time_methods.insert("now".to_string(), Rc::new(RefCell::new(Element::new_native_function("now".to_string(), 0, now))));
        time_methods.insert("sleep".to_string(), Rc::new(RefCell::new(Element::new_native_function("sleep".to_string(), 1, sleep))));
        let time = Element::new_prototype("Time", None, time_methods);
        ctx.set("Time", Rc::new(RefCell::new(time)));
    }
    // Json库
    {
        let mut json_methods = HashMap::new();
        let stringify = |this: &mut Element, vec: Vec<Rc<RefCell<Element>>>| {
            Rc::new(RefCell::new(Element::new_string(element_to_json(vec[0].clone()))))
        };
        let parse = |this: &mut Element, vec: Vec<Rc<RefCell<Element>>>| {
            let result: Result<Value, _> = serde_json::from_str(&vec[0].borrow().to_string());

            if result.is_err() {
                panic_any(SimpleError::new("json parse error"))
            }
            let value = result.unwrap();
            json_to_element(&value)
        };


        json_methods.insert("stringify".to_string(), Rc::new(RefCell::new(Element::new_native_function("stringify".to_string(), 1, stringify))));
        json_methods.insert("parse".to_string(), Rc::new(RefCell::new(Element::new_native_function("parse".to_string(), 1, parse))));
        let json = Element::new_prototype("Json", None, json_methods);
        ctx.set("Json", Rc::new(RefCell::new(json)));
    }
    // File库
    {
        let mut file_methods = HashMap::new();
        let read_file = |this: &mut Element, vec: Vec<Rc<RefCell<Element>>>| {
            let result = fs::read_to_string(vec[0].borrow().to_string());
            if result.is_err() {
                panic_any(SimpleError::new("read file error, please check"))
            }
            Rc::new(RefCell::new(Element::new_string(result.unwrap())))
        };
        let write_file = |this: &mut Element, vec: Vec<Rc<RefCell<Element>>>| {
            let result = write_to_file(vec[0].borrow().to_string(), &vec[1].borrow().to_string());
            if result.is_err() {
                panic_any(SimpleError::new("write file error, please check"))
            }
            get_null()
        };
        let append_file = |this: &mut Element, vec: Vec<Rc<RefCell<Element>>>| {
            let result = append_to_file(vec[0].borrow().to_string(), &vec[1].borrow().to_string());
            if result.is_err() {
                panic_any(SimpleError::new("append file error, please check"))
            }
            get_null()
        };
        file_methods.insert("readFile".to_string(), Rc::new(RefCell::new(Element::new_native_function("readFile".to_string(), 1, read_file))));
        file_methods.insert("writeFile".to_string(), Rc::new(RefCell::new(Element::new_native_function("writeFile".to_string(), 1, write_file))));
        file_methods.insert("appendFile".to_string(), Rc::new(RefCell::new(Element::new_native_function("appendFile".to_string(), 1, append_file))));
        let file = Element::new_prototype("File", None, file_methods);
        ctx.set("File", Rc::new(RefCell::new(file)));
    }
    // Http库
    {
        let mut http_methods = HashMap::new();

        let request = |this: &mut Element, vec: Vec<Rc<RefCell<Element>>>| {
            let method = Method::from_bytes(vec[0].borrow().to_string().trim().to_uppercase().as_bytes());
            if method.is_err() {
                panic_any(SimpleError::new("request method invalid"))
            }
            let url = Url::parse(vec[1].borrow().to_string().as_str());
            if url.is_err() {
                panic_any(SimpleError::new("request url parse error"))
            }
            let method = method.unwrap();
            let url = url.unwrap();
            let client = Client::new();
            let mut request_builder: RequestBuilder = client
                .request(method, url);
            let headers = vec[2].borrow().get("headers");
            if headers.is_some() {
                let headers = headers.unwrap();
                let header_map = match &*headers.borrow() {
                    Element::Object(common) => {
                        let opt = &common.map;
                        let mut header_map = HeaderMap::new();
                        for (k, v) in opt {
                            let k = get_static_str(k.as_str());
                            header_map.insert(k, v.borrow().to_string().parse().unwrap());
                        }
                        header_map
                    }
                    _ => panic_any(SimpleError::new("request option invalid"))
                };
                request_builder = request_builder.headers(header_map);
            };
            let body = vec[2].borrow().get("body");
            if body.is_some() {
                let body = body.unwrap();
                let body_str = match &*body.borrow() {
                    Element::Object(_) => {element_to_json(body.clone())}
                    Element::Array{ .. } => {element_to_json(body.clone())}
                    _ => body.borrow().to_string()
                };
                request_builder = request_builder.body(body_str);
            }
            let result = client.execute(request_builder.build().unwrap());
            match result {
                Ok(result) => {
                    let status = result.status().as_u16() as f64;
                    let txt = result.text();
                    if txt.is_err() {
                        panic_any(SimpleError::new("response not text"))
                    }
                    let mut res = Element::new();
                    res.set("body", Rc::new(RefCell::new(Element::new_string(txt.unwrap()))));
                    res.set("status", Rc::new(RefCell::new(Element::new_number(status))));
                    Rc::new(RefCell::new(res))
                }
                Err(_) => {
                    panic_any(SimpleError::new("request failed"))
                }
            }

        };
        http_methods.insert("request".to_string(), Rc::new(RefCell::new(Element::new_native_function("request".to_string(), 3, request))));
        let http = Element::new_prototype("Http", None, http_methods);
        ctx.set("Http", Rc::new(RefCell::new(http)));
    }
    ctx
}



pub fn get_build_in_ctx() -> Context {
    let ctx = Context::new(None);
    init(ctx)
}

fn element_to_json(ele: Rc<RefCell<Element>>) -> String {
    match &*ele.borrow() {
        Element::Object(common) => {
            let content = common.map.iter().flat_map(|(k, v)| {
                if k != PROTO_TYPE {
                    vec![format!(r#""{}": {}"#, k, element_to_json(v.clone()))]
                } else {
                    vec![]
                }
            }).collect::<Vec<String>>().join(", ");
            format!("{{ {} }}", content)
        }
        Element::Number { value, .. } => format!("{}", value),
        Element::String { value, .. } => format!("{}", escape(value)),
        Element::Boolean { value, .. } => format!("{}", value),
        Element::Null { .. } => "null".to_string(),
        Element::Array { array, .. } => {
            let content = array.iter().map(|item| element_to_json(item.clone())).collect::<Vec<String>>().join(", ");
           format!("[ {} ]", content)
        }
        _ => panic_any("type not support json stringify")
    }
}
fn json_to_element(value: &Value) -> Rc<RefCell<Element>> {
    match value {
        Value::Null => {return get_null()}
        Value::Bool(b) => {if *b { get_true() } else { get_false() } }
        Value::Number(n) => {Rc::new(RefCell::new(Element::new_number(n.as_f64().unwrap())))}
        Value::String(s) => {Rc::new(RefCell::new(Element::new_string(s.to_string())))}
        Value::Array(arr) => {
            let array = arr.iter().map(|it| json_to_element(it)).collect::<Vec<Rc<RefCell<Element>>>>();
            Rc::new(RefCell::new(Element::new_array(array)))
        }
        Value::Object(map) => {
            let mut obj = Element::new();
            map.iter().for_each(|(k, v)| {
                obj.set(k, json_to_element(v))
            });
            Rc::new(RefCell::new(obj))
        }
    }
}

fn write_to_file<P: AsRef<Path>>(path: P, content: &str) -> io::Result<()> {
    if let Some(parent) = path.as_ref().parent() {
        // Create the directory if it doesn't exist
        fs::create_dir_all(parent)?;
    }

    // Create or open the file in write mode
    let mut file = File::create(path)?;

    // Write the content to the file
    file.write_all(content.as_bytes())?;
    Ok(())
}
fn append_to_file<P: AsRef<Path>>(path: P, content: &str) -> io::Result<()> {
    if let Some(parent) = path.as_ref().parent() {
        // Create the directory if it doesn't exist
        fs::create_dir_all(parent)?;
    }

    // Create or open the file in write mode
    let mut file = OpenOptions::new()
        .write(true)
        .append(true)
        .create(true)
        .open(path)?;

    // Write the content to the file
    file.write_all(content.as_bytes())?;
    Ok(())
}

fn escape(input: &str) -> String {
    serde_json::to_string(input).unwrap_or_else(|_| String::new())
}

lazy_static! {
    static ref STRING_STORAGE: Mutex<HashMap<String, &'static str>> = Mutex::new(HashMap::new());
}
fn get_static_str(s: &str) -> &'static str {
    let mut storage = STRING_STORAGE.lock().unwrap();
    if let Some(&value) = storage.get(s) {
        value
    } else {
        let value: &'static str = Box::leak(s.to_string().into_boxed_str());
        storage.insert(s.to_string(), value);
        value
    }
}

