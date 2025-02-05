extern crate core;

mod eval;
mod lexer;
mod parser;
mod sdk;

use crate::eval::eval::eval_statements;
use crate::eval::SimpleError;
use crate::sdk::get_build_in_ctx;
use lexer::lexer as LEX;
use parser::parser as PARSER;
use std::cell::RefCell;
use std::io::Write;
use std::panic::{panic_any, AssertUnwindSafe};
use std::rc::Rc;
use std::{fs, io, panic};

fn main() {
    panic::set_hook(Box::new(|_| {}));
    env_logger::init();

    // 命令行参数
    let args: Vec<String> = std::env::args().collect();

    if args.len() > 1 {
        let filename = &args[1];
        match fs::read_to_string(filename) {
            Ok(code) => {
                let tokens = LEX::lex(&code);
                let statements = PARSER::Parser::new(tokens).parse();
                let ctx = Rc::new(RefCell::new(get_build_in_ctx()));
                let _ = eval_statements(&statements, ctx.clone(), true);
            }
            Err(e) => {
                eprintln!("Error reading file {}: {}", filename, e);
            }
        }
    } else {
        let ctx = Rc::new(RefCell::new(get_build_in_ctx()));
        loop {
            print!(">>> ");
            io::stdout().flush().unwrap();

            let mut code = String::new();
            match io::stdin().read_line(&mut code) {
                Ok(_) => {
                    match panic::catch_unwind(AssertUnwindSafe(|| {
                        let tokens = LEX::lex(&code);
                        let statements = PARSER::Parser::new(tokens).parse();
                        let res = eval_statements(&statements, ctx.clone(), false);
                        println!("{}", res.borrow().to_string())
                    })) {
                        Ok(_) => {}
                        Err(e) => {
                            let res = e.downcast::<SimpleError>();
                            match res {
                                Ok(simple_err) => {
                                    eprintln!("Error: {}", &simple_err.msg);
                                }
                                Err(e) => panic_any(e),
                            }
                        }
                    }
                }
                Err(e) => eprintln!("Error reading input: {}", e),
            }
        }
    }
}

/*
var a = {age: 10, name: "liming"}; print(a); a["age"] = 11; print(a); print(a.age); var b = "你好世界"; print(b[2]);
        var c = [1, 2, "你好", 4];  print(c); print(c[2]); print(c[3]);
        var add = function(a, b) {return a + b; };
        print(add(add(1, 11), false));
        if (a.age > 10) {
            print(a);
        } else { print(100);}

        for (var i = 0; i < 10; i++) {
            if (i >= 6) {
                break;
            }
            if (i%2 == 0) {
                continue;
            }
            print(i);
        }
 */
