use rand;
use std::{fmt, io, ops::Deref, rc::Rc, vec};

use crate::decoder::{Code, Optype, Program};

pub enum Value {
    Nil,
    Int(i64),
    Float(f64),
    Ref(Rc<Value>),
    List(Vec<Value>),
    Tuple(Vec<Value>),
    Fn(Vec<Value>, usize, usize),
    Builtin(usize),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Value::Nil => "nil",
            Value::Int(i) => &i.to_string(),
            Value::Float(f) => &f.to_string(),
            Value::Ref(r) => &format!("<ref {}>", Rc::strong_count(r)),
            Value::List(_) | Value::Tuple(_) | Value::Fn(_, _, _) | Value::Builtin(_) => panic!(),
        };
        f.write_str(s)
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        // println!("cloning {}", self);
        match self {
            Value::Nil => Value::Nil,
            Value::Int(i) => Value::Int(*i),
            Value::Float(f) => Value::Float(*f),
            Value::Ref(r) => Value::Ref(Rc::clone(r)),
            Value::List(_) | Value::Tuple(_) | Value::Fn(_, _, _) | Value::Builtin(_) => panic!(),
        }
    }
}

fn init_frame(u: usize) -> Vec<Value> {
    vec![Value::Nil; u]
}

fn str_to_val(s: &str) -> Value {
    let i = s.as_bytes().iter().map(|u| Value::Int(*u as i64)).collect();
    Value::List(i)
}

fn val_to_str(v: Value) -> String {
    match v {
        Value::List(cs) => cs
            .iter()
            .map(|v| match v {
                Value::Int(i) => *i as u8 as char,

                _ => panic!(),
            })
            .collect(),
        _ => panic!(),
    }
}

fn val_to_i64(v: Value) -> i64 {
    match v {
        Value::Int(i) => i,
        _ => panic!(),
    }
}

fn val_to_f64(v: Value) -> f64 {
    match v {
        Value::Float(f) => f,
        _ => panic!(),
    }
}

fn val_deref(v: &Value) -> &Value {
    match v {
        Value::Ref(r) => r.deref(),
        _ => panic!(),
    }
}

fn builtin_print(s: Value) -> Value {
    match s {
        Value::Ref(i) => match Rc::deref(&i) {
            Value::List(s) => s
                .iter()
                .map(|v| match v {
                    Value::Int(n) => *n as u8 as char,
                    _ => panic!(),
                })
                .for_each(|c| print!("{}", c)),
            _ => panic!(),
        },
        _ => panic!(),
    };
    Value::Int(0)
}

fn builtin_println(s: Value) -> Value {
    match s {
        Value::Ref(i) => match Rc::deref(&i) {
            Value::List(s) => s
                .iter()
                .map(|v| match v {
                    Value::Int(n) => *n as u8 as char,
                    _ => panic!(),
                })
                .for_each(|c| print!("{}", c)),
            _ => panic!(),
        },
        _ => panic!(),
    };
    println!();
    Value::Int(0)
}

fn builtin_readline(_: Value) -> Value {
    let s = match io::read_to_string(io::stdin()) {
        Err(_) => vec![],
        Ok(v) => v.bytes().map(|b| Value::Int(b as i64)).collect(),
    };

    let res = Value::List(s);

    Value::Ref(Rc::new(res))
}

fn builtin_itoa(s: Value) -> Value {
    let i = match s {
        Value::Int(i) => i,
        _ => panic!(),
    };

    let res = str_to_val(&i.to_string());

    Value::Ref(Rc::new(res))
}

fn builtin_ftoa(s: Value) -> Value {
    let f = match s {
        Value::Float(f) => f,
        _ => panic!(),
    };

    let res = str_to_val(&f.to_string());
    Value::Ref(Rc::new(res))
}

fn builtin_ftoi(s: Value) -> Value {
    let f = match s {
        Value::Float(f) => f,
        _ => panic!(),
    };

    Value::Int(f as i64)
}

fn builtin_atoi(s: Value) -> Value {
    let i = match s {
        Value::List(_) => {
            let s = val_to_str(s);
            match str::parse::<i64>(&s) {
                Ok(i) => i,
                Err(_) => 0i64,
            }
        }
        _ => panic!(),
    };

    Value::Int(i)
}

fn builtin_itof(s: Value) -> Value {
    let i = match s {
        Value::Int(i) => i,
        _ => panic!(),
    };

    Value::Float(i as f64)
}

fn builtin_rand(_: Value) -> Value {
    Value::Float(rand::random::<f64>())
}

pub fn run(p: Program) {
    let mut frames: Vec<Vec<Value>> = vec![];
    let mut stack: Vec<Value> = vec![];

    let mut jmplist: Vec<usize> = vec![];

    let mut pc = 0usize;
    let (sc, bcs) = p;
    let mut frame = init_frame(sc as usize);
    let builtins = [
        builtin_print,
        builtin_println,
        builtin_readline,
        builtin_itoa,
        builtin_ftoa,
        builtin_ftoi,
        builtin_itof,
        builtin_atoi,
        builtin_rand,
    ];
    (0..builtins.len()).into_iter().for_each(|i| {
        let b = Value::Builtin(i);
        frame[i] = Value::Ref(Rc::new(b));
    });
    frames.push(frame);

    let len = bcs.len();
    while pc < len {
        // println!("{}", pc);
        match &bcs[pc as usize] {
            Code::Pushi(i) => {
                stack.push(Value::Int(*i));
                pc += 1
            }
            Code::Pushf(f) => {
                stack.push(Value::Float(*f));
                pc += 1
            }
            Code::Pushc(u) => {
                stack.push(Value::Int(*u as i64));
                pc += 1
            }
            Code::Pop => match stack.pop() {
                Some(_) => pc += 1,
                None => panic!(),
            },

            Code::Op(op) => {
                match op {
                    Optype::INeg => {
                        let i = val_to_i64(stack.pop().unwrap());
                        stack.push(Value::Int(-i))
                    }
                    Optype::IAdd => {
                        let b = val_to_i64(stack.pop().unwrap());
                        let a = val_to_i64(stack.pop().unwrap());
                        stack.push(Value::Int(a + b))
                    }
                    Optype::ISub => {
                        let b = val_to_i64(stack.pop().unwrap());
                        let a = val_to_i64(stack.pop().unwrap());
                        stack.push(Value::Int(a - b))
                    }
                    Optype::IMul => {
                        let b = val_to_i64(stack.pop().unwrap());
                        let a = val_to_i64(stack.pop().unwrap());
                        stack.push(Value::Int(a * b))
                    }
                    Optype::IDiv => {
                        let b = val_to_i64(stack.pop().unwrap());
                        let a = val_to_i64(stack.pop().unwrap());
                        stack.push(Value::Int(a / b))
                    }
                    Optype::IMod => {
                        let b = val_to_i64(stack.pop().unwrap());
                        let a = val_to_i64(stack.pop().unwrap());
                        stack.push(Value::Int(a % b))
                    }
                    Optype::IEq => {
                        let b = val_to_i64(stack.pop().unwrap());
                        let a = val_to_i64(stack.pop().unwrap());
                        stack.push(Value::Int(if a == b { 1 } else { 0 }))
                    }
                    Optype::INeq => {
                        let b = val_to_i64(stack.pop().unwrap());
                        let a = val_to_i64(stack.pop().unwrap());
                        stack.push(Value::Int(if a != b { 1 } else { 0 }))
                    }
                    Optype::IGt => {
                        let b = val_to_i64(stack.pop().unwrap());
                        let a = val_to_i64(stack.pop().unwrap());
                        stack.push(Value::Int(if a > b { 1 } else { 0 }))
                    }
                    Optype::IGe => {
                        let b = val_to_i64(stack.pop().unwrap());
                        let a = val_to_i64(stack.pop().unwrap());
                        stack.push(Value::Int(if a >= b { 1 } else { 0 }))
                    }
                    Optype::ILt => {
                        let b = val_to_i64(stack.pop().unwrap());
                        let a = val_to_i64(stack.pop().unwrap());
                        stack.push(Value::Int(if a < b { 1 } else { 0 }))
                    }
                    Optype::ILe => {
                        let b = val_to_i64(stack.pop().unwrap());
                        let a = val_to_i64(stack.pop().unwrap());
                        stack.push(Value::Int(if a <= b { 1 } else { 0 }))
                    }
                    Optype::FNeg => {
                        let f = val_to_f64(stack.pop().unwrap());
                        stack.push(Value::Float(-f))
                    }
                    Optype::FAdd => {
                        let b = val_to_f64(stack.pop().unwrap());
                        let a = val_to_f64(stack.pop().unwrap());
                        stack.push(Value::Float(a + b))
                    }
                    Optype::FSub => {
                        let b = val_to_f64(stack.pop().unwrap());
                        let a = val_to_f64(stack.pop().unwrap());
                        stack.push(Value::Float(a - b))
                    }
                    Optype::FMul => {
                        let b = val_to_f64(stack.pop().unwrap());
                        let a = val_to_f64(stack.pop().unwrap());
                        stack.push(Value::Float(a * b))
                    }
                    Optype::FDiv => {
                        let b = val_to_f64(stack.pop().unwrap());
                        let a = val_to_f64(stack.pop().unwrap());
                        stack.push(Value::Float(a / b))
                    }
                    Optype::FGt => {
                        let b = val_to_f64(stack.pop().unwrap());
                        let a = val_to_f64(stack.pop().unwrap());
                        stack.push(Value::Int(if a > b { 1 } else { 0 }))
                    }
                    Optype::FLt => {
                        let b = val_to_f64(stack.pop().unwrap());
                        let a = val_to_f64(stack.pop().unwrap());
                        stack.push(Value::Int(if a < b { 1 } else { 0 }))
                    }
                    Optype::Not => {
                        let b = val_to_i64(stack.pop().unwrap());
                        stack.push(Value::Int(if b == 0 { 1 } else { 0 }))
                    }
                    Optype::And => {
                        let b = val_to_i64(stack.pop().unwrap());
                        let a = val_to_i64(stack.pop().unwrap());
                        stack.push(Value::Int(if a != 0 && b != 0 { 1 } else { 0 }))
                    }
                    Optype::Or => {
                        let b = val_to_i64(stack.pop().unwrap());
                        let a = val_to_i64(stack.pop().unwrap());
                        stack.push(Value::Int(if a != 0 || b != 0 { 1 } else { 0 }))
                    }
                    Optype::Xor => {
                        let b = val_to_i64(stack.pop().unwrap());
                        let a = val_to_i64(stack.pop().unwrap());
                        stack.push(Value::Int(if (a != 0) ^ (b != 0) { 1 } else { 0 }))
                    }
                };
                pc += 1
            }
            Code::List(n) => {
                let mut v: Vec<Value> = vec![];
                for _ in 0..*n {
                    v.push(stack.pop().unwrap());
                }
                v.reverse();
                let v = Value::List(v);
                stack.push(Value::Ref(Rc::new(v)));
                pc += 1
            }
            Code::At => {
                let i = val_to_i64(stack.pop().unwrap()) as usize;
                let r = &stack.pop().unwrap();
                match val_deref(r) {
                    Value::List(list) => stack.push(list[i].clone()),
                    _ => panic!(),
                };
                pc += 1
            }
            Code::Tuple(n) => {
                let mut v: Vec<Value> = vec![];
                for _ in 0..*n {
                    v.push(stack.pop().unwrap());
                }
                v.reverse();
                let v = Value::Tuple(v);
                stack.push(Value::Ref(Rc::new(v)));
                pc += 1
            }
            Code::Destruct => {
                let r = &stack.pop().unwrap();
                match val_deref(r) {
                    Value::Tuple(vs) => vs.iter().rev().for_each(|v| stack.push(v.clone())),
                    _ => panic!(),
                }
                pc += 1;
            }
            Code::Fn(cs, sc, bz) => {
                let closure = cs
                    .iter()
                    .map(|i| frames.last().unwrap()[*i as usize].clone())
                    .collect();
                let v = Value::Fn(closure, *sc as usize, pc + 1);
                stack.push(Value::Ref(Rc::new(v)));
                pc += 1 + *bz as usize;
            }
            Code::Call => {
                let arg = stack.pop().unwrap();
                let r = stack.pop().unwrap();
                match val_deref(&r) {
                    Value::Fn(cs, sc, loc) => {
                        let mut frame = init_frame(*sc);
                        frame[0] = r.clone();
                        (0..cs.len())
                            .into_iter()
                            .for_each(|i| frame[i + 1] = cs[i].clone());
                        frames.push(frame);
                        jmplist.push(pc + 1);
                        stack.push(arg);
                        pc = *loc;
                    }
                    Value::Builtin(i) => {
                        let f = builtins[*i];
                        stack.push(f(arg));
                        pc += 1
                    }
                    _ => panic!(),
                }
            }
            Code::TailCall => {
                let arg = stack.pop().unwrap();
                let r = stack.pop().unwrap();
                match val_deref(&r) {
                    Value::Fn(cs, sc, loc) => {
                        let frame = frames.last_mut().unwrap();
                        frame.resize(*sc, Value::Nil);
                        frame[0] = r.clone();
                        (0..cs.len())
                            .into_iter()
                            .for_each(|i| frame[i + 1] = cs[i].clone());
                        stack.push(arg);
                        pc = *loc;
                    }
                    Value::Builtin(i) => {
                        let f = builtins[*i];
                        stack.push(f(arg));
                        pc += 1
                    }
                    _ => panic!(),
                }
            }
            Code::Bind(u) => {
                let v = stack.pop().unwrap();
                frames.last_mut().unwrap()[*u as usize] = v;
                pc += 1;
            }
            Code::Val(u) => {
                stack.push(frames.last().unwrap()[*u as usize].clone());
                pc += 1;
            }
            Code::If => {
                let v = val_to_i64(stack.pop().unwrap());
                if v == 1 {
                    pc += 2;
                } else {
                    pc += 1;
                }
            }
            Code::Jmp(i) => {
                if *i > 0 {
                    pc += *i as usize;
                } else {
                    pc -= (-*i) as usize;
                }
            }
            Code::JmpBack => {
                let i = jmplist.pop().unwrap();
                pc = i
            }
        }
    }
}
