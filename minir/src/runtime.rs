use crate::builtin::*;
use crate::decoder::{Code, Optype, Program};
use crate::value::*;
use std::panic;
use std::{fmt, rc::Rc, vec};

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

fn eval_op(op: &Optype, stack: &mut Vec<Value>) -> Value {
    match op {
        Optype::INeg => {
            let i = val_to_i64(stack.pop().unwrap());
            Value::Int(-i)
        }
        Optype::IAdd => {
            let b = val_to_i64(stack.pop().unwrap());
            let a = val_to_i64(stack.pop().unwrap());
            Value::Int(a + b)
        }
        Optype::ISub => {
            let b = val_to_i64(stack.pop().unwrap());
            let a = val_to_i64(stack.pop().unwrap());
            Value::Int(a - b)
        }
        Optype::IMul => {
            let b = val_to_i64(stack.pop().unwrap());
            let a = val_to_i64(stack.pop().unwrap());
            Value::Int(a * b)
        }
        Optype::IDiv => {
            let b = val_to_i64(stack.pop().unwrap());
            let a = val_to_i64(stack.pop().unwrap());
            Value::Int(a / b)
        }
        Optype::IMod => {
            let b = val_to_i64(stack.pop().unwrap());
            let a = val_to_i64(stack.pop().unwrap());
            Value::Int(a % b)
        }
        Optype::IEq => {
            let b = val_to_i64(stack.pop().unwrap());
            let a = val_to_i64(stack.pop().unwrap());
            Value::Int(if a == b { 1 } else { 0 })
        }
        Optype::INeq => {
            let b = val_to_i64(stack.pop().unwrap());
            let a = val_to_i64(stack.pop().unwrap());
            Value::Int(if a != b { 1 } else { 0 })
        }
        Optype::IGt => {
            let b = val_to_i64(stack.pop().unwrap());
            let a = val_to_i64(stack.pop().unwrap());
            Value::Int(if a > b { 1 } else { 0 })
        }
        Optype::IGe => {
            let b = val_to_i64(stack.pop().unwrap());
            let a = val_to_i64(stack.pop().unwrap());
            Value::Int(if a >= b { 1 } else { 0 })
        }
        Optype::ILt => {
            let b = val_to_i64(stack.pop().unwrap());
            let a = val_to_i64(stack.pop().unwrap());
            Value::Int(if a < b { 1 } else { 0 })
        }
        Optype::ILe => {
            let b = val_to_i64(stack.pop().unwrap());
            let a = val_to_i64(stack.pop().unwrap());
            Value::Int(if a <= b { 1 } else { 0 })
        }
        Optype::FNeg => {
            let f = val_to_f64(stack.pop().unwrap());
            Value::Float(-f)
        }
        Optype::FAdd => {
            let b = val_to_f64(stack.pop().unwrap());
            let a = val_to_f64(stack.pop().unwrap());
            Value::Float(a + b)
        }
        Optype::FSub => {
            let b = val_to_f64(stack.pop().unwrap());
            let a = val_to_f64(stack.pop().unwrap());
            Value::Float(a - b)
        }
        Optype::FMul => {
            let b = val_to_f64(stack.pop().unwrap());
            let a = val_to_f64(stack.pop().unwrap());
            Value::Float(a * b)
        }
        Optype::FDiv => {
            let b = val_to_f64(stack.pop().unwrap());
            let a = val_to_f64(stack.pop().unwrap());
            Value::Float(a / b)
        }
        Optype::FGt => {
            let b = val_to_f64(stack.pop().unwrap());
            let a = val_to_f64(stack.pop().unwrap());
            Value::Int(if a > b { 1 } else { 0 })
        }
        Optype::FLt => {
            let b = val_to_f64(stack.pop().unwrap());
            let a = val_to_f64(stack.pop().unwrap());
            Value::Int(if a < b { 1 } else { 0 })
        }
        Optype::Not => {
            let b = val_to_i64(stack.pop().unwrap());
            Value::Int(if b == 0 { 1 } else { 0 })
        }
        Optype::And => {
            let b = val_to_i64(stack.pop().unwrap());
            let a = val_to_i64(stack.pop().unwrap());
            Value::Int(if a != 0 && b != 0 { 1 } else { 0 })
        }
        Optype::Or => {
            let b = val_to_i64(stack.pop().unwrap());
            let a = val_to_i64(stack.pop().unwrap());
            Value::Int(if a != 0 || b != 0 { 1 } else { 0 })
        }
        Optype::Xor => {
            let b = val_to_i64(stack.pop().unwrap());
            let a = val_to_i64(stack.pop().unwrap());
            Value::Int(if (a != 0) ^ (b != 0) { 1 } else { 0 })
        }
    }
}

fn exec_builtin(
    p: &Program,
    builtins: &[fn(EntryValue) -> ExitValue],
    stack: &mut Vec<Value>,
    i: usize,
    arg: Value,
) {
    let f = builtins[i];
    let mut ev = f(EntryValue::Call(arg));
    while let ExitValue::ListCallback(a, acc, g, len) = ev {
        let r = g.clone();
        let arg = a;
        let Value::Fn(cs, sc, loc) = val_deref(&r) else {
            panic!()
        };

        let mut frame = init_frame(*sc);
        frame[0] = r.clone();
        (0..cs.len())
            .into_iter()
            .for_each(|i| frame[i + 1] = cs[i].clone());
        let val = exec(p, builtins, vec![frame], vec![arg], vec![], *loc);
        ev = f(EntryValue::ListReentry(val, acc, g, len));
    }
    let ExitValue::Done(v) = ev else { panic!() };
    stack.push(v)
}

fn exec(
    p: &Program,
    builtins: &[fn(EntryValue) -> ExitValue],
    mut frames: Vec<Vec<Value>>,
    mut stack: Vec<Value>,
    mut jmplist: Vec<usize>,
    mut pc: usize,
) -> Value {
    let (_, bcs) = p;
    loop {
        match &bcs[pc] {
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
                let v = eval_op(op, &mut stack);
                stack.push(v);
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
                let r = stack.pop().unwrap();
                let Value::List(list) = val_deref(&r) else {
                    panic!()
                };
                stack.push(list[i].clone());
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
                let r = stack.pop().unwrap();
                let Value::Tuple(vs) = val_deref(&r) else {
                    panic!()
                };
                vs.iter().rev().for_each(|v| stack.push(v.clone()));
                pc += 1
            }
            Code::Fn(cs, sc, bz) => {
                let closure = cs
                    .iter()
                    .map(|i| frames.last().unwrap()[*i as usize].clone())
                    .collect();
                let v = Value::Fn(closure, *sc as usize, pc + 1);
                stack.push(Value::Ref(Rc::new(v)));
                pc += 1 + *bz as usize
            }
            Code::Call => {
                let arg = stack.pop().unwrap();
                let r = stack.pop().unwrap();
                let v = val_deref(&r);
                if let Value::Fn(cs, sc, loc) = v {
                    let mut frame = init_frame(*sc);
                    frame[0] = r.clone();
                    (0..cs.len())
                        .into_iter()
                        .for_each(|i| frame[i + 1] = cs[i].clone());
                    frames.push(frame);
                    jmplist.push(pc + 1);
                    stack.push(arg);
                    pc = *loc
                } else {
                    let Value::Builtin(i) = v else { panic!() };
                    exec_builtin(p, builtins, &mut stack, *i, arg);
                    pc += 1
                }
            }
            Code::TailCall => {
                let arg = stack.pop().unwrap();
                let r = stack.pop().unwrap();
                let v = val_deref(&r);
                if let Value::Fn(cs, sc, loc) = v {
                    let frame = frames.last_mut().unwrap();
                    frame.resize(*sc, Value::Nil);
                    frame[0] = r.clone();
                    (0..cs.len())
                        .into_iter()
                        .for_each(|i| frame[i + 1] = cs[i].clone());
                    stack.push(arg);
                    pc = *loc
                } else {
                    let Value::Builtin(i) = v else { panic!() };
                    exec_builtin(p, builtins, &mut stack, *i, arg);
                    pc += 1
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
                if v != 1 && v != 0 {
                    panic!()
                };
                pc += 1 + v as usize
            }
            Code::Jmp(i) => {
                if *i > 0 {
                    pc += *i as usize;
                } else {
                    pc -= (-*i) as usize;
                }
            }
            Code::JmpBack => match jmplist.pop() {
                Some(i) => pc = i,
                None => break,
            },
        }
    }
    stack.pop().unwrap()
}

pub fn run(p: Program) {
    let mut frames: Vec<Vec<Value>> = vec![];
    let stack: Vec<Value> = vec![];

    let jmplist: Vec<usize> = vec![];

    let (sc, _) = &p;
    let mut frame = init_frame(*sc as usize);
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
        builtin_list,
    ];
    (0..builtins.len()).into_iter().for_each(|i| {
        let b = Value::Builtin(i);
        frame[i] = Value::Ref(Rc::new(b));
    });
    frames.push(frame);

    exec(&p, &builtins, frames, stack, jmplist, 0usize);
}
