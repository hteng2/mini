use std::{ops::Deref, rc::Rc};

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

pub fn str_to_val(s: &str) -> Value {
    let i = s.as_bytes().iter().map(|u| Value::Int(*u as i64)).collect();
    Value::List(i)
}

pub fn val_to_str(v: Value) -> String {
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

pub fn val_to_i64(v: Value) -> i64 {
    match v {
        Value::Int(i) => i,
        _ => panic!(),
    }
}

pub fn val_to_f64(v: Value) -> f64 {
    match v {
        Value::Float(f) => f,
        _ => panic!(),
    }
}

pub fn val_deref(v: &Value) -> &Value {
    match v {
        Value::Ref(r) => Rc::deref(r),
        _ => panic!(),
    }
}
