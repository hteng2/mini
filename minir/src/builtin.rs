use std::{io, ops::Deref, panic, rc::Rc};

use crate::value::*;

pub enum EntryValue {
    Call(Value),
    ListReentry(Value, Vec<Value>, Value, usize),
}

pub enum ExitValue {
    ListCallback(Value, Vec<Value>, Value, usize),
    Done(Value),
}

pub fn builtin_print(e: EntryValue) -> ExitValue {
    if let EntryValue::Call(Value::Ref(i)) = e
        && let Value::List(s) = Rc::deref(&i)
    {
        s.iter()
            .map(|v| match v {
                Value::Int(n) => *n as u8 as char,
                _ => panic!(),
            })
            .for_each(|c| print!("{}", c))
    }
    ExitValue::Done(Value::Int(0))
}

pub fn builtin_println(e: EntryValue) -> ExitValue {
    if let EntryValue::Call(Value::Ref(i)) = e
        && let Value::List(s) = Rc::deref(&i)
    {
        s.iter()
            .map(|v| match v {
                Value::Int(n) => *n as u8 as char,
                _ => panic!(),
            })
            .for_each(|c| print!("{}", c))
    }
    println!();
    ExitValue::Done(Value::Int(0))
}

pub fn builtin_readline(_: EntryValue) -> ExitValue {
    let s = match io::read_to_string(io::stdin()) {
        Err(_) => vec![],
        Ok(v) => v.bytes().map(|b| Value::Int(b as i64)).collect(),
    };

    let res = Value::List(s);

    ExitValue::Done(Value::Ref(Rc::new(res)))
}

pub fn builtin_itoa(e: EntryValue) -> ExitValue {
    let EntryValue::Call(Value::Int(i)) = e else {
        panic!()
    };

    let res = str_to_val(&i.to_string());

    ExitValue::Done(Value::Ref(Rc::new(res)))
}

pub fn builtin_ftoa(e: EntryValue) -> ExitValue {
    let EntryValue::Call(Value::Float(f)) = e else {
        panic!()
    };

    let res = str_to_val(&f.to_string());
    ExitValue::Done(Value::Ref(Rc::new(res)))
}

pub fn builtin_ftoi(e: EntryValue) -> ExitValue {
    let EntryValue::Call(Value::Float(f)) = e else {
        panic!()
    };

    ExitValue::Done(Value::Int(f as i64))
}

pub fn builtin_atoi(e: EntryValue) -> ExitValue {
    let i = if let EntryValue::Call(s) = e
        && let Value::List(_) = s
    {
        let s = val_to_str(s);
        str::parse::<i64>(&s).unwrap_or(0i64)
    } else {
        panic!()
    };

    ExitValue::Done(Value::Int(i))
}

pub fn builtin_itof(e: EntryValue) -> ExitValue {
    let EntryValue::Call(Value::Int(i)) = e else {
        panic!()
    };

    ExitValue::Done(Value::Float(i as f64))
}

pub fn builtin_rand(_: EntryValue) -> ExitValue {
    ExitValue::Done(Value::Float(rand::random::<f64>()))
}

pub fn builtin_list(status: EntryValue) -> ExitValue {
    if let EntryValue::Call(Value::Ref(i)) = status {
        if let Value::Tuple(vs) = Rc::deref(&i)
            && let Value::Int(i) = vs[1]
        {
            ExitValue::ListCallback(
                Value::Int(0),
                Vec::with_capacity(i as usize),
                vs[0].clone(),
                i as usize,
            )
        } else {
            panic!()
        }
    } else if let EntryValue::ListReentry(v, mut acc, f, len) = status {
        acc.push(v);
        if acc.len() == len {
            ExitValue::Done(Value::Ref(Rc::new(Value::List(acc))))
        } else {
            ExitValue::ListCallback(Value::Int(acc.len() as i64), acc, f, len)
        }
    } else {
        panic!()
    }
}
