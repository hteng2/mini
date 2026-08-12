pub enum Optype {
    INeg,
    IAdd,
    ISub,
    IMul,
    IDiv,
    IMod,
    IEq,
    INeq,
    IGt,
    IGe,
    ILt,
    ILe,

    FNeg,
    FAdd,
    FSub,
    FMul,
    FDiv,
    FGt,
    FLt,

    Not,
    And,
    Or,
    Xor,
}

pub enum Code {
    Pushi(i64),
    Pushf(f64),
    Pushc(u8),
    Pop,
    Op(Optype),
    List(u64),
    At,
    Tuple(u64),
    Destruct,
    Fn(Vec<u64>, u64, u64),
    Call,
    TailCall,
    Bind(u64),
    Val(u64),
    If,
    Jmp(i64),
    JmpBack,
}

pub type Program = (u64, Vec<Code>);

fn bytes_to_u64(bytes: &[u8]) -> u64 {
    bytes
        .iter()
        .map(|&x| x as u64)
        .rfold(0, |acc, x| (acc << 8) + x)
}

pub fn decode(buffer: &[u8]) -> Program {
    let sc = bytes_to_u64(&buffer[..8]);

    let mut code = Vec::new();
    let mut i = 8;
    while i < buffer.len() {
        if i + 1 >= buffer.len() {
            panic!()
        }

        let opc = &buffer[i];
        let opt = &buffer[i + 1];
        i += 2;
        match (*opc, *opt) {
            // push/pop
            (0, 0) => {
                let n = bytes_to_u64(&buffer[i..i + 8]);
                let n = i64::from_le_bytes(n.to_le_bytes());
                code.push(Code::Pushi(n));
                i += 8;
            }
            (0, 1) => {
                let f = bytes_to_u64(&buffer[i..i + 8]);
                let f = f64::from_le_bytes(f.to_le_bytes());
                code.push(Code::Pushf(f));
                i += 8;
            }

            (1, n) => {
                code.push(Code::Pushc(n));
            }

            (2, 0) => {
                code.push(Code::Pop);
            }

            // int / float / char / bool ops
            // (char and bool comparisons share subs with the int ones)
            (3, n) => {
                let op = match n {
                    0 => Optype::INeg,
                    1 => Optype::IAdd,
                    2 => Optype::ISub,
                    3 => Optype::IMul,
                    4 => Optype::IDiv,
                    5 => Optype::IMod,
                    8 => Optype::IEq,
                    9 => Optype::INeq,
                    10 => Optype::ILt,
                    11 => Optype::ILe,
                    12 => Optype::IGt,
                    13 => Optype::IGe,
                    16 => Optype::FNeg,
                    17 => Optype::FAdd,
                    18 => Optype::FSub,
                    19 => Optype::FMul,
                    20 => Optype::FDiv,
                    26 => Optype::FGt,
                    28 => Optype::FLt,
                    32 => Optype::Not,
                    33 => Optype::And,
                    34 => Optype::Or,
                    35 => Optype::Xor,
                    _ => panic!(),
                };
                code.push(Code::Op(op));
            }

            // list / tuple
            (4, 0) => {
                let n = bytes_to_u64(&buffer[i..i + 8]);
                code.push(Code::List(n));
                i += 8;
            }
            (5, 0) => {
                code.push(Code::At);
            }
            (6, 0) => {
                let n = bytes_to_u64(&buffer[i..i + 8]);
                code.push(Code::Tuple(n));
                i += 8;
            }
            (7, 0) => {
                code.push(Code::Destruct);
            }

            // fns
            (8, 0) => {
                let ncs = bytes_to_u64(&buffer[i..i + 8]);
                i += 8;
                let mut cs = Vec::with_capacity(ncs as usize);
                for _ in 0..ncs {
                    cs.push(bytes_to_u64(&buffer[i..i + 8]));
                    i += 8;
                }
                let sc = bytes_to_u64(&buffer[i..i + 8]);
                i += 8;
                let bz = bytes_to_u64(&buffer[i..i + 8]);
                i += 8;
                code.push(Code::Fn(cs, sc, bz));
            }
            (9, 0) => {
                code.push(Code::Call);
            }
            (9, 1) => {
                code.push(Code::TailCall);
            }

            // bind
            (10, 0) => {
                let n = bytes_to_u64(&buffer[i..i + 8]);
                code.push(Code::Bind(n));
                i += 8;
            }
            (10, 1) => {
                let n = bytes_to_u64(&buffer[i..i + 8]);
                code.push(Code::Val(n));
                i += 8;
            }

            // control flow
            (11, 0) => {
                code.push(Code::If);
            }
            (12, 0) => {
                let n = bytes_to_u64(&buffer[i..i + 8]);
                let n = i64::from_le_bytes(n.to_le_bytes());
                code.push(Code::Jmp(n));
                i += 8;
            }
            (13, 0) => {
                code.push(Code::JmpBack);
            }

            _ => {
                panic!()
            }
        }
    }
    (sc, code)
}
