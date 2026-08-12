use std::env::args;
use std::fs::File;
use std::io::Read;

mod decoder;
mod runtime;

fn print_usage() {
    println!("Usage: minir <file.mbc>");
}

fn main() -> std::io::Result<()> {
    if args().count() != 2 {
        print_usage();
        return Ok(());
    }

    let filename = args().nth(1).unwrap();
    let mut file = File::open(&filename)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    for byte in buffer.iter() {
        print!("{:02x} ", byte);
    }
    println!();

    let program = decoder::decode(&buffer);
    println!("sc: {}", program.0);

    let mut i = 0;
    for bc in &(program.1) {
        let s = match bc {
            decoder::Code::Pushi(i) => format!("pushi {}", &i),
            decoder::Code::Pushf(f) => format!("pushf {}", &f),
            decoder::Code::Pushc(u) => format!("pushc {}", &u),
            decoder::Code::Pop => format!("pop"),

            decoder::Code::Op(op) => match &op {
                decoder::Optype::INeg => format!("op ineg"),
                decoder::Optype::IAdd => format!("op iadd"),
                decoder::Optype::ISub => format!("op isub"),
                decoder::Optype::IMul => format!("op imul"),
                decoder::Optype::IDiv => format!("op idiv"),
                decoder::Optype::IMod => format!("op imod"),
                decoder::Optype::IEq => format!("op ieq"),
                decoder::Optype::INeq => format!("op ineq"),
                decoder::Optype::IGt => format!("op igt"),
                decoder::Optype::IGe => format!("op ige"),
                decoder::Optype::ILt => format!("op ilt"),
                decoder::Optype::ILe => format!("op ile"),

                decoder::Optype::FNeg => format!("op fneg"),
                decoder::Optype::FAdd => format!("op fadd"),
                decoder::Optype::FSub => format!("op fsub"),
                decoder::Optype::FMul => format!("op fmul"),
                decoder::Optype::FDiv => format!("op fdiv"),
                decoder::Optype::FGt => format!("op fgt"),
                decoder::Optype::FLt => format!("op flt"),

                decoder::Optype::Not => format!("op not"),
                decoder::Optype::And => format!("op and"),
                decoder::Optype::Or => format!("op or"),
                decoder::Optype::Xor => format!("op xor"),
            },

            decoder::Code::List(n) => format!("list {}", &n),
            decoder::Code::At => format!("at"),
            decoder::Code::Tuple(n) => format!("tuple {}", &n),
            decoder::Code::Destruct => format!("destruct"),

            decoder::Code::Fn(cs, sc, bz) => {
                format!("fn closure={:?} symcnt={} size={}", &cs, &sc, &bz)
            }
            decoder::Code::Call => format!("call"),
            decoder::Code::TailCall => format!("tailcall"),

            decoder::Code::Bind(n) => format!("bind {}", n),
            decoder::Code::Val(n) => format!("val {}", n),

            decoder::Code::If => format!("if"),
            decoder::Code::Jmp(n) => format!("jmp {}", n),
            decoder::Code::JmpBack => format!("jmpbck"),
        };
        println!("{:-5} {}", i, s);
        i += 1;
    }

    runtime::run(program);

    Ok(())
}
