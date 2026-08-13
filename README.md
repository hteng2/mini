# Mini

This is the repository for the Mini language, containing the compiler/interpreter, runtime, and documentation.

## What is Mini

Mini is a small functional language dedicated to clarity, correctness, and performance. Featuring a simple unambiguous grammar, strong type system, and a growing code optimization process.

The Mini compiler (minic) outputs its own bytecode runnable via the Mini runtime, but also serves as an interpreter.

## Building

**minic**

```bash
cd minic; dune build
```

**minir**

```bash
cd minir; cargo build
```
