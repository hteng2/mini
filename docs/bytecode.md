# Bytecode specification

## Datatypes

0: void (0 bytes)
1: bool (1 byte)
2: char (1 byte)
3: int (8 bytes)
4: ptr (8 bytes)

    - array is int followed by data
    - str is a char array
    - fn is param array, closure array, self, body size

## Moving data

**Push [x:64]**

stack -> x :: stack

**Pop**

x :: stack -> stack

**Store**

x :: i :: stack -> stack

heap[i] = x

**Fetch**

i :: stack -> heap[i] :: stack

## Control flow

**Ifn**

x :: stack -> stack

pc += 1

**Jmp**

x :: stack -> stack

pc += x

**Goto**

x :: stack -> stack

pc = x

**Call [i]**

calls the ith builtin

depends on implementation

## Doing operations

**Neg**

x :: stack -> -x :: stack

**Add, Sub, Mul, Div, Mod**

x1 :: x0 :: stack -> x' :: stack

**Not**

b :: stack -> not b :: stack

**And, Or, Xor**

b1 :: b0 :: stack -> b' :: stack

**Eq, Gt, Lt**

x1 :: x0 :: stack -> c :: stack
