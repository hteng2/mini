# Bytecode specification

## Data

Int -> int
Bool -> bool
Char -> char
Str -> pass by ptr, char array on heap
Void -> _
List -> pass by ptr, array on heap
Fn -> pass by ptr, fn on heap

array -> len data, data
fn -> len closures, closures, len params, body

## Moving data

**Push [x:64]**

stack -> x :: stack

**Alloc**

n :: stack -> i :: stack

finds an index i on the heap with n free slots

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
