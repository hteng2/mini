open Minic_lib

(* little endian *)
let u8_to_bytes n =
  let mask = Int64.of_int 255 in
  List.init 8 (fun i ->
      Int64.shift_right n (i * 8) |> Int64.logand mask |> Int64.to_int)

let bc_to_ints (bc : Ir3.ir3) : int list =
  match bc with
  (* push/pop *)
  | Int i -> 0 :: 0 :: u8_to_bytes (Int64.of_int i)
  | Float f -> 0 :: 1 :: u8_to_bytes (Int64.bits_of_float f)
  | Char c -> [ 1; Char.code c ]
  | Bool b -> [ 1; (if b then 1 else 0) ]
  | Void -> [ 1; 0 ]
  | Pop -> [ 2; 0 ]
  (* int *)
  | INeg -> [ 3; 0 ]
  | IAdd -> [ 3; 1 ]
  | ISub -> [ 3; 2 ]
  | IMul -> [ 3; 3 ]
  | IDiv -> [ 3; 4 ]
  | IMod -> [ 3; 5 ]
  | IEq -> [ 3; 8 ]
  | INeq -> [ 3; 9 ]
  | ILt -> [ 3; 10 ]
  | ILe -> [ 3; 11 ]
  | IGt -> [ 3; 12 ]
  | IGe -> [ 3; 13 ]
  (* float *)
  | FNeg -> [ 3; 16 ]
  | FAdd -> [ 3; 17 ]
  | FSub -> [ 3; 18 ]
  | FMul -> [ 3; 19 ]
  | FDiv -> [ 3; 20 ]
  | FGt -> [ 3; 26 ]
  | FLt -> [ 3; 28 ]
  (* char *)
  | CEq -> [ 3; 8 ]
  | CNeq -> [ 3; 9 ]
  | CLt -> [ 3; 10 ]
  | CLe -> [ 3; 11 ]
  | CGt -> [ 3; 12 ]
  | CGe -> [ 3; 13 ]
  (* bool *)
  | Not -> [ 3; 32 ]
  | And -> [ 3; 33 ]
  | Or -> [ 3; 34 ]
  | Xor -> [ 3; 35 ]
  | BEq -> [ 3; 8 ]
  (* list *)
  | List i -> 4 :: 0 :: u8_to_bytes (Int64.of_int i)
  | At -> [ 5; 0 ]
  (* tuple *)
  | Tuple i -> 6 :: 0 :: u8_to_bytes (Int64.of_int i)
  | Destruct -> [ 7; 0 ]
  (* fns *)
  | FnVal (cs, sc, bz) ->
      8 :: 0
      :: List.concat
           [
             u8_to_bytes (Int64.of_int (Array.length cs));
             cs |> Array.map Int64.of_int |> Array.map u8_to_bytes
             |> Array.to_list |> List.concat;
             u8_to_bytes (Int64.of_int sc);
             u8_to_bytes (Int64.of_int bz);
           ]
  | FnCall -> [ 9; 0 ]
  | FnTailCall -> [ 9; 1 ]
  (* bind *)
  | Bind i -> 10 :: 0 :: u8_to_bytes (Int64.of_int i)
  | Name i -> 10 :: 1 :: u8_to_bytes (Int64.of_int i)
  (* control flow *)
  | If -> [ 11; 0 ]
  | Jmp i -> 12 :: 0 :: u8_to_bytes (Int64.of_int i)
  | JmpBck -> [ 13; 0 ]

let run out (bcs, sc) =
  let file = open_out_bin out in
  List.iter (fun i -> output_byte file i) (u8_to_bytes (Int64.of_int sc));
  Array.iter
    (fun bc -> List.iter (fun i -> output_byte file i) (bc_to_ints bc))
    bcs
