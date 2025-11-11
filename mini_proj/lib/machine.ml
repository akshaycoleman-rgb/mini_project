let encode_dest (d : dest) =
  let a = if List.mem A d then '1' else '0' in
  let d = if List.mem D d then '1' else '0' in
  let m = if List.mem M d then '1' else '0' in
  String.make 1 a ^ String.make 1 d ^ String.make 1 m


let encode_jump = function
  | NullJump -> "000"
  | JGT -> "001"
  | JEQ -> "010"
  | JGE -> "011"
  | JLT -> "100"
  | JNE -> "101"
  | JLE -> "110"
  | JMP -> "111"


let encode_const = function
  | Zero -> "0101010"
  | One -> "0111111"
  | MinusOne -> "0111010"

let encode_unary (op, r) =
  match op, r with
  | Identity, D -> "0001100"
  | Identity, A -> "0110000"
  | Identity, M -> "1110000"

  | Neg, D -> "0001111"
  | Neg, A -> "0110011"
  | Neg, M -> "1110011"

  | BNot, D -> "0001101"
  | BNot, A -> "0110001"
  | BNot, M -> "1110001"

  | Succ, D -> "0011111"
  | Succ, A -> "0110111"
  | Succ, M -> "1110111"

  | Pred, D -> "0001110"
  | Pred, A -> "0110010"
  | Pred, M -> "1110010"

  | _ -> failwith "Unsupported unary comp"

let encode_binary = function
  | Add, D -> "0000010"   (* D + register → but your AST only carries one register *)
  | Sub, D -> "0010011"   (* D - register *)
  | SubFrom, D -> "0000111" (* register - D *)
  | BAnd, D -> "0000000"
  | BOr, D -> "0010101"
  | _ -> failwith "Unsupported binary comp"

let encode_comp = function
  | Const c -> encode_const c
  | UnaryOp u -> encode_unary u
  | BinaryOp b -> encode_binary b


let encode_A n =
  let bits = Printf.sprintf "%015b" n in
  "0" ^ bits

let encode_C d c j =
  "111" ^ encode_comp c ^ encode_dest d ^ encode_jump j


let encode_inst (tbl : 'v table) (inst : 'v inst) : string =
  match inst with
  | Ldef _ ->
      ""      (* labels can't emit code *)

  | At n ->
      encode_A n

  | Ainst sym ->
      let addr = Hashtbl.find tbl sym in
      encode_A addr

  | Cinst (d, c, j) ->
      encode_C d c j
