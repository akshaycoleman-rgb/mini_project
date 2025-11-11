

let trim s =
  let len = length s in
  let rec left i = if i < len && (s.[i] = ' ' || s.[i] = '\t') then left (i+1) else i in
  let rec right i = if i >= 0 && (s.[i] = ' ' || s.[i] = '\t') then right (i-1) else i in
  let l = left 0 in
  let r = right (len-1) in
  if r < l then "" else sub s l (r-l+1)
  
let split_char c s =
  match index_opt s c with
  | None -> (s, "")
  | Some i ->
      let left = sub s 0 i in
      let right = sub s (i+1) (length s - i - 1) in
      (left, right)

let parse_reg = function
  | "A" -> A
  | "D" -> D
  | "M" -> M
  
let parse_dest s =
  let rec chars i acc =
    if i = length s then acc
    else chars (i+1) (parse_reg (String.make 1 s.[i]) :: acc)
  in
  List.rev (chars 0 [])

let parse_comp s =
  match s with
  | "0" -> Const Zero
  | "1" -> Const One
  | "-1" -> Const MinusOne

  | "D" -> UnaryOp (Identity, D)
  | "A" -> UnaryOp (Identity, A)
  | "M" -> UnaryOp (Identity, M)

  | "!D" -> UnaryOp (BNot, D)
  | "!A" -> UnaryOp (BNot, A)
  | "!M" -> UnaryOp (BNot, M)

  | "-D" -> UnaryOp (Neg, D)
  | "-A" -> UnaryOp (Neg, A)
  | "-M" -> UnaryOp (Neg, M)

  | "D+1" -> UnaryOp (Succ, D)
  | "A+1" -> UnaryOp (Succ, A)
  | "M+1" -> UnaryOp (Succ, M)

  | "D-1" -> UnaryOp (Pred, D)
  | "A-1" -> UnaryOp (Pred, A)
  | "M-1" -> UnaryOp (Pred, M)

  | _ ->
      if starts_with ~prefix:"D+" s then
        let r = parse_reg (sub s 2 1) in BinaryOp (Add, r)
      else if starts_with ~prefix:"D-" s then
        let r = parse_reg (sub s 2 1) in BinaryOp (Sub, r)
      else if ends_with ~suffix:("-D") s && length s = 3 then
        let r = parse_reg (sub s 0 1) in BinaryOp (SubFrom, r)
      else if starts_with ~prefix:"D&" s then
        let r = parse_reg (sub s 2 1) in BinaryOp (BAnd, r)
      else if starts_with ~prefix:"D|" s then
        let r = parse_reg (sub s 2 1) in BinaryOp (BOr, r)
      else
        failwith ("Invalid comp: " ^ s)

let parse_jump = function
  | "" -> NullJump
  | "JGT" -> JGT
  | "JEQ" -> JEQ
  | "JGE" -> JGE
  | "JLT" -> JLT
  | "JNE" -> JNE
  | "JLE" -> JLE
  | "JMP" -> JMP
  | s -> failwith ("Invalid jump: " ^ s)

let parse_line line =
  let line =
    match index_opt line '/' with
    | Some i when i+1 < length line && line.[i+1] = '/' ->
        trim (sub line 0 i)
    | _ -> trim line
  in
  if line = "" then None else


  if starts_with ~prefix:"(" line && ends_with ~suffix:")" line then
    let name = sub line 1 (length line - 2) in
    Some (Ldef (Symbol name))

  
  else if starts_with ~prefix:"@" line then
    let v = sub line 1 (length line - 1) in
    (* numeric or symbol*)
    match int_of_string_opt v with
    | Some n -> Some (At n)
    | None -> Some (Ainst (Symbol v))

  else
    let (left, jmp) = split_char ';' line in
    let (dest_part, comp_part) = split_char '=' left in

    let (dest, comp) =
      if comp_part = "" then
        ([], parse_comp dest_part)
      else
        (parse_dest dest_part, parse_comp comp_part)
    in

    Some (Cinst (dest, comp, parse_jump jmp))

let parse_program lines =
  lines
  |> List.filter_map parse_line
