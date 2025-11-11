type address = int
type register = A | M | D
type dest = register list
type const = Zero | One | MinusOne
type unary_op = Identity | BNot | Neg | Succ | Pred
type binary_op = Add | Sub | SubFrom | BAnd | BOr
type comp =
  | Const of const
  | UnaryOp of unary_op * register           (* e.g. -D, !A *)
  | BinaryOp of binary_op * register * register  (* e.g. D + A, M & D *)
type jump = JEQ | JNE | JLE | JLT | JGE | JGT | JMP | NullJump

type 'v inst =
  | Ldef of 'v
  | Aaddr of address
  | Asym of 'v
  | Cinst of dest * comp * jump

type 'v table = ('v, address) Hashtbl.t
type 'v program = 'v inst list
type avar = Symbol of string


let rec get_labels (prog : 'v inst list) (symbol_table : 'v table) (curr_address : int) =
  match prog with
  | [] -> ()
  | Ldef label_name :: tail ->
      Hashtbl.add symbol_table label_name curr_address;
      get_labels tail symbol_table curr_address
  | _ :: tail ->
      get_labels tail symbol_table (curr_address + 1)


let resolve_symbol (symbol : 'v) (symbol_table : 'v table) (curr_address : address) : address * address =
  if Hashtbl.mem symbol_table symbol then
    (Hashtbl.find symbol_table symbol, curr_address)
  else
    ( Hashtbl.add symbol_table symbol curr_address;
      (curr_address, curr_address + 1) )


module Helper = struct
  let aaddr n = Aaddr n
  let asym s = Asym s
  let ldef s = Ldef s

  let a = [A]
  let d = [D]
  let m = [M]
  let am = [A; M]
  let md = [M; D]

  let areg = A
  let dreg = D
  let mreg = M

  let one = Const One
  let zero = Const Zero
  let minusone = Const MinusOne

  let jeq = JEQ
  let jlt = JLT
  let jgt = JGT
  let jne = JNE
  let jmp = JMP
  let nulljump = NullJump

  let iden r = UnaryOp (Identity, r)
  let bnot r = UnaryOp (BNot, r)
  let neg r = UnaryOp (Neg, r)
  let succ r = UnaryOp (Succ, r)
  let pred r = UnaryOp (Pred, r)

  (* Binary: specify both operands explicitly *)
  let bin op r1 r2 = BinaryOp (op, r1, r2)

  let add r1 r2 = BinaryOp (Add, r1, r2)
  let sub r1 r2 = BinaryOp (Sub, r1, r2)
  let subfrom r1 r2 = BinaryOp (SubFrom, r1, r2)
  let band r1 r2 = BinaryOp (BAnd, r1, r2)
  let bor  r1 r2 = BinaryOp (BOr,  r1, r2)

  let assign regs expr = Cinst (regs, expr, NullJump)
  let cond_jump expr jmp = Cinst ([], expr, jmp)
  let uncond_jump = Cinst ([], Const Zero, JMP)
end
