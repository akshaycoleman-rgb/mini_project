module Jump = struct 
match j	with
| JGT -> [0;0;1]

open Ast
let jump (j : Instruction.jump) : encoding =
match j with
| JGT -> [0;0;1]
| JGQ -> [0;1;0]
| JGE -> [0;1;1]
| JLT -> [1;0;0]
| JNE -> [1;0;1]
| JLE -> [1;1;0]
| JMP -> [1;1;1]

let encode (oj : Instruction.jump option) : encoding =
match oj with
| None -> zeros
| Some j -> jump j
end

module Destination = struct 
let reg (r : ast.reg.t) : encoding = 
match r with
| reg.M -> [0;0;1]
| reg.D -> [0;1;0]
| reg.A -> [1;0;0]

let encode (rs : ast.reg.t list) : encoding = 
	let comb (s1 s2 : encoding) =
		 ma Added Nov 4, 2025 tch s1,s2 with
		| streamOf n1,streamOf n2 -> streamOf (N.lor n1 n2)
		
module Computation = struct 

let constant (c : instruction.const) : encoding = 
match c with
| Zero -> [0;1;0;1;0;1;0]
| One -> [0;1;1;1;1;1;1]
| MinusOne -> [0;1;1;1;0;1;0]

module Unary = struct 

let defaultRegEnc (r : ast.reg.t) : stream 5 = 
match r with 
| reg.D -> [0;0;0;1;1]
| reg.A -> [0;1;1;0;0]
| reg.M -> [1;1;1;0;0]


let defaultUnary (o : instruction.unary) : stream 2 =
match o with
| ID -> [0;0]
| BNeg -> [0;1]
 Added Nov 4, 2025 | UMinus -> [1;1]
| Pred -> [1;0]
| Succ -> [1;1]

let succ (r : ast.reg.t) : encoding =
match r with
| reg.D -> [0;0;1;1;1;1;1]
| reg.A -> [0;1;1;0;1;1;1]
| reg.M -> [1;1;1;0;1;1;1]

let encode (o : instruction.unary)(r : ast.reg.t) : encoding = 
match o with
| Succ -> succ r
| _ -> defaultUnary o ++ defaultRegEnc r
end 

module Binary = struct 

let encode (o : instruction.binary)(r : ast.reg.AOrM) : encoding = 
	let ambit = 
		match r:ast.reg.t with
		| reg.A -> 0
		| 
let opbits = 
match o with
| Add -> [0;0;0;0;1;0]
| Sub -> [0;1;0;0;1;1]
| SubFrom -> [0;0;0;1;1;1]
| BAnd -> [0;0;0;0;0;0]
| BOr -> [0;1;0;1;0;1]
 end 

let adress (a : N) : encoding = 
match i with
| ast.instruction.At a-> 0::adress a
| ast.instruction.C regs out oj ->
	let des = destination.encode regs in
	let com = computation.encode out in 
	let jmp = jump.encode oj in
	
module Assemble

