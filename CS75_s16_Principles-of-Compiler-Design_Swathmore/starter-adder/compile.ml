open Printf
open List

(* Abstract syntax of (a small subset of) x86 assembly instructions *)

type reg =
	| EAX
	| ESP

type arg =
  | Const of int
  | Reg of reg
  | RegOffset of int * reg

type instruction =
	| IMov of arg * arg
  | IAdd of arg * arg
	| IRet


(* Abstract syntax of the Adder language *)

type prim1 =
  | Add1
  | Sub1

type expr =
	| Number of int
  | Prim1 of prim1 * expr
  | Let of (string * expr) list * expr
  | Id of string


(* Functions that implement the compiler *)

let reg_to_asm_string (r : reg) : string =
	match r with
		| EAX -> "eax"
		| ESP -> "esp"

let arg_to_asm_string (a : arg) : string =
  match a with
    | Const(n) -> sprintf "%d" n
    | Reg(r) -> reg_to_asm_string r
    | RegOffset(n, r) ->
           "[" ^ reg_to_asm_string r ^ (if n > 0 then "+" else "-") ^ string_of_int (abs n) ^ "]"

let instruction_to_asm_string (i : instruction) : string =
	match i with
		| IMov(dest, value) ->
			sprintf "	mov %s, %s" (arg_to_asm_string dest) (arg_to_asm_string value)
		| IRet ->
			"	ret"
		| IAdd(dest, to_add) ->
      sprintf "	add %s, %s" (arg_to_asm_string dest) (arg_to_asm_string to_add)

(** instructions to string*)
let to_asm_string (is : instruction list) : string =
  List.fold_left (fun s i -> sprintf "%s\n%s" s (instruction_to_asm_string i)) "" is

let rec find (ls : (string * int) list) (x : string) =
  match ls with
    | [] -> None
    | (y,v)::rest ->
      if y = x then Some(v) else find rest x

(** remove the duplicates in binding list*)
let rec removeDup  (l :  (string * expr) list) = 
   match l with
   | [] -> []
   | (id, expr) :: rest -> 
                      (id, expr) :: removeDup (List.filter (fun elem -> (fst elem) <> id) rest) 
    

let has_unique_key l =
  (*ignore (Printf.printf "%d\n" (length l)); *)
  length (removeDup l) = length l   


let rec compile_env
    (p : expr)
    (stack_index : int)
    (env : (string * int) list)
  : instruction list =
	match p with
		| Number(n) ->
			[
				IMov(Reg(EAX), Const(n))
			] 
    | Let(binds, body) ->
       let rec helper xs si env =
           match xs with
           | []  -> compile_env body (si + 1) env
           | (id, expr) :: rest -> 
             let new_env = (id, (-4 * si)) :: env in
                (compile_env expr si env) @ [IMov(RegOffset((-4) * si, ESP), Reg(EAX))] @ helper rest (si + 1) new_env
           in 
           if has_unique_key binds then 
               helper binds stack_index env
           else 
              failwith "there is a binding list containing two or more bindings with the same name."

    | Id(x) ->
      [
        match (find env x) with
       | None -> failwith "not found id"
       | Some offset -> IMov(Reg(EAX), RegOffset(offset, ESP))
      ]

    | Prim1(op, e) -> 
      (compile_env e stack_index env)
       @
      (let num = (if op = Add1 then 1 else -1) in
      [
        IAdd(Reg(EAX), Const(num));
      ])
      
  

let compile (p : expr) : instruction list =
  compile_env p 1 []

(* The entry point for the compiler: a function that takes a expr and
creates an assembly-program string representing the compiled version *)

let compile_to_string (prog : expr) =
  let prelude = 
"section .text
global our_code_starts_here
our_code_starts_here:" in
  let asm_string = (to_asm_string ((compile prog) @ [IRet])) in
	let asm_program = sprintf "%s%s\n" prelude asm_string in
  asm_program

