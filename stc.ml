open Printf
open Num
open Types
open Stack
open Util

let rec command_str x = match x with
    | Oper x -> oper_str x
    | Stk_elem x -> stack_elem_str x
and oper_str x = match x with
    | Oper_plus -> "+"
    | _ -> "?"
and stack_elem_str x = match x with
    | Stk_num n -> string_of_num n
    | Stk_uneval u -> let cmd_strs = List.map (command_str) u in
        let s = String.concat ", " cmd_strs in sprintf "<%s>" s

    (*
let top_stack_str stk = let top = try Some (Stack.top stk) with
    | Empty -> None in match top with
    | None -> ""
    | Some x -> stack_elem_str x
    *)


    (*
let print_prompt stk = let top = top_stack_str stk in
    Printf.printf "%d: %s -- " (Stack.length stk) top
*)

let eval_oper stk o = match o with
    | Oper_plus -> Ops.op_eval_simple Num.add_num stk
    | Oper_minus -> Ops.op_eval_simple Num.sub_num stk
    | Oper_mult -> Ops.op_eval_simple Num.mult_num stk
    | Oper_div -> Ops.op_eval_simple Num.div_num stk
    | _ -> printf "not implemented"

let eval_command stk c = match c with
    | Stk_elem x -> push x stk
    | Oper x -> eval_oper stk x

let eval_command_list stk l =
    List.iter (eval_command stk) l

let read_loop () =
    let stk = Stack.create() in
    try
        let lexbuf = Lexing.from_channel stdin in
        while true do
            printf ("%d> ") (Stack.length stk); flush stdout;
            let l = Parser.input Lexer.token lexbuf in
            (try eval_command_list stk l with
                | Stack_underflow -> printf "stack underflow\n"
                | Parse_error -> printf "parse error\n"
            );
            dump_stack stk
        done
    with End_of_file -> exit 0;;
      
(* ---[ MAIN ] --- *)
(* read_loop (optab ()) *)
read_loop ()
