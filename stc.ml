open Printf
open Num
open Types
open Stack
open Util
open Eval

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

let top_stack_str stk =
    let top = try Some (Stack.top stk) with
        | Empty -> None in match top with
        | None -> ""
        | Some x -> stack_elem_str x

let print_prompt stk = let top = top_stack_str stk in
    Printf.printf "%d: %s -- " (Stack.length stk) top; flush stdout

let print_err x = match x with
    | Stack.Empty -> print_string "  stack underflow\n"
    | Parse_error -> print_string "  parse error\n"
    | Type_error -> print_string "  type error\n"
    | _ -> print_string "  unknown error\n"

let read_loop () =
    let stk = Stack.create() in
    try
        let lexbuf = Lexing.from_channel stdin in
        while true do
            print_prompt stk;
            let l = try Some (Parser.input Lexer.token lexbuf) with
                | Parsing.Parse_error -> None in
            match l with
                | None -> ()
                | Some x -> ( try eval_command_list stk x with
                    | e -> print_err e)
        done
    with End_of_file -> exit 0;;
      
(* ---[ MAIN ] --- *)
read_loop ()
