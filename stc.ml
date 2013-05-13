open Printf
open Types
open Eval

let top_stack_str stk =
    let top = try Some (Stack.top stk) with
        | Stack.Empty -> None in match top with
            | None -> ""
            | Some x -> Util.stack_elem_str x

let print_prompt stk = let top = top_stack_str stk in
    printf "%d: %s -- " (Stack.length stk) top; flush stdout

let print_err x = match x with
    | Stack.Empty -> print_string "  stack underflow\n"
    | Type_error -> print_string "  type error\n"
    | No_reg_error -> print_string "  no such register\n"
    | _ -> print_string "unknown error\n"

let read_loop () =
    let regtab = Hashtbl.create 16 in
    let stk = ref (Stack.create ()) in
    try
        let lexbuf = Lexing.from_channel stdin in
        while true do
            print_prompt !stk;
            let stk_copy = Stack.copy !stk in
            let l = try Some (Parser.input Lexer.token lexbuf) with
                | Parsing.Parse_error -> Lexing.flush_input lexbuf; None in
            match l with
                | None -> ()
                | Some x -> try eval_command_list !stk regtab x with
                    | Stack.Empty
                    | No_reg_error
                    | Type_error as e -> print_err e; stk := stk_copy
        done
    with End_of_file -> print_string "\n"; exit 0;;
      
(* ---[ MAIN ] --- *)
read_loop ()
