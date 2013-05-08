open Printf
open Num
open Types
open Stack
open Ops
open Parse

(* Functions *)
let print_err x = match x with
    | Stack_underflow -> print_string "stack underflow\n"
    | Parse_error -> print_string "parse error\n"
    | Type_error -> print_string "type error\n"
    | _ -> print_string "guru meditation error\n"

let stack_elem_str x = match x with
    | StkNum n -> string_of_num n
    | Unevaluated u -> sprintf "<%s>" u

let top_stack_str stk = let top = try Some (Stack.top stk) with
    | Empty -> None in match top with
    | None -> ""
    | Some x -> stack_elem_str x


let print_prompt stk = let top = top_stack_str stk in
    Printf.printf "%d: %s -- " (Stack.length stk) top

let read_loop optab =
    let stk = Stack.create() in
    while true do
            print_prompt stk;
            let x = try read_line() with
                End_of_file -> print_string "\n"; exit 0 in
            if String.length x != 0 then try parse stk optab x with e ->
                print_err e
    done;;

(* ---[ MAIN ] --- *)
print_string "Edd's Stacked Calculator\n";
read_loop (optab ())
