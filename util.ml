open Types
open Num
open Printf
open Stack

(* print stuff on the stack *)
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

let rec dump_stack stk n =
    if Stack.length stk = 0 then
        ()
    else
        let elem = Stack.pop stk in dump_stack stk (n - 1);
        printf "  %3u: %s\n" n (stack_elem_str elem); flush stdout;
        push elem stk 
