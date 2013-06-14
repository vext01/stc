open Types
open Num
open Printf
open Stack

let my_string_of_num x = match x with
    | Ratio x as x'-> Printf.sprintf "%s  (~%s)" (string_of_num (x')) (approx_num_fix 8 x')
    | x as x' -> Printf.sprintf "%s" (string_of_num x')

(* print stuff on the stack *)
let rec command_str x = match x with
    | Oper x -> oper_str x
    | Stk_elem x -> stack_elem_str x
and oper_str x = match x with
    | Oper_plus -> "+"
    | Oper_minus -> "-"
    | Oper_mult -> "*"
    | Oper_div -> "/"
    | Oper_del -> "`"
    | Oper_swap -> "s"
    | Oper_clear -> "c"
    | Oper_eval -> "e"
    | Oper_dump -> "p"
    | Oper_fold -> "f"
    | Oper_approx -> "a"
    | Oper_mod -> "%"
    | Oper_abs -> "|"
    | Oper_pow -> "^"
    | Oper_dup -> "d"
    | Oper_store -> "S"
    | Oper_recall -> "R"
    | Oper_regs -> "r"
    | Oper_clearreg -> "R"
    | Oper_evalreg x -> x
    | Oper_sum -> "E"
    | Oper_help -> "?"
and stack_elem_str x = match x with
    | Stk_num n -> my_string_of_num n
    | Stk_str s -> String.concat "" ["\""; s; "\""]
    | Stk_reg r -> r
    | Stk_uneval u -> let cmd_strs = List.map (command_str) u in
        let s = String.concat ", " cmd_strs in sprintf "<%s>" s

let rec dump_stack stk n =
    if Stack.length stk = 0 then
        ()
    else
        let elem = Stack.pop stk in dump_stack stk (n - 1);
        printf "  %3u: %s\n" n (stack_elem_str elem); flush stdout;
        push elem stk 
