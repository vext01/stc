open Num
open Stack
open Types
open Printf
open Util

let check_stack stk h =
    if Stack.length stk < h then raise Stack_underflow

(* Calculator operations here *)
let op_eval_simple f stk =
    check_stack stk 2;
    let arg2 = pop stk in let arg1 = pop stk in
    match (arg1, arg2) with
        | (Stk_num a1, Stk_num a2) -> let res = f a1 a2 in push (Stk_num res) stk
        | _ -> raise Type_error
        (* | _ -> push arg2 stk; push arg1 stk; raise Type_error *)

let op_del stk = ignore (Stack.pop stk);;

let op_dump stk =
    print_string "--------\n";
    dump_stack stk;
    print_string "--------\n";;

let op_del stk = ignore (Stack.pop stk);;

let op_swap stk = let o1 = Stack.pop stk in
    let o2 = Stack.pop stk in
    Stack.push o1 stk; Stack.push o2 stk;;

(*
let op_eval optab stk =
    if length stk < 1 then
        raise Stack_underflow
    else
        let e = pop stk in match e with
            | Unevaluated s -> parse stk optab s
            | _ -> raise Type_error
            *)

let op_fold optab stk =
    let e = pop stk in match e with
        | Stk_elem Stk_uneval x -> let times = (length stk) - 1 in
            for i=1 to times do
                push e stk; Eval.eval_command_list optab stk
            done
        | _ -> raise Type_error
