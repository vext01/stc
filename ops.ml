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

let rec op_eval stk =
    let e = pop stk in (match e with
        | Stk_elem (Stk_uneval l) -> eval_command_list stk l
        | _ -> ()
    )
and op_fold stk = ()
    (* XXX
    let e = pop stk in match e with
        | Stk_elem Stk_uneval x -> let times = (length stk) - 1 in
            for i=1 to times do
                push e stk; eval_command_list stk x
            done
        | _ -> raise Type_error
        *)
and eval_oper stk o = match o with
    | Oper_plus -> op_eval_simple Num.add_num stk
    | Oper_minus -> op_eval_simple Num.sub_num stk
    | Oper_mult -> op_eval_simple Num.mult_num stk
    | Oper_div -> op_eval_simple Num.div_num stk
    | Oper_del -> op_del stk
    | Oper_clear -> Stack.clear stk
    | Oper_swap -> op_swap stk
    | Oper_dump -> op_dump stk
    | Oper_eval -> () (* XXX *)
    | Oper_fold -> op_fold stk
and eval_command stk c = match c with
    | Stk_elem x -> Stack.push x stk
    | Oper x -> eval_oper stk x
and eval_command_list stk l =
    List.iter (eval_command stk) l
