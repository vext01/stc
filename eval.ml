open Num
open Stack
open Types
open Printf
open Util

let op_eval_simple f stk =
    let arg2 = pop stk in let arg1 = pop stk in
    match (arg1, arg2) with
        | (Stk_num a1, Stk_num a2) -> let res = f a1 a2 in push (Stk_num res) stk
        | _ -> raise Type_error

let op_del stk = ignore (pop stk);;

let op_dump stk =
    print_string "\n";
    dump_stack stk (Stack.length stk);
    print_string "\n";;

let op_del stk = ignore (pop stk);;

let op_swap stk = let o1 = pop stk in
    let o2 = pop stk in
    push o1 stk; push o2 stk;;

let op_approx stk =
    let x = pop stk in match x with
        | Stk_num n -> push (Stk_str (approx_num_fix 8 n)) stk
        | _ -> raise Type_error

let op_dup stk =
    let e = pop stk in push e stk; push e stk

let op_abs stk = match pop stk with
        | Stk_num o -> push (Stk_num (abs_num o)) stk
        | _ -> raise Type_error

let op_store stk = ()
let op_recall stk = ()
let op_regs stk = ()

let rec op_eval stk =
    let e = pop stk in match e with
        | Stk_uneval l -> eval_command_list stk l
        | _ -> push e stk
and op_fold stk =
    let e = pop stk in match e with
        | Stk_uneval x -> let times = (length stk) - 1 in
            for i=1 to times do
                push e stk; op_eval stk
            done
        | _ -> raise Type_error
and eval_oper stk o = match o with
    | Oper_plus -> op_eval_simple Num.add_num stk
    | Oper_minus -> op_eval_simple Num.sub_num stk
    | Oper_mult -> op_eval_simple Num.mult_num stk
    | Oper_div -> op_eval_simple Num.div_num stk
    | Oper_del -> op_del stk
    | Oper_clear -> Stack.clear stk
    | Oper_swap -> op_swap stk
    | Oper_dump -> op_dump stk
    | Oper_eval -> op_eval stk
    | Oper_fold -> op_fold stk
    | Oper_approx -> op_approx stk
    | Oper_dup -> op_dup stk
    | Oper_mod -> op_eval_simple Num.mod_num stk
    | Oper_pow -> op_eval_simple Num.power_num stk
    | Oper_abs -> op_abs stk
    | Oper_store -> op_store stk
    | Oper_recall -> op_recall stk
    | Oper_regs -> op_regs stk
and eval_command stk c = match c with
    | Stk_elem x -> push x stk
    | Oper x -> eval_oper stk x
and eval_command_list stk l =
    List.iter (eval_command stk) l
