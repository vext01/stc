open Types

let eval_oper stk o = match o with
    | Oper_plus -> Ops.op_eval_simple Num.add_num stk
    | Oper_minus -> Ops.op_eval_simple Num.sub_num stk
    | Oper_mult -> Ops.op_eval_simple Num.mult_num stk
    | Oper_div -> Ops.op_eval_simple Num.div_num stk
    | Oper_del -> Ops.op_del stk
    | Oper_clear -> Stack.clear stk
    | Oper_swap -> Ops.op_swap stk
    | Oper_dump -> Ops.op_dump stk
    | Oper_eval -> () (* XXX *)
    | Oper_fold -> Ops.oper_fold stk

let eval_command stk c = match c with
    | Stk_elem x -> Stack.push x stk
    | Oper x -> Ops.eval_oper stk x

let eval_command_list stk l =
    List.iter (eval_command stk) l
