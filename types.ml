exception Type_error
exception Parse_error

type operator =
    (* arithmetic *)
    | Oper_plus
    | Oper_minus
    | Oper_mult
    | Oper_div
    (* stack management *)
    | Oper_del
    | Oper_swap
    | Oper_clear
    | Oper_dump
    (* black magic *)
    | Oper_eval
    | Oper_fold

type stack_elem =
    | Stk_num of Num.num
    | Stk_uneval of command list
and command = Stk_elem of stack_elem | Oper of operator
