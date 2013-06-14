exception Type_error
exception No_reg_error

type operator =
    (* arithmetic *)
    | Oper_plus
    | Oper_minus
    | Oper_mult
    | Oper_div
    | Oper_pow
    | Oper_mod
    | Oper_abs
    | Oper_sum
    (* stack management *)
    | Oper_del
    | Oper_swap
    | Oper_clear
    | Oper_dump
    | Oper_dup
    (* Register management *)
    | Oper_store
    | Oper_recall
    | Oper_regs
    | Oper_clearreg
    | Oper_evalreg of string
    (* black magic *)
    | Oper_eval
    | Oper_fold
    | Oper_approx

type stack_elem =
    | Stk_num of Num.num
    | Stk_str of string
    | Stk_reg of string
    | Stk_uneval of command list
and command = Stk_elem of stack_elem | Oper of operator
