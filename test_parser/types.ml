type operator =
    | Oper_plus
    | Oper_minus

type stack_elem =
    | Stk_num of Num.num
    | Stk_uneval of command list
and
command = Stk_elem of stack_elem | Oper of operator
