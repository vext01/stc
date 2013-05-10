type operator =
    | Plus
    | Minus

type stack_elem =
    | Stack_num of Num.num
    | Uneval of command list
and
command = Stack_elem of stack_elem | Operator of operator
