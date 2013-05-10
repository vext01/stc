type stack_elem =
    | Stack_num of Num.num
    | Stack_uneval of stack_elem list
    | Stack_plus
    | Stack_minus
