type stack_item =
        | StkNum of Num.num
        | Unevaluated of string

module OpMap = Map.Make(String);;

exception Type_error
exception Parse_error
exception Stack_underflow
