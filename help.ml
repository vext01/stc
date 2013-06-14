open Types

(* Kind of hacky way to iterate the operators *)
let show_help_for = [
    Oper_plus; Oper_minus; Oper_mult; Oper_div; Oper_pow; Oper_mod; Oper_abs;
    Oper_sum; Oper_del; Oper_swap; Oper_clear; Oper_dump; Oper_dup; Oper_store;
    Oper_recall; Oper_regs; Oper_clearreg; Oper_evalreg "xxx"; Oper_eval;
    Oper_fold; Oper_help
]

let op_help_str op = match op with
    (* Arithmetic *)
    | Oper_plus -> "+      (2:1)\tPushes #1 plus #0"
    | Oper_minus -> "-      (2:1)\tPushes #1 subtracted from #0"
    | Oper_mult -> "*      (2:1)\tPushes the product of #1 and #0"
    | Oper_div -> "/      (2:1)\tPushes #1 divided by #0"
    | Oper_pow -> "^      (2:1)\tPushes #1 to the power of #0"
    | Oper_mod -> "%      (2:1)\tPushes #1 modulo #0"
    | Oper_abs -> "|      (1:1)\tPushes the absolute value of #0"
    | Oper_sum -> "E      (n:1)\tFolds the stack over addition"
    (* Stack management *)
    | Oper_del -> "`      (1:0)\tDrop the top element of the stack"
    | Oper_swap -> "s      (2:2)\tSwap #0 and #1"
    | Oper_clear -> "c      (n:0)\tClears the stack (of length n) and clears registers"
    | Oper_dump -> "p      (0:0)\tPrints the stack to stdout"
    | Oper_dup -> "d      (1:2)\tCopies #0 back twice (duplicate)"
    (* Register management *)
    | Oper_store -> "S      (2:0)\tStores #1 into the register #0"
    | Oper_recall -> "R      (1:1)\tRecalls the value of register #0"
    | Oper_regs -> "r      (0:0)\tPrints the contents of registers to stdout"
    | Oper_clearreg -> "C      (1:0)\tClears register #0"
    | Oper_evalreg _ -> "$$x    (1:n)\tEvaluates contents of register 'x'"
    (* Other *)
    | Oper_eval -> "e      (1:n)\tEvaluates #0"
    | Oper_fold -> "f      (n:1)\tFolds the stack over the unevaluated code in #1"
    | Oper_help -> "h/?/H  (0:0)\tShows this message"

let print_help_str op = Printf.printf "  %s.\n" (op_help_str op)

let print_help () =
    print_string "\n Oper    (-:+)\tDescription\n";
    print_string ((String.make 80 '-') ^ "\n");
    List.iter (print_help_str) show_help_for;
    print_string "\nWhere the stack is [ #n-1; ...; #1; #0 ] ";
    print_string "and #0 is the top of the stack.\n\n"

