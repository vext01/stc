open Num;;
open Stack
open Types
open Printf
open Parse

(* Calculator operations here *)
let op_eval_simple f stk =
    if length stk < 2 then raise Stack_underflow else
    let arg2 = pop stk in let arg1 = pop stk in
    match (arg1, arg2) with
        | (StkNum a1, StkNum a2) -> let res = f a1 a2 in push (StkNum res) stk
        | _ -> push arg2 stk; push arg1 stk; raise Type_error

let op_del stk = ignore (Stack.pop stk);;

let op_swap stk =
    let arg2 = Stack.pop stk in
    let arg1 = Stack.pop stk in
    Stack.push arg1 stk; Stack.push arg2 stk;;

let rec dump_stack stk =
    match Stack.length stk with
    | 0 -> ()
    | _ -> let elem = Stack.pop stk in dump_stack stk;
    (match elem with
        | StkNum x -> printf "  %s\n" (string_of_num x)
        | Unevaluated x -> printf "  <%s>\n" x
    ); push elem stk

let op_dump_stack stk =
    print_string "--------\n";
    dump_stack stk;
    print_string "--------\n";;

let op_del stk = ignore (Stack.pop stk);;

let op_swap stk = let o1 = Stack.pop stk in
    let o2 = Stack.pop stk in
    Stack.push o1 stk; Stack.push o2 stk;;

let op_eval optab stk =
    if length stk < 1 then
        raise Stack_underflow
    else
        let e = pop stk in match e with
            | Unevaluated s -> parse stk optab s
            | _ -> raise Type_error

let op_fold optab stk =
    if length stk < 2 then
        raise Stack_underflow
    else let e = pop stk in match e with
        | Unevaluated x -> let times = (length stk) - 1 in
            for i=1 to times do
                push e stk; op_eval optab stk
            done
        | _ -> raise Type_error

(* create and load up the operation mapping *)
let optab () =
    let optab = ref OpMap.empty in
    optab := OpMap.add "+" (op_eval_simple Num.add_num) !optab;
    optab := OpMap.add "p" op_dump_stack !optab;
    optab := OpMap.add "+" (op_eval_simple Num.add_num) !optab;
    optab := OpMap.add "-" (op_eval_simple Num.sub_num) !optab;
    optab := OpMap.add "*" (op_eval_simple Num.mult_num) !optab;
    optab := OpMap.add "/" (op_eval_simple Num.div_num) !optab;
    optab := OpMap.add "`" op_del !optab;
    optab := OpMap.add "s" op_swap !optab;
    optab := OpMap.add "c" Stack.clear !optab;
    optab := OpMap.add "e" (op_eval !optab) !optab;
    optab := OpMap.add "f" (op_fold !optab) !optab;
    !optab;;
