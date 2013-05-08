open Num;;

module OpMap = Map.Make(String);;

(* Calculator operations here *)
let op_eval_simple f stk =
    let arg2 = Stack.pop stk in
    let arg1 = Stack.pop stk in
    let res = f arg1 arg2 in
    Stack.push res stk;;

let op_del stk = ignore (Stack.pop stk);;

let op_swap stk =
    let arg2 = Stack.pop stk in
    let arg1 = Stack.pop stk in
    Stack.push arg1 stk; Stack.push arg2 stk;;

let rec dump_stack stk =
    match Stack.length stk with
    | 0 -> ()
    | _ -> let elem = Stack.pop stk in dump_stack stk;
    Printf.printf "  %s\n" (string_of_num elem);
    Stack.push elem stk;;

let op_dump_stack stk =
    print_string "--------\n";
    dump_stack stk;
    print_string "--------\n";;

let op_del stk = ignore (Stack.pop stk);;

let op_swap stk = let o1 = Stack.pop stk in
    let o2 = Stack.pop stk in
    Stack.push o1 stk; Stack.push o2 stk;;

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
    !optab;;
