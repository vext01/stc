open Printf;;
open Num;;      (* arbitrary precision numbers *)
open Hashtbl;;

(* Map module for the operation table *)
module StringMap = Map.Make(String);;

type op = Operation of (Num.num Stack.t -> unit);;
type err = Parse_error | Stack_underflow;;

(* Functions *)
let print_err x = match x with
        | Stack_underflow -> print_string "stack underflow\n"
        | Parse_error -> print_string "parse error\n";;

(* returns a tuple (bool, val) *)
let is_num s =
        let i = try (true, Num.num_of_string s) with
        | Failure "num_of_string" -> (false, (num_of_int 0)) in
        i;;

let rec dump_stack stk =
        match Stack.length stk with
        | 0 -> ()
        | _ -> let elem = Stack.pop stk in dump_stack stk;
                Printf.printf "  %s\n" (string_of_num elem);
                Stack.push elem stk;;
 
(* second level parsing *)
let parse_operator stk optab line =
        try let op = StringMap.find line optab in
        op stk
        with Not_found -> print_string " **parse error\n";;

(* top level parsing *)
let parse_line stk optab line =
        match is_num line with
        | (true, x) -> Stack.push x stk
        | (false, _) -> parse_operator stk optab line;;

let read_loop optab =
        let stk = Stack.create() in
        while true do
                Printf.printf "%3u -> " (Stack.length stk);
                let x = try read_line() with
                        End_of_file -> print_string "\n"; exit 0 in
                if String.length x != 0 then parse_line stk optab x
        done;;

let op_del stk = ignore (Stack.pop stk);;
let op_swap stk = let o1 = Stack.pop stk in
        let o2 = Stack.pop stk in
        Stack.push o1 stk; Stack.push o2 stk;;

(* Operations here. XXX move to separate compilation unit *)
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

(* create and load up the operation mapping *)
let init_optab () =
        let optab = ref StringMap.empty in
        optab := StringMap.add "+" (op_eval_simple Num.add_num) !optab;
        optab := StringMap.add "p" dump_stack !optab;
        optab := StringMap.add "+" (op_eval_simple Num.add_num) !optab;
        optab := StringMap.add "-" (op_eval_simple Num.sub_num) !optab;
        optab := StringMap.add "*" (op_eval_simple Num.mult_num) !optab;
        optab := StringMap.add "/" (op_eval_simple Num.div_num) !optab;
        optab := StringMap.add "`" op_del !optab;
        optab := StringMap.add "s" op_swap !optab;
        !optab;;

(* ---[ MAIN ] --- *)
print_string "Edd's Stacked Calculator\n";
read_loop (init_optab ());;
