open Printf;;
open Num;;      (* arbitrary precision numbers *)
open Hashtbl;;

(* HashTable Module *)
module StringMap = Map.Make(String);;

(* Variant Types *)
type op = UnaryOp of (Num.num -> Num.num)
        | BinaryOp of (Num.num -> Num.num -> Num.num)
        | TrinaryOp of (Num.num -> Num.num -> Num.num -> Num.num)
        | SpecialOp of (Num.num Stack.t -> unit );;

type err = Parse_error | Stack_underflow;;

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

let eval_op op stk = match op with
        | UnaryOp f -> let res = f (Stack.pop stk) in
                Stack.push res stk
        | BinaryOp f -> let oprnd1 = Stack.pop stk in
                let oprnd2 = Stack.pop stk in
                let res = f oprnd2 oprnd1 in
                Stack.push res stk
        | TrinaryOp f -> let oprnd1 = Stack.pop stk in
                let oprnd2 = Stack.pop stk in
                let oprnd3 = Stack.pop stk in
                let res = f oprnd3 oprnd2 oprnd1 in
                Stack.push res stk
        | SpecialOp f -> f stk;;

(* second level parsing *)
let parse_operator stk optab line =
        try let op = StringMap.find line optab in
        eval_op op stk
        with Not_found -> print_string "parse error\n";;

(* top level parsing, returns new stack *)
let parse_line stk optab line =
        match is_num line with
        | (true, x) -> Stack.push x stk
        | (false, _) -> parse_operator stk optab line;;

let read_loop optab =
        let stk = Stack.create() in
        while true do
                dump_stack stk;
                print_string("-> ");
                let x = read_line() in
                if String.length x != 0 then parse_line stk optab x
        done;;

let print_err x = match x with
        | Stack_underflow -> print_string "stack underflow\n"
        | Parse_error -> print_string "parse error\n";;

let op_del stk = 
        if (Stack.length stk) < 1 then
                print_err Stack_underflow
        else
                ignore (Stack.pop stk);;

(* create and load up the operation mapping *)
let init_optab () =
        let optab = ref StringMap.empty in
        optab := StringMap.add "+" (BinaryOp Num.add_num) !optab;
        optab := StringMap.add "-" (BinaryOp Num.sub_num) !optab;
        optab := StringMap.add "*" (BinaryOp Num.mult_num) !optab;
        optab := StringMap.add "/" (BinaryOp Num.div_num) !optab;
        optab := StringMap.add "`" (SpecialOp op_del) !optab;
        !optab;;

(* ---[ MAIN ] --- *)
print_string "Edd's Stacked Calculator\n";
read_loop (init_optab ());;
