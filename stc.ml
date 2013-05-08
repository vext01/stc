open Printf;;
open Num;;      (* arbitrary precision numbers *)

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

let top_stack_str stk = let top = try Some (Stack.top stk) with
    Stack.Empty -> None in match top with
    | None -> ""
    | Some x -> Num.string_of_num x;;

(* second level parsing *)
let parse_operator stk optab line =
    let op = try Some (Ops.OpMap.find line optab)
    with Not_found -> None in try (match op with
    | None -> print_err Parse_error
    | Some f -> f stk) with Stack.Empty -> print_err Stack_underflow;;

(* top level parsing *)
let parse_line stk optab line =
    match is_num line with
    | (true, x) -> Stack.push x stk
    | (false, _) -> parse_operator stk optab line;;

let print_prompt stk = let top = top_stack_str stk in
    Printf.printf "%d: %s> " (Stack.length stk) top;;

let read_loop optab =
    let stk = Stack.create() in
    while true do
            print_prompt stk;
            let x = try read_line() with
                    End_of_file -> print_string "\n"; exit 0 in
            if String.length x != 0 then parse_line stk optab x
    done;;

(* ---[ MAIN ] --- *)
print_string "Edd's Stacked Calculator\n";
read_loop (Ops.optab ());;
