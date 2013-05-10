open Types
open Num
open Util
open Printf

(* returns a tuple (bool, val) *)
let is_num s =
    let i = try (true, Num.num_of_string s) with
    | Failure "num_of_string" -> (false, (num_of_int 0)) in i

(* Lastly, we look for operators *)
let parse_operator stk optab line =
    let op = try Some (OpMap.find line optab)
    with Not_found -> None in try (match op with
    | None -> raise Parse_error
    | Some f -> f stk) with Stack.Empty -> raise Stack_underflow

(* Third, we look for numbers *)
let parse_num stk optab line =
    match is_num line with
    | (true, x) -> Stack.push (StkNum x) stk
    | (false, _) -> parse_operator stk optab line

(* Second, we look for multiple stack items (comma-separated) *)
let rec parse_multi stk optab line =
    let elems = split_string line ',' in
    match elems with
    | [x] -> parse_num stk optab x
    | _ -> List.iter (parse stk optab) elems
and
(* First we look for unevaluated code *)
parse stk optab line = let len = String.length line in
    if String.get line 0 = '<' && String.get line (len - 1) = '>' then
        let sub = String.sub line 1 (len - 2) in
            Stack.push (Unevaluated sub) stk
    else parse_multi stk optab line
