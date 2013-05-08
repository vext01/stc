open Types
open Num

(* returns a tuple (bool, val) *)
let is_num s =
    let i = try (true, Num.num_of_string s) with
    | Failure "num_of_string" -> (false, (num_of_int 0)) in i

let parse_unevaluated stk line = let len = String.length line in
    if String.get line 0 = '<' && String.get line (len - 1) = '>' then (
        let sub = String.sub line 1 (len - 2) in
        let open_chev = try Some (String.index sub '<')
            with Not_found -> None in
            let close_chev = try Some (String.index sub '<')
                with Not_found -> None in
                if open_chev != None or close_chev != None then
                    raise Parse_error
                else
                    Stack.push (Unevaluated sub) stk
    ) else raise Parse_error

let parse_operator stk optab line =
    let op = try Some (OpMap.find line optab)
    with Not_found -> None in try (match op with
    | None -> parse_unevaluated stk line
    | Some f -> f stk) with Stack.Empty -> raise Stack_underflow

let parse stk optab line =
    match is_num line with
    | (true, x) -> Stack.push (StkNum x) stk
    | (false, _) -> parse_operator stk optab line
