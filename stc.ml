open Printf;;
open Num;;      (* arbitrary precision numbers *)

(* returns a tuple (bool, val) *)
let is_num s =
        let i = try (true, Num.num_of_string s) with
        | Failure "num_of_string" -> (false, num_of_int 0) in
        i;;

let dump_stack stk =
        Printf.printf "stack height: %u\n" (List.length stk);
        List.iter (fun x -> Printf.printf "  %s\n" (string_of_num x)) stk;;

(* returns new stack *)
let parse_line stk line =
        match is_num line with
        | (true, x) -> stk @ [x]
        | (false, _) -> (print_string "parse error\n"); stk;;

let read_loop () = 
        let stk = ref [] in (* it's actually easier to use a list as a stack *)
        while true do
                dump_stack !stk;
                print_string("-> ");
                let x = read_line() in
                if String.length x != 0 then stk := parse_line !stk x
        done;;

print_string "Edd's Stacked Calculator\n";
read_loop();;
