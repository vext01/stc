open Num
open Printf
open Types

let rec print_list ls = match ls with
| [] -> ()
| l -> match List.hd l with
    Stack_num x -> printf "%s\n" (string_of_num x); print_list (List.tl l)
| _ -> printf "How do i print that?\n"

let main () =
    try
        let lexbuf = Lexing.from_channel stdin in
        while true do
            let l = Parser.input Lexer.token lexbuf in
            printf "Got a list of %d elems\n" (List.length l); ignore (flush stdout);
            print_list l; print_string "\n"; flush stdout
        done
    with End_of_file -> exit 0
      
let _ = Printexc.print main ()
