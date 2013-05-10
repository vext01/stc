{
    open Num
    open Parser
}

let digit = ['0'-'9']
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9']

rule token = parse
    | '<'           { print_string "*lchev\n"; LCHEV }
    | '>'           { print_string "*rchev\n"; RCHEV }
    | '+'           { PLUS }
    | '-'           { MINUS }
    | digit+ as x   { print_string "*num"; BIGNUM (num_of_string x) }
    | alphanum+ as x{ print_string "*arb\n"; ARBITRARY x }
    | ','           { COMMA }
    | '\n'          { ENDLINE }
    | eof           { raise End_of_file }
    | _             { token lexbuf }
