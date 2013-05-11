{
    open Num
    open Parser
}

let digit = ['0'-'9']
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9']

rule token = parse
    | '<'           { LCHEV }
    | '>'           { RCHEV }
    | '+'           { PLUS }
    | '-'           { MINUS }
    | digit+ as x   { BIGNUM (num_of_string x) }
    | ','           { COMMA }
    | '\n'          { ENDLINE }
    | eof           { raise End_of_file }
    | _             { token lexbuf }
