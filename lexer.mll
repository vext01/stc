{
    open Num
    open Parser
}

let digit = ['0'-'9']
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9']

rule token = parse
    | '<'                   { LCHEV }
    | '>'                   { RCHEV }
    | '+'                   { PLUS }
    | '-'                   { MINUS }
    | '*'                   { MULT }
    | '/'                   { DIV }
    | digit+ as x
    | digit+ '/' digit+
    | '-' digit+ as x       { BIGNUM (num_of_string x) }
    | ','                   { COMMA }
    | 's'                   { SWAP }
    | 'p'                   { DUMP }
    | 'e'                   { EVAL }
    | '`'                   { DEL }
    | 'c'                   { CLEAR }
    | 'f'                   { FOLD }
    | '\n'                  { ENDLINE }
    | eof                   { raise End_of_file }
    | _                     { token lexbuf }
