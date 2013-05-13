{
    open Num
    open Parser
    open String

    let num_of_dec_string s =
        let idx = index s '.' in
        let len = length s in
        let whole = sub s 0 idx in
        let frac = sub s (idx + 1) (len - idx - 1) in
        let scale = power_num (num_of_int 10) (num_of_int (length frac)) in
        let conc = concat "" [whole; frac] in
        let move_dp = num_of_string conc in
        div_num move_dp scale
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
    | digit+ '.' digit+ as x{ BIGNUM (num_of_dec_string x) }
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
    | 'a'                   { APPROX }
    | '%'                   { MOD }
    | '|'                   { ABS }
    | 'd'                   { DUP }
    | '^'                   { POW }
    | '\n'                  { ENDLINE }
    | '$' alphanum as x     { REG x }
    | 'S'                   { STORE }
    | 'R'                   { RECALL }
    | 'r'                   { REGS }
    | eof                   { raise End_of_file }
    | _                     { token lexbuf }
