%{
    (* declarations *)
    open Printf
    open Types
    let parse_error s = printf "%s\n" s
    let fso () = ignore (flush stdout)
%}

%token <Num.num> BIGNUM
%token <string> ARBITRARY
%token LCHEV RCHEV COMMA
%token PLUS MINUS
%token ENDLINE

%start input
%type <Types.stack_elem list> input

%%

input:
    | /* empty */   { [] }
    | expr ENDLINE  { printf "got expr\n"; fso(); $1 }

expr:
    | BIGNUM            { printf "got bignum \n"; fso(); [ Stack_num $1 ] }
    | oper              { printf "got op\n"; fso(); [ $1 ] }
    | expr COMMA expr   { printf "got composite\n"; fso(); $1 @ $3 }
    | uneval            { [ Stack_uneval $1 ] }
    ;

oper:
    | PLUS              { Stack_plus }
    | MINUS             { Stack_minus }

uneval:
    | LCHEV expr RCHEV       { $2 }
%%
