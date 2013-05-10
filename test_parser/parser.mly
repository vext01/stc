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
%type <Types.command list> input

%%

input:
    | /* empty */   { [] }
    | expr ENDLINE  { $1 }

expr:
    | expr COMMA term   { $1 @ $3 }
    | term              { $1 }
    ;

term:
    | oper              { [ $1 ] }
    | BIGNUM            { [ Stack_elem (Stack_num $1) ] }
    | uneval            { [ Stack_elem (Uneval $1) ] }

oper:
    | PLUS              { Operator Plus }
    | MINUS             { Operator Minus }

uneval:
    | LCHEV expr RCHEV       { $2 }
%%
