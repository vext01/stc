%{
    (* declarations *)
    open Printf
    open Types
    let parse_error s = printf "%s\n" s
    let fso () = ignore (flush stdout)
%}

%token <Num.num> BIGNUM
%token LCHEV RCHEV COMMA
%token PLUS MINUS DIV MULT
%token SWAP DUMP_STACK EVAL DEL CLEAR FOLD
%token ENDLINE

%start input
%type <Types.command list> input

%%

input:
    | /* empty */       { [] }
    | expr ENDLINE      { $1 }

expr:
    | expr COMMA term   { $1 @ $3 }
    | term              { $1 }

term:
    | oper              { [ $1 ] }
    | BIGNUM            { [ Stk_elem (Stk_num $1) ] }
    | uneval            { [ Stk_elem (Stk_uneval $1) ] }

oper:
    | PLUS              { Oper Oper_plus }
    | MINUS             { Oper Oper_minus }
    | MULT              { Oper Oper_mult }
    | DIV               { Oper Oper_div }

uneval:
    | LCHEV expr RCHEV  { $2 }
%%
