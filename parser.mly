%{
    (* declarations *)
    open Printf
    open Types
    let parse_error s = printf "%s\n" s
    let fso () = ignore (flush stdout)
%}

%token <Num.num> BIGNUM
%token <string> REG
%token <string> EVALREG
%token LCHEV RCHEV COMMA
%token PLUS MINUS DIV MULT SUM
%token REG REGS STORE RECALL CLEARREG
%token SWAP DUMP EVAL DEL CLEAR FOLD APPROX
%token POW ABS MOD DUP
%token ENDLINE BADTOK

%start input
%type <Types.command list> input

%%

input:
    | /* empty */       { [] }
    | expr ENDLINE      { $1 }

expr:
    | expr COMMA term   { $1 @ $3 }
    | expr COMMA        { $1 }
    | term              { $1 }

term:
    | oper              { [ $1 ] }
    | BIGNUM            { [ Stk_elem (Stk_num $1) ] }
    | uneval            { [ Stk_elem (Stk_uneval $1) ] }
    | REG               { [ Stk_elem (Stk_reg $1) ] }

oper:
    | PLUS              { Oper Oper_plus }
    | MINUS             { Oper Oper_minus }
    | MULT              { Oper Oper_mult }
    | DIV               { Oper Oper_div }
    | SWAP              { Oper Oper_swap }
    | DUMP              { Oper Oper_dump }
    | EVAL              { Oper Oper_eval }
    | DEL               { Oper Oper_del }
    | CLEAR             { Oper Oper_clear }
    | FOLD              { Oper Oper_fold }
    | APPROX            { Oper Oper_approx }
    | POW               { Oper Oper_pow }
    | MOD               { Oper Oper_mod }
    | ABS               { Oper Oper_abs }
    | DUP               { Oper Oper_dup }
    | REGS              { Oper Oper_regs }
    | STORE             { Oper Oper_store }
    | RECALL            { Oper Oper_recall }
    | CLEARREG          { Oper Oper_clearreg }
    | EVALREG           { Oper (Oper_evalreg $1) }
    | SUM               { Oper Oper_sum }

uneval:
    | LCHEV expr RCHEV  { $2 }
%%
