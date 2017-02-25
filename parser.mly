
%{
(* Parsing of micro-JavaScript use Ocamlyacc *)
  open Parseutils
  open Ast
  open Lexing

  let current_pos () =
    make_position (Parsing.symbol_start_pos ())
                  (Parsing.symbol_end_pos ())

(* Called by the parser function on error *)
  let parse_error s =
    Printf.printf "Error at line %d:\n ==> "
                  (Parsing.symbol_start_pos ()).pos_lnum;
    print_endline s;
    flush stdout

%}

/* (* reserved words *) */

%token FUNCTION RETURN IF VAR ELSE IN FOR WHILE
%token NULL LET

%token <int> INT
%token <bool> BOOL
%token <string> IDENTIFIER
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token INF INFEQ SUP SUPEQ EQ EQEQ

/* (* control characters *) */
%token EOF
%token COMMA SEMICOL LCURLY RCURLY LBRACKET RBRACKET

/* (* expressions *) */

%left PLUS MINUS    /* lowest precedence */
%left TIMES DIV     /* medium precedence */
%left UMINUS        /* highest precedence */
%nonassoc EQEQ

%start program      /* the entry point */
%type <Ast.program> program
%type <Ast.expr> expr
%%

program:
  | statements  { { filename = ""; body = $1 } }
;

statements:
  | /* empty */                    { [] }
  | statement separator statements { $1::$3 }
  | block statements               { $1 }
;

separator: SEMICOL {}
;;

statement:
  | IDENTIFIER LBRACKET expr RBRACKET EQ expr
    { ArrayAssign ($1, $3, $6, current_pos ()) }
  | IDENTIFIER EQ expr
    { Assign ($1, $3, current_pos ()) }
  | expr
    { VoidExpr ($1, current_pos ()) }
  | WHILE LPAREN expr RPAREN block
    { While($3, $5, current_pos ()) }
  | FOR LPAREN IDENTIFIER IN expr RPAREN block
    { ForIn($3, $5, $7, current_pos ()) }
  | IF LPAREN expr RPAREN block ELSE block
    { If($3, $5, $7, current_pos ()) }
  | FUNCTION IDENTIFIER LPAREN parameters RPAREN block
    { Fundef($2, $4, $6, current_pos ()) }
  | VAR IDENTIFIER EQ expr
    { Var($2, $4, current_pos ()) }
  | LET IDENTIFIER EQ expr
    { Let($2, $4, current_pos ()) }
  | RETURN expr
    { Return($2, current_pos ()) }
;

block:
  | LCURLY statements RCURLY { $2 }
  ;

parameters:
  | /* empty */                 { [] }
  | IDENTIFIER                  { [ $1 ] }
  | IDENTIFIER COMMA parameters { $1 :: $3 }


expr:
  | INT
    { IntConst ($1, current_pos ()) }
  | BOOL
    { BoolConst ($1, current_pos ()) }
  | expr LBRACKET expr RBRACKET
    { ArrayRef($1, $3, current_pos ()) }
  | expr LPAREN arguments RPAREN
    { Funcall($1, $3, current_pos ()) }
  | IDENTIFIER
    { EVar ($1, current_pos ()) }
  | LPAREN expr RPAREN
    { $2 }
  | expr PLUS expr
    { BinOp(Add, $1, $3, current_pos ()) }
  | expr MINUS expr
    { BinOp(Sub, $1, $3, current_pos ()) }
  | expr TIMES expr
    { BinOp(Mult, $1, $3, current_pos ()) }
  | expr DIV expr
    { BinOp(Div, $1, $3, current_pos ()) }
  | expr EQEQ expr
    { BinOp(BEq, $1, $3, current_pos ()) }
  | expr INF expr
    { BinOp(BInf, $1, $3, current_pos ()) }
  | expr INFEQ expr
    { BinOp(BInfeq, $1, $3, current_pos ()) }
  | expr SUP expr
    { BinOp(BSup, $1, $3, current_pos ()) }
  | expr SUPEQ expr
    { BinOp(BSupeq, $1, $3, current_pos ()) }
  | LBRACKET arguments RBRACKET
    { ArrayLiteral($2, current_pos ()) }
  ;

arguments:
  | /* empty */ { [] }
  | expr { [$1] }
  | expr COMMA arguments { $1::$3 }

%%
