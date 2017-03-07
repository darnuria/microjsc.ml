
%{
(*
 * parser.mly
 *
 * Parser of Micro-JavaScript written with ocamlyacc:
 * consume tokens for making the first Abstract syntax tree
 * (in ast.ml)
 *
 * Part of Micro-JavaScript compiler in ML project
 * at Université Pierre et Marie Curie
 *
 * Copyright 2016 - 2017
 *
 * 3I018 Compilation Course
 * Teachers:
 *   - Frederic Peschanski
 *   - Lieu Choun Tong
 *   - Chailloux Emmanuel
 *)


  let current_pos () =
    Parseutils.make_position (Parsing.symbol_start_pos ())
                  (Parsing.symbol_end_pos ())

(* Called by the parser function on error *)
  let parse_error s =
    let pos = Parsing.symbol_start_pos () in
    Printf.printf "Error at line %d:\n ==> %s"
                  Lexing.(pos.pos_lnum) s;
    Pervasives.flush Pervasives.stdout
%}

/* (* reserved words *) */
%token FUNCTION RETURN IF VAR ELSE IN FOR WHILE
%token NULL LET

/* (* Values *) */
%token <int> INT
%token <bool> BOOL
%token <string> IDENTIFIER

/* (* Operators *) */
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
  | statements  { Ast.({ filename = ""; body = $1 }) }
;

statements:
  | /* empty */                    { [] }
  | statement separator statements { $1 :: $3 }
  | block statements               { $1 }
;

separator: SEMICOL {}
;;

statement:
  | IDENTIFIER LBRACKET expr RBRACKET EQ expr
    { Ast.ArrayAssign ($1, $3, $6, current_pos ()) }
  | IDENTIFIER EQ expr
    { Ast.Assign ($1, $3, current_pos ()) }
  | expr
    { Ast.VoidExpr ($1, current_pos ()) }
  | WHILE LPAREN expr RPAREN block
    { Ast.While($3, $5, current_pos ()) }
  | FOR LPAREN IDENTIFIER IN expr RPAREN block
    { Ast.ForIn($3, $5, $7, current_pos ()) }
  | IF LPAREN expr RPAREN block ELSE block
    { Ast.If($3, $5, $7, current_pos ()) }
  | FUNCTION IDENTIFIER LPAREN parameters RPAREN block
    { Ast.Fundef($2, $4, $6, current_pos ()) }
  | VAR IDENTIFIER EQ expr
    { Ast.Var($2, $4, current_pos ()) }
  | LET IDENTIFIER EQ expr
    { Ast.Let($2, $4, current_pos ()) }
  | RETURN expr
    { Ast.Return($2, current_pos ()) }
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
    { Ast.IntConst ($1, current_pos ()) }
  | BOOL
    { Ast.BoolConst ($1, current_pos ()) }
  | expr LBRACKET expr RBRACKET
    { Ast.ArrayRef($1, $3, current_pos ()) }
  | expr LPAREN arguments RPAREN
    { Ast.Funcall($1, $3, current_pos ()) }
  | IDENTIFIER
    { Ast.EVar ($1, current_pos ()) }
  | LPAREN expr RPAREN
    { $2 }
  | expr PLUS expr
    { Ast.(BinOp(Add, $1, $3, current_pos ())) }
  | expr MINUS expr
    { Ast.(BinOp(Sub, $1, $3, current_pos ())) }
  | expr TIMES expr
    { Ast.(BinOp(Mult, $1, $3, current_pos ())) }
  | expr DIV expr
    { Ast.(BinOp(Div, $1, $3, current_pos ())) }
  | expr EQEQ expr
    { Ast.(BinOp(BEq, $1, $3, current_pos ())) }
  | expr INF expr
    { Ast.(BinOp(BInf, $1, $3, current_pos ())) }
  | expr INFEQ expr
    { Ast.(BinOp(BInfeq, $1, $3, current_pos ())) }
  | expr SUP expr
    { Ast.(BinOp(BSup, $1, $3, current_pos ())) }
  | expr SUPEQ expr
    { Ast.(BinOp(BSupeq, $1, $3, current_pos ())) }
  | LBRACKET arguments RBRACKET
    { Ast.(ArrayLiteral($2, current_pos ())) }
  ;

arguments:
  | /* empty */ { [] }
  | expr { [$1] }
  | expr COMMA arguments { $1::$3 }

%%
