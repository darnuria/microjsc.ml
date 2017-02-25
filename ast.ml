
open Printf
open Parseutils
open Utils

type program = { filename: string; body: statements }

and statements = statement list

and statement =
  (* function fname(param1, param2, ...) *)
  | Fundef of (string * string list * statements * parse_pos)
  (* <expr>  (drop result) *)
  | VoidExpr of (expr * parse_pos)
  (* var name = <expr>    (global variable) *)
  | Var of (string * expr * parse_pos)
  (* let name = <expr>   (lexical variable) *)
  | Let of (string * expr * parse_pos)
  (* <var> = <expr> *)
  | Assign of (string * expr * parse_pos)
  (* <var>[<pos>] = <expr> *)
  | ArrayAssign of (string * expr * expr * parse_pos)
  (* while(<cond>){ <statements> ... } *)
  | While of (expr * statements * parse_pos)
  (* if(<cond>) { <consequent> ... } *)
  (* if(<cond>) { <consequent> ... } else { <alternative> ... } *)
  | If of (expr * statements * statements * parse_pos)
  (* for (<var> in <expr>) { <statements> ... }  (property enumeration) *)
  | ForIn of (string * expr * statements * parse_pos)
  (* return <expr>    (function return) *)
  | Return of (expr * parse_pos)

 and expr =
   (* 1, 23, -12, etc. *)
   | IntConst of (int * parse_pos)
   (* 1.2,  -54.42   2.2e-17 etc. *)
   | FloatConst of (float * parse_pos)
   (* true, false *)
   | BoolConst of (bool * parse_pos)
   | EVar of (string * parse_pos)
   (* <expr>  <BINOP> <expr> *)
   | BinOp of (binop_name * expr * expr * parse_pos)
   (* <UNOP> <expr> *)
   | UnOp of (unop_name * expr * parse_pos)
   (* "this is a string"   etc. *)
   | StringLiteral of (string * parse_pos)
   (* [ elem0, elem1, ... ] *)
   | ArrayLiteral of (expr list * parse_pos)
   (* <expr>[<expr>]   array access *)
   | ArrayRef of (expr * expr * parse_pos)
   | Funcall of (expr * expr list * parse_pos)
   (*  function (param1, param2, ...) { <statements> ... } *)
   | Closuredef of (string list * statements * parse_pos)

 and binop_name =
   | Add  | Sub  | Mult | Div
   | BNot | BOr  | BAnd
   | BEq  | BInf | BInfeq | BSup | BSupeq

 and unop_name =
   | UMinus

let position_of_statement = function
  | Fundef (_, _, _, p)     -> p
  | VoidExpr (_, p)         -> p
  | Var (_, _, p)           -> p
  | Let (_, _, p)           -> p
  | Assign (_, _, p)        -> p
  | ArrayAssign(_, _, _, p) -> p
  | While(_, _, p)          -> p
  | If(_, _, _, p)          -> p
  | ForIn(_, _, _, p)       -> p
  | Return(_, p)            -> p

let position_of_expr = function
  | IntConst(_, p)      -> p
  | FloatConst(_, p)    -> p
  | BoolConst(_, p)     -> p
  | EVar(_, p)          -> p
  | BinOp(_, _, _, p)   -> p
  | UnOp(_, _, p)       -> p
  | StringLiteral(_, p) -> p
  | ArrayLiteral(_, p)  -> p
  | ArrayRef(_, _, p)   -> p
  | Funcall(_, _, p)    -> p
  | Closuredef(_, _, p) -> p

let indent_factor = 2

let indent_string level =
  String.make (level * indent_factor) ' '

let rec string_of_program { body = instrs ; _ } : string =
  Utils.string_join ";\n" (List.map (string_of_statement 0) instrs)

and string_of_statements indent instrs =
  Utils.string_join ";\n" (List.map (string_of_statement indent) instrs)

and string_of_statement indent instr =
  match instr with
  | Fundef (fvar, params, body, _) ->
    let params = (Utils.string_join ", " params) in
    let body = (string_of_statements (indent + 1) body)
    in sprintf "%sfunction %s(%s) {\n%s%s}"
             (indent_string indent)
             fvar params body
             (indent_string indent)
  | VoidExpr (expr, _) -> string_of_expr indent expr
  | Var (id, expr, _) ->
    sprintf "%svar %s = %s" (indent_string indent) id (string_of_expr 0 expr)
  | Let (id, expr, _) ->
    sprintf "%slet %s = %s" (indent_string indent) id (string_of_expr 0 expr)
  | Assign(id, expr, _) ->
    sprintf "%s%s = %s;" (indent_string indent) id (string_of_expr 0 expr)
  | ArrayAssign(id, pos, expr, _) ->
    (sprintf "%s%s[%s] = %s" (indent_string indent) id
      (string_of_expr 0 pos) (string_of_expr 0 expr))
  | While(cond, body, _) ->
     (sprintf "%swhile (%s) {\n%s\n%s}"
             (indent_string indent)
             (string_of_expr 0 cond)
             (string_of_statements (indent + 1) body)
             (indent_string indent))
  | ForIn(id, iter, body, _) ->
     sprintf "%sfor (%s in %s) {\n%s\n%s}"
             (indent_string indent) id
             (string_of_expr 0 iter)
             (string_of_statements (indent + 1) body)
             (indent_string indent)
  | If(cond, conseq, alter, _) ->
     sprintf "%sif (%s) {\n%s\n%s} else {\n%s\n%s}"
             (indent_string indent)
             (string_of_expr 0 cond)
             (string_of_statements (indent + 1) conseq)
             (indent_string indent)
             (string_of_statements (indent + 1) alter)
             (indent_string indent)
  | Return(expr, _) ->
    sprintf "%sreturn %s" (indent_string indent) (string_of_expr 0 expr)
  | _ -> failwith "Not yet implemented (string_of_statement)"

and string_of_expr indent expr =
  match expr with
  | IntConst (n, _)      -> sprintf "%s%d" (indent_string indent) n
  | BoolConst (true, _)  -> sprintf "%strue" (indent_string indent)
  | BoolConst (false, _) -> sprintf "%sfalse" (indent_string indent)
  | EVar (id, _)         -> sprintf "%s%s" (indent_string indent) id
  | BinOp (op, expr1, expr2, _) ->
    sprintf "%s(%s %s %s)" (indent_string indent) (string_of_expr 0 expr1)
                            (string_of_binop op) (string_of_expr 0 expr2)
  | Funcall(fexpr, arguments, _) ->
    let arguments = Utils.string_join ", " (List.map (string_of_expr 0) arguments)
    in sprintf "%s%s(%s)"
      (indent_string indent)
      (string_of_expr 0 fexpr)
      arguments
  | _ -> failwith "Not yet implemented (string_of_expr)"


and string_of_binop = function
  | Add    -> "+"
  | Sub    -> "-"
  | Mult   -> "*"
  | Div    -> "/"
  | BNot   -> "!"
  | BOr    -> "||"
  | BAnd   -> "&&"
  | BEq    -> "=="
  | BInf   -> "<"
  | BInfeq -> "<="
  | BSup   -> ">"
  | BSupeq -> ">="
