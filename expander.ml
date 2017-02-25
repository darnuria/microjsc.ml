(*
 * expander.ml
 *
 * Part of Micro-JavaScript in ML compiler
 *
 * In charge of the transformation of `Ast.t` to `Kast.t`.
 *
 * Written for 3I018 Compilation course at University Pierre and Marie Curie
 * Teachers: Frédéric Pechansky and Chen Tong Lieu
 *)

open Printf
open Parseutils
open Lexing
open Ast
open Kast

(** Erreurs de compilation. *)
let expand_error msg pos =
  printf "Expansion error at line %d column %d\n"
    pos.start_pos.pos_lnum
    pos.start_pos.pos_cnum;
  printf "  ==> %s\n" msg;
  failwith "Abort compilation."

let rec expand_statement stmt =
  match stmt with
  | VoidExpr (expr, pos)    -> KVoidExpr (expand_expr expr, pos)
  | Var (name, expr, pos)   -> KVar (name, expand_expr expr, pos)
  | Assign (var, expr, pos) -> KAssign (var, expand_expr expr, pos)
  | Return (expr, pos)      -> KReturn (expand_expr expr, pos)
  | If (cond, thens, elses, pos) ->
    let kThen = KSeq (List.map expand_statement thens, pos) in
    let kElse = KSeq (List.map expand_statement elses, pos) in
    KIf (expand_expr cond, kThen, kElse, pos)
  | Fundef (name, params, body, pos) ->
    let kbody = KSeq (List.map expand_statement body, pos) in
    let kClosure = KClosure (params, kbody, pos) in
    KVar (name, kClosure, pos)
  | _ -> expand_error "Don't know how to expand statement"
           (position_of_statement stmt)

and expand_expr expr =
  match expr with
  | IntConst (n, pos)      -> KInt (n, pos)
  | BoolConst (true, pos)  -> KTrue pos
  | BoolConst (false, pos) -> KFalse pos
  | EVar (var, pos)        -> KEVar (var, pos)
  | BinOp (op, lexpr, rexpr, pos) ->
    expand_binop op (expand_expr lexpr) (expand_expr rexpr) pos
  | Funcall(fexpr, args, pos) ->
    KCall(expand_expr fexpr, List.map expand_expr args, pos)
  | Closuredef (params, body, pos) ->
    KClosure(params, KSeq (List.map expand_statement body, pos), pos)
  | _ ->
    expand_error "Don't know how to expand expression"
      (position_of_expr expr)

and expand_binop op left right pos =
  let primitive = expand_binop_prim op pos, pos in
  let arguments = [ left; right ] in
  KCall (KEVar primitive, arguments, pos)

and expand_binop_prim op pos =
  match op with
  | Add  -> "+"
  | Sub  -> "-"
  | Mult -> "*"
  | Div  -> "/"
  | BEq  -> "=="
  | _    ->
    let msg = sprintf "Don't know (yet) how to expand binary operator: %s"
        (string_of_binop op)
    in expand_error msg pos

let expand_prog { filename=fname; body=stmts } =
  { kfilename = fname
  ; kbody     = (List.map expand_statement stmts)
  }

