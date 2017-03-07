(*
 * expander.ml
 *
 * Expand an abstract syntax tree to a kernel abstract syntax tree
 * Perform some reduction such as transforming binary operators to
 * function calls (BinOp -> KCall)
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

open Parseutils
open Lexing

(** Erreurs de compilation. *)
let expand_error msg pos =
  Printf.printf "Expansion error at line %d column %d\n  ==> %s\n"
    pos.start_pos.pos_lnum
    pos.start_pos.pos_cnum
    msg;
  failwith "Abort compilation."

let rec expand_statement stmt =
  let module A = Ast in
  let module K = Kast in
  match stmt with
  | A.VoidExpr (expr, pos)    -> K.KVoidExpr (expand_expr expr, pos)
  | A.Var (name, expr, pos)   -> K.KVar (name, expand_expr expr, pos)
  | A.Assign (var, expr, pos) -> K.KAssign (var, expand_expr expr, pos)
  | A.Return (expr, pos)      -> K.KReturn (expand_expr expr, pos)
  | A.If (cond, thens, elses, pos) ->
    let kThen = K.KSeq (List.map expand_statement thens, pos) in
    let kElse = K.KSeq (List.map expand_statement elses, pos) in
    K.KIf (expand_expr cond, kThen, kElse, pos)
  | A.Fundef (name, params, body, pos) ->
    let kbody = K.KSeq (List.map expand_statement body, pos) in
    let kClosure = K.KClosure (params, kbody, pos) in
    K.KVar (name, kClosure, pos)
  | _ -> A.position_of_statement stmt
         |>  expand_error "Don't know how to expand statement"

and expand_statements =
  (fun stmts -> List.map expand_statement stmts)

and expand_expr expr =
  let module A = Ast in
  let module K = Kast in
  match expr with
  | A.IntConst (n, pos)      -> K.KInt (n, pos)
  | A.BoolConst (true, pos)  -> K.KTrue pos
  | A.BoolConst (false, pos) -> K.KFalse pos
  | A.EVar (var, pos)        -> K.KEVar (var, pos)
  | A.BinOp (op, lexpr, rexpr, pos) ->
    let primitive = (expand_binop_prim op pos, pos) in
    let arguments = [ expand_expr lexpr; expand_expr rexpr ] in
    K.KCall (K.KEVar primitive, arguments, pos)
  | A.Funcall(fexpr, args, pos) ->
    let args = List.map expand_expr args in
    K.KCall(expand_expr fexpr, args, pos)
  | A.Closuredef (params, body, pos) ->
    let kbody = K.KSeq (expand_statements body, pos) in
    K.KClosure(params, kbody, pos)
  | _ -> Ast.position_of_expr expr
         |> expand_error "Don't know how to expand expression"

and expand_binop_prim op pos =
  let module A = Ast in
  match op with
  | A.Add  -> "+"
  | A.Sub  -> "-"
  | A.Mult -> "*"
  | A.Div  -> "/"
  | A.BEq  -> "=="
  | _  ->
    let msg =
      A.string_of_binop op
      |> Printf.sprintf "Don't know (yet) how to expand binary operator: %s"
    in expand_error msg pos

let expand_prog Ast.{ filename=kfilename; body=stmts } =
  let kbody = List.map expand_statement stmts in
  Kast.{ kfilename; kbody }

