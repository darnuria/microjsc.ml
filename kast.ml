(*
 * kast.ml
 *
 * Kernel abstract syntax tree of the compiler.
 *
 * We reduce a lot of syntax constructions like:
 * Function declarations becomes assigned lambda...
 * the Ast expansion * to Kast is made in expander.ml
 *
 * Part of Micro-JavaScript compiler in ML project
 * at Université Pierre et Marie Curie
 *
 * Copyright 2016 - 2017
 *
 * 3I018 Compilation Course:
 * Teachers:
 *   - Frederic Peschanski
 *   - Lieu Choun Tong
 *   - Chailloux Emmanuel
 *)

module Parse = Parseutils

type kprogram = { kfilename: string; kbody: kstatements }

and kstatements = kstatement list

and kstatement =
  (* <expr>  (drop result) *)
  | KVoidExpr of (kexpr * Parse.parse_pos)
  (* var name = <expr>    (global variable) *)
  | KVar of (string * kexpr * Parse.parse_pos)
  (* sequence *)
  | KSeq of (kstatements * Parse.parse_pos)
  (* <var> = <expr> *)
  | KAssign of (string * kexpr * Parse.parse_pos)
  (* while(<cond>){ <statements> ... } *)
  | KWhile of (kexpr * kstatement * Parse.parse_pos)
  (* if(<cond>) { <consequent> ... } else { <alternative> ... } *)
  | KIf of (kexpr * kstatement * kstatement * Parse.parse_pos)
  (* return <expr>    (function return) *)
  | KReturn of (kexpr * Parse.parse_pos)

and kexpr =
  (* 1, 23, -12, etc. *)
  | KInt of (int * Parse.parse_pos)
  (* 1.2,  -54.42   2.2e-17 etc. *)
  | KFloat of (float * Parse.parse_pos)
  (* true, false *)
  | KTrue of Parse.parse_pos
  | KFalse of Parse.parse_pos
  (* "this is a string"   etc. *)
  | KString of (string * Parse.parse_pos)
  (* variable *)
  | KEVar of (string * Parse.parse_pos)
  (* call *)
  | KCall of (kexpr * kexpr list * Parse.parse_pos)
  (* function (param1, param2, ...) { <statements> ... } *)
  | KClosure of (string list * kstatement * Parse.parse_pos)

let position_of_kstatement = function
  | KVoidExpr (_, p)  -> p
  | KVar (_, _, p)    -> p
  | KSeq (_, p)       -> p
  | KAssign (_, _, p) -> p
  | KWhile(_, _, p)   -> p
  | KIf(_, _, _, p)   -> p
  | KReturn(_, p)     -> p

let position_of_kexpr = function
  | KInt(_, p)        -> p
  | KFloat(_, p)      -> p
  | KTrue (p)         -> p
  | KFalse (p)        -> p
  | KString(_, p)     -> p
  | KEVar(_, p)       -> p
  | KCall(_, _, p)    -> p
  | KClosure(_, _, p) -> p

let rec string_of_kprogram { kbody = instrs ; _ } =
  string_of_kstatements 0 instrs

and string_of_kstatements indent instrs =
  String.concat "\n" (List.map (string_of_kstatement indent) instrs)

and string_of_kstatement indent = function
  | KVoidExpr (expr, _) ->
    Printf.sprintf "%sKVoidExpr[\n%s\n%s]"
      (Utils.indent_string indent)
      (string_of_kexpr (indent + 1) expr)
      (Utils.indent_string indent)
  | KVar (id, expr, _) ->
    Printf.sprintf "%sKVar(%s)[\n%s\n%s]"
      (Utils.indent_string indent)
      id
      (string_of_kexpr (indent + 1) expr)
      (Utils.indent_string indent)
  | KSeq (stmts, _) ->
    Printf.sprintf "%sKSeq[\n%s\n%s]"
      (Utils.indent_string indent)
      (string_of_kstatements (indent + 1) stmts)
      (Utils.indent_string indent)
  | KAssign(id, expr, _) ->
    failwith "Not implemented"
  | KWhile(cond, body, _) ->
    failwith "Not implemented"
  | KIf(cond, conseq, alter, _) ->
    let indent'  = indent + 1 in (* One lvl of indentation *)
    let indent'' = indent + 2 in (* Two lvl of indentation *)
    Printf.sprintf "%sKIf[\n%s\n%s<then>[\n%s\n%s] <else>[\n%s\n%s]\n%s]"
      (Utils.indent_string indent)
      (string_of_kexpr indent' cond)
      (Utils.indent_string indent')
      (string_of_kstatement indent'' conseq)
      (Utils.indent_string indent')
      (string_of_kstatement indent'' alter)
      (Utils.indent_string indent')
      (Utils.indent_string indent)
  | KReturn(expr, _) ->
    failwith "Not Implemented"

and string_of_kexpr indent = function
  | KCall(var, args, _) ->
    Printf.sprintf "%sKCall[\n%s](\n%s)"
      (Utils.indent_string indent)
      (string_of_kexpr (indent + 1) var)
      (List.map (string_of_kexpr (indent + 2)) args
       |> String.concat ",\n")
  | KInt (n, _)   -> Printf.sprintf "%sKInt(%d)" (Utils.indent_string indent) n
  | KTrue _       -> Printf.sprintf "%sKTrue" (Utils.indent_string indent)
  | KFalse _      -> Printf.sprintf "%sKFalse" (Utils.indent_string indent)
  | KEVar (id, _) -> Printf.sprintf "%sKEVar(%s)" (Utils.indent_string indent) id
  | _ -> failwith "Not yet implemented (string_of_kexpr)"

