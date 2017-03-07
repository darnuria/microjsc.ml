(*
 * compiler.ml
 *
 * Heart of the compiler
 * Compile down the Kernel internal representation of Micro-Javascript (KAst)
 * to Bytecode.
 * Manage lexicals environnements (global and locals)
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

open Bytecode

(** Environnement de compilation pour
    les variables globales. *)
type glob_env = (string list) ref

(** Environnement de compilation
    pour les variables locales. *)
type lex_env = string list

(** Erreurs de compilation. *)
let compile_error msg pos =
  let open Parseutils in
  let open Lexing in
  Printf.printf "Compilation error at line %d column %d\n"
    pos.start_pos.pos_lnum
    pos.start_pos.pos_cnum;
  Printf.printf "  ==> %s\n" msg;
  failwith "Abort compilation."

(** Fonction utilitaire pour l'accès aux
  * environnements globaux et locaux.
*)
let fetch_index xs x =
  let rec fetch i = function
    | []       -> None
    | y :: xs' ->
      if y = x then Some i
      else fetch (i + 1) xs'
  in fetch 0 xs

let rec tostr genv index = match genv with
  | [] -> ""
  | v :: genv' ->
    let str_genv = match genv' with
      | [] -> ""
      | _ -> ", " ^ tostr genv' (index + 1)
    in Printf.sprintf "%s = %d %s" v index str_genv

let string_of_genv global_env = tostr !global_env 0

(** Indice de la variable [var] dans l'environment
    global [genv]. *)
let genv_fetch genv var = fetch_index !genv var

(** Extension de l'environnement global *)
let genv_extend genv var pos =
  let rec extend genv gref ngenv =
    match genv with
    | [] -> (gref, List.rev (var :: ngenv))
    | v :: genv' when v = var ->
      compile_error
        (Printf.sprintf "Global variable '%s' already defined" var)
        pos
    | v :: genv' ->
      extend genv' (gref + 1) (v :: ngenv)
  in let (gref, genv') = extend !genv 0 [] in
  genv := genv';
  gref

(** Indice de la variable [var] dans l'environment
    lexical [lenv]. *)
let lenv_fetch lenv var =
  fetch_index lenv var

let (next_label, reset_label) =
  let label_value = ref 1 in
  ((fun () ->
      let s = Printf.sprintf "L%d" !label_value in
      incr label_value;
      s)
  , (fun () -> label_value := 1))

(** Compilation des expressions. *)
let rec compile_expr prims genv lenv expr =
  let module K = Kast in
  match expr with
  | K.KEVar (var, pos) ->
    compile_var prims genv lenv var pos
  | K.KTrue _ ->
    [ BC_PUSH BC_TRUE ]
  | K.KFalse _ ->
    [ BC_PUSH BC_FALSE ]
  | K.KInt (n, _) ->
    [ BC_PUSH (BC_INT n) ]
  | K.KCall(expr, args, pos) ->
    compile_call prims genv lenv expr args pos
  | K.KClosure (params, body, pos) ->
    compile_closure prims genv lenv params body pos
  | _ ->
    compile_error "Doesn't know (yet) how to compile expression."
      (K.position_of_kexpr expr)

(** Compilation des variables. *)
and compile_var prims genv lenv var pos =
  let open Prim in
  match lenv_fetch lenv var with
  | Some i -> [ (BC_FETCH i) ]
  | None ->
    (match genv_fetch genv var with
    | Some i -> [ (BC_GFETCH i) ]
    | None ->
      match prim_fetch prims var with
      | Some { id = i } -> [ BC_PUSH (BC_PRIM i) ]
      | None -> compile_error (Printf.sprintf "Not in scope : %s" var) pos)

and compile_args prims genv lenv args =
  List.rev args
  |> Utils.mappend (fun arg -> compile_expr prims genv lenv arg)

and compile_call prims genv lenv funexpr args pos =
  compile_args prims genv lenv args
  @ compile_expr prims genv lenv funexpr
  @ [ BC_CALL (List.length args) ]

and compile_closure prims genv lenv params body pos =
  let fun_lbl = next_label ()
  and cont_lbl = next_label ()
  and lenv' = params @ lenv
  in
  [ BC_JUMP cont_lbl
  ; BC_LABEL fun_lbl ]
  @ (compile_statement prims genv lenv' body)
  @ [ BC_RETURN
    ; BC_LABEL (cont_lbl)
    ; BC_PUSH (BC_FUN (fun_lbl)) ]

and compile_statement prims genv lenv stmt =
  let module K = Kast in
  match stmt with
  | K.KVoidExpr (expr, pos) ->
    compile_expr prims genv lenv expr
    @ [ BC_POP ] (* nettoyer la pile *)
  | K.KVar (var, expr, pos) ->
    compile_gvar prims genv lenv var expr pos
  | K.KAssign (var, expr, pos) ->
    compile_assign prims genv lenv var expr pos
  | K.KSeq (stmts, pos) ->
    compile_seq prims genv lenv stmts
  | K.KIf (cond, conseq, alter, pos) ->
    compile_if prims genv lenv cond conseq alter pos
  | K.KReturn (expr, pos) ->
    compile_return prims genv lenv expr pos
  | _ ->
    compile_error "Don't know (yet) how to compile statement."
      (K.position_of_kstatement stmt)

and compile_gvar prims genv lenv gvar expr pos =
  let gref = genv_extend genv gvar pos in
  BC_GALLOC :: (compile_expr prims genv lenv expr)
  @ [ BC_GSTORE (gref) ]

and compile_assign prims genv lenv var expr pos =
  let bc_env = match lenv_fetch lenv var with
    | Some (i) -> [ BC_STORE (i) ]
    | None -> compile_global_env genv var pos
  in (compile_expr prims genv lenv expr)
     @ bc_env

and compile_global_env genv var pos =
  match genv_fetch genv var with
  | Some (i) -> [ BC_GSTORE (i) ]
  | None -> compile_error (Printf.sprintf "Not in scope: %s" var) pos

and compile_if prims genv lenv cond then_stmt else_stmt pos =
  let onfalse = next_label ()
  and contlbl = next_label () in
  (compile_expr prims genv lenv cond)
  @ [ BC_JFALSE (onfalse) ]
  @ (compile_statement prims genv lenv then_stmt)
  @ [ BC_JUMP  (contlbl)
    ; BC_LABEL (onfalse) ]
  @ (compile_statement prims genv lenv else_stmt)
  @ [ BC_LABEL (contlbl) ]

and compile_return prims genv lenv expr pos =
  (compile_expr prims genv lenv expr)
  @ [ BC_RETURN ]

and compile_seq prims genv lenv stmts =
  List.fold_left
    (fun res stmt -> res @ (compile_statement prims genv lenv stmt))
    [] stmts

let compile_prog prims Kast.{ kfilename=_; kbody } =
  reset_label ();
  let genv = ref [] in
  compile_seq prims genv [] kbody
