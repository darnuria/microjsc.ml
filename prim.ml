(*
 * prim.ml
 *
 * Provide some functions and types for manipulating
 * the primitives provided by the MVI018 virtual machine (nativeVM)
 * used in this course.
 *
 * Hacking:
 * Adding an operator:
 * - add an entry in init_prim_lst.
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

type prim =
  { name  : string
  ; id    : int
  ; doc   : string
  ; arity : int
  }

(** Environnement de compilation
pour les primitives. *)
type prim_env = prim Utils.StringMap.t

(** Référence de la primitive de
 nom [name] dans l'environnement [prims] des primitives. *)
let prim_fetch primitives name =
  try Some (Utils.StringMap.find name primitives)
  with Not_found -> None

let init_prim_lst =
  [ { name = "+"  ; id = 1  ; doc = "Addition"       ; arity = 2 }
  ; { name = "-"  ; id = 2  ; doc = "Subtraction"    ; arity = 2 }
  ; { name = "*"  ; id = 3  ; doc = "Multiplication" ; arity = 2 }
  ; { name = "/"  ; id = 4  ; doc = "Division"       ; arity = 2 }
  ; { name = "==" ; id = 10 ; doc = "Equality"       ; arity = 2 }
  ]

let build_prim_map primitives =
  List.fold_left
    (fun pmap prim ->
       Utils.StringMap.add prim.name prim pmap)
    Utils.StringMap.empty
    primitives

let init_prim_env () =
  build_prim_map init_prim_lst
