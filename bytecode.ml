(*
 * bytecode.ml
 *
 * Definition of Frame for MVI018 (nativeVM) Bytecode type
 * Define also functions for converting to string.
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

type bcinstr =
  (* global variables *)
  | BC_GALLOC 
 | BC_GFETCH of int
  | BC_GSTORE of int
  (* stack *)
  | BC_PUSH of bcvalue
  | BC_POP
  (* lexical environment *)
  | BC_FETCH of int
  | BC_STORE of int
  (* control *)
  | BC_CALL of int
  | BC_RETURN
  (* jumps *)
  | BC_LABEL of string
  | BC_JUMP of string
  | BC_JFALSE of string

and bcvalue =
  | BC_UNIT
  | BC_TRUE
  | BC_FALSE
  | BC_INT of int
  | BC_PRIM of int (* Primitives *)
  | BC_FUN of string


let rec string_of_bcinstr = function
  | BC_GALLOC     -> "GALLOC"
  | BC_GFETCH (n) -> Printf.sprintf "GFETCH %d" n
  | BC_GSTORE (n) -> Printf.sprintf "GSTORE %d" n
  | BC_PUSH (v)   -> Printf.sprintf "PUSH_%s" (string_of_bcvalue v)
  | BC_POP        -> "POP"
  | BC_FETCH (n)  -> Printf.sprintf "FETCH %d" n
  | BC_STORE (n)  -> Printf.sprintf "STORE %d" n
  | BC_CALL (n)   -> Printf.sprintf "CALL %d" n
  | BC_RETURN     -> "RETURN"
  | BC_LABEL (s)  -> Printf.sprintf "%s:" s
  | BC_JUMP (s)   -> Printf.sprintf "JUMP %s" s
  | BC_JFALSE (s) -> Printf.sprintf "JFALSE %s" s

and string_of_bcvalue = function
  | BC_UNIT     -> "UNIT"
  | BC_TRUE     -> "BOOL true"
  | BC_FALSE    -> "BOOL false"
  | BC_INT (n)  -> Printf.sprintf "INT %d" n
  | BC_PRIM (n) -> Printf.sprintf "PRIM %d" n
  | BC_FUN lbl  -> Printf.sprintf "FUN %s" lbl

let string_of_bytecode bc =
  List.map string_of_bcinstr bc
  |> String.concat "\n"

