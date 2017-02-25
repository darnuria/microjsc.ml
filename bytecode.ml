module P = Printf

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
  | BC_GFETCH (n) -> P.sprintf "GFETCH %d" n
  | BC_GSTORE (n) -> P.sprintf "GSTORE %d" n
  | BC_PUSH (v)   -> P.sprintf "PUSH_%s" (string_of_bcvalue v)
  | BC_POP        -> "POP"
  | BC_FETCH (n)  -> P.sprintf "FETCH %d" n
  | BC_STORE (n)  -> P.sprintf "STORE %d" n
  | BC_CALL (n)   -> P.sprintf "CALL %d" n
  | BC_RETURN     -> "RETURN"
  | BC_LABEL (s)  -> P.sprintf "%s:" s
  | BC_JUMP (s)   -> P.sprintf "JUMP %s" s
  | BC_JFALSE (s) -> P.sprintf "JFALSE %s" s

and string_of_bcvalue = function
  | BC_UNIT     -> "UNIT"
  | BC_TRUE     -> "BOOL true"
  | BC_FALSE    -> "BOOL false"
  | BC_INT (n)  -> P.sprintf "INT %d" n
  | BC_PRIM (n) -> P.sprintf "PRIM %d" n
  | BC_FUN lbl  -> P.sprintf "FUN %s" lbl

let string_of_bytecode bc =
  List.map string_of_bcinstr bc
  |> Utils.string_join "\n"

