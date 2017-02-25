open Utils

type prim =
  { name  : string;
    id    : int;
    doc   : string;
    arity : int;
  }

(** Environnement de compilation
pour les primitives. *)
type prim_env = prim StringMap.t

(** Référence de la primitive de
 nom [name] dans l'environnement [prims] des primitives. *)
let prim_fetch (prims:prim_env) (name:string) : prim option =
  try Some (StringMap.find name prims)
  with Not_found -> None

let init_prim_lst =
  [ { name = "+"  ; id = 1  ; doc = "Addition"       ; arity = 2 }
  ; { name = "-"  ; id = 2  ; doc = "Subtraction"    ; arity = 2 }
  ; { name = "*"  ; id = 3  ; doc = "Multiplication" ; arity = 2 }
  ; { name = "/"  ; id = 4  ; doc = "Division"       ; arity = 2 }
  ; { name = "==" ; id = 10 ; doc = "Equality"       ; arity = 2 }
  ]

let build_prim_map (l:prim list) : prim_env =
  List.fold_left
    (fun pmap prim ->
      StringMap.add prim.name prim pmap) (StringMap.empty) l

let init_prim_env () =
  build_prim_map init_prim_lst
