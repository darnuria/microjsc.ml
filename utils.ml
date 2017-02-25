
module StringMap = Map.Make (String)

let rec string_join sep = function
  | []         -> ""
  | [ s ]      -> s
  | s :: strs' -> s ^ sep ^ (string_join sep strs')

let indent_factor = ref 2

let indent_string level = String.make (level * !indent_factor) ' '

let rec mappend (f:'a -> 'b list) (l:'a list) : 'b list =
  List.fold_left (fun res e -> res @ (f e)) [] l

