open Printf

open Parseutils

let _ =
  try
   (let lexbuf = Lexing.from_channel stdin in
      let result = Parser.program Lexer.token lexbuf in
      print_string (Ast.string_of_program result); print_newline(); flush stdout ;
      exit 0)
  with Parse_Exception (msg, pos) ->
       ( printf "Parse error: %s\n(%s)\n" msg (string_of_position pos) ) ;
       exit 1

