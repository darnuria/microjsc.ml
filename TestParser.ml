(*
 * testParser.ml
 *
 * Little wrapper for testing the parsing.
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

let _ =
  try
    (let lexbuf = Lexing.from_channel Pervasives.stdin in
     let result = Parser.program Lexer.token lexbuf in
     print_string (Ast.string_of_program result);
     print_newline ();
     Pervasives.flush Pervasives.stdout;
     exit 0)
  with Parseutils.Parse_Exception (msg, pos) ->
    Printf.printf "Parse error: %s\n(%s)\n"
      msg
      (Parseutils.string_of_position pos);
    exit 1

