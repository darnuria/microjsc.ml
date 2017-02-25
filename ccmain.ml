module P = Printf
open Utils
open Parseutils
open Ast
open Prim
open Kast
open Expander
open Compiler
open Bytecode

let result_string s = P.sprintf
    "\n===========================\n%s\n===========================\n" s

let abort msg err_code =
  P.printf "Now quitting\n  ==> %s\n\nBye bye !\n" msg ;
 exit err_code

let parse_file filename =
  try
    let in_file = Pervasives.open_in filename in
    let lexbuf = Lexing.from_channel in_file in
    let prog = Parser.program Lexer.token lexbuf in
    close_in in_file;
    { prog with filename = filename }
  with Parse_Exception (msg, pos) ->
    (P.printf "Parse error: %s\n(%s)\n" msg (string_of_position pos));
    exit 1

type control_mode =
  | PARSE_ONLY
  | PARSE_AND_EXPAND
  | COMPILE_AND_SHOW_BYTECODE
  | COMPILE_AND_GENERATE_TARGET
  | COMPILE_AND_RUN

let _ =
  P.printf "Microjs compiler v0.0.1\n" ;
  P.printf "-----------------------\n" ;

  let mode = ref COMPILE_AND_GENERATE_TARGET
  and src_filename = ref "" in
  Arg.parse [ ("-parse", (Arg.Unit (fun () -> mode := PARSE_ONLY))
              , "Parse and show parsed program")
            ; ("-expand", (Arg.Unit (fun () -> mode := PARSE_AND_EXPAND))
              , "Parse, expand and show kernel abstract syntax tree")
            ; ("-compile", (Arg.Unit (fun () -> mode := COMPILE_AND_SHOW_BYTECODE))
              , "Compile and show bytecode")
            ; ("-gen", (Arg.Unit (fun () -> mode := COMPILE_AND_GENERATE_TARGET))
              , "Compile and generate target (default mode)")
            ; ("-run", (Arg.Unit (fun () -> mode := COMPILE_AND_RUN))
              , "Compile and run program (debug mode)") ]
    (fun s -> src_filename := s) "Usage:\n compiler [opts] <source_file>\nopts:";

  (if !src_filename = "" then
     abort "No source file..." 0);

  P.printf "[1] Parsing source file: %s ...\n" !src_filename;
  let prog = parse_file !src_filename in
  P.printf "... parsing done.\n";

  if !mode = PARSE_ONLY
  then (P.printf "Parsed program:%s"
          (result_string (string_of_program prog));
        abort "I could compile, you know..." 0);

  P.printf "[2] Expanding ...\n";

  let kprog = expand_prog prog in
  P.printf "... expansion done.\n";

  if !mode = PARSE_AND_EXPAND
  then (P.printf "Kernal Abstract Syntax Tree:%s"
          (result_string (string_of_kprogram kprog));
        abort "I could compile, you know..." 0);

  P.printf "[3] Compiling ...\n" ;

  let target = compile_prog (init_prim_env ()) kprog
  in

  P.printf "... compilation done.\n";

  (if !mode = COMPILE_AND_SHOW_BYTECODE
   then (P.printf "Bytecode:%s" (result_string (string_of_bytecode target));
         abort "I could generate the target, you know..." 0));

  abort "The rest is not implemented yet ..." 0

