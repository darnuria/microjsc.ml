
open Printf
open Lexing

type parse_pos =
  { start_pos: Lexing.position
  ; end_pos:   Lexing.position
  }

exception Parse_Exception of (string * parse_pos)

let make_position startp endp =
  { start_pos = startp; end_pos = endp }

let default_position =
  { start_pos = Lexing.dummy_pos; end_pos = Lexing.dummy_pos }


let string_of_position pos =
  sprintf "%d:%d => %d:%d"
          pos.start_pos.pos_lnum pos.start_pos.pos_cnum
          pos.end_pos.pos_lnum pos.end_pos.pos_lnum

