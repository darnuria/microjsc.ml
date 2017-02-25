{

  (* Micro-JavaScript lexer use OcamlLex *)
  open Parseutils
  open Parser        (* The type token is defined in parser.mli *)
  exception Eof
}

let identifier = ['a'-'z''A'-'Z'] ['a'-'z''A'-'Z''0'-'9']*

rule token = parse
  | [' ' '\t']   { token lexbuf } (* skip blanks *)
  | "//"[^'\n']* { token lexbuf } (* skip c-style line comments *)
  (* | "\n\n"    { Lexing.new_line lexbuf;
                   Lexing.new_line lexbuf;
                   EOL } *)
  | ['\n' ]    { (Lexing.new_line lexbuf) ; (token lexbuf) }

  | "function"   { FUNCTION }
  | "return"   { RETURN }
  | "true"     { BOOL(true) }
  | "false"    { BOOL(false) }
  | "var"      { VAR }
  | "let"      { LET }
  | "while"    { WHILE }
  | "for"      { FOR }
  | "in"       { IN }
  | "if"       { IF }
  | "else"     { ELSE }

  | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
  | ','               { COMMA }
  | ';'               { SEMICOL }
  | '+'               { PLUS }
  | '-'               { MINUS }
  | '*'               { TIMES }
  | '/'               { DIV }
  | '('               { LPAREN }
  | ')'               { RPAREN }
  | '{'               { LCURLY }
  | '}'               { RCURLY }
  | '['               { LBRACKET }
  | ']'               { RBRACKET }
  | '='               { EQ }
  | "<="              { INFEQ }
  | '<'               { INF }
  | ">="              { SUPEQ }
  | '>'               { SUP }
  | "=="              { EQEQ }
  | "~~~~"            { EOF }
  | identifier as lxm { IDENTIFIER(lxm) }
  | eof               { EOF }
      (* Do not move the '{' OcamlLex expect directly
         an action without accepting a newline. *)
  | _  as lxm {
      raise (Parse_Exception
               (Printf.sprintf "Unexpected character: %c" lxm
               , default_position))
    }

