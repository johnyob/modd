{
open! Core
open Parser
exception Lexer_error of string
}

let upper = ['A' - 'Z']
let lower = ['a' - 'z']
let letter = lower | upper
 
let digit = ['0' - '9']
let space = [' ' '\t']

let sign = '-'?
let int = sign digit+

let id_char = lower | digit | ['_' '\'']
let id = lower id_char*
let uid = upper id_char*

rule read = 
  parse
  (* keywords *) 
  | "fun"                         { FUN }
  | "local"                       { LOCAL }
  | "global"                      { GLOBAL }
  | "let"                         { LET }
  | "in"                          { IN }
  | "match"                       { MATCH }
  | "with"                        { WITH }
  | "ref"                         { REF }
  | "and"                         { AND }
  
  (* reserved operators *)
  | "->"                          { RIGHT_ARROW }
  | "="                           { EQUAL }
  | ":"                           { COLON }
  | "@"                           { AT }
  | "+"                           { PLUS }
  | "-"                           { MINUS }
  | "*"                           { STAR }
  | "!"                           { MARK }
  | ":="                          { WALRUS }
  | "|"                           { BAR }
  | "_"                           { UNDERSCORE }
  | ","                           { COMMA }
  
  (* identifiers *)
  | id                            { IDENT (Lexing.lexeme lexbuf) }
  | uid                           { UIDENT (Lexing.lexeme lexbuf) }

  (* literals *)
  | "()"                          { UNIT }
  | int                           { INT (Int.of_string (Lexing.lexeme lexbuf)) }

  | space+                        { read lexbuf }

  (* braces *)
  | "("                           { LEFT_PAREN }
  | ")"                           { RIGHT_PAREN }

  | eof                           { EOF }
  | _                             { raise (Lexer_error ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }
