module Ast_types = Ast_types
module Parsetree = Parsetree

let parse_expression lexbuf = Parser.parse_expression Lexer.read lexbuf
