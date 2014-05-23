type token =
  | NAME of (string)
  | LPAREN
  | RPAREN
  | EOF

val implementation_file :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lisp_ast.t list
