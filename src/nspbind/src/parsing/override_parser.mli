type token =
  | EOF
  | NAME of (string)
  | IGNORE of (string)
  | RULE2 of (string * string )
  | RULE3 of (string * string * string )
  | RULE4 of (string * string * string * string )

val implementation_file :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Override_ast.parsing Override_ast.implementation_file
