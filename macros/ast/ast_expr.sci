function ast = ast_expr(str)
// get the ast associated to an expression
  ast=parse(str);
  // we drop the statements and \n
  ast=ast.get_args[](1).get_args[](1).get_args[](1);
endfunction
