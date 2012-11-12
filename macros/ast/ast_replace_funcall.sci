function rep=ast_replace_funcall(ast,code,code_new)
// replace code by a code_new
  function ast=ast_replace_funcall_visit(ast,H)
  // a visitor 
    if ast.equal[H.code] then 
      // found a proper ast 
      ast = H.code_new;
    else
      args = ast_visit_args(ast,ast_replace_funcall_visit,H);
      ast.set_args[args];
    end
  endfunction
  H=hash(code=code,code_new=code_new);
  rep =ast_visit(ast,ast_replace_funcall_visit,H);
endfunction

function ast_replace_funcall_test();
  // test file 
  printf("Test: replace f(5,6) by foo");
  ast = ast_expr('f(5,6)');
  astnew=ast_expr('foo');
  function test() y=f(5,6) + sin(f(5,6)+4);endfunction;
  printf("\n");
  printf("Results:\n");
  rep = ast_replace_funcall(pl2ast(test),ast,astnew)
  rep.print[];
  printf('\n');
endfunction
