function rep=ast_collect_funcall(ast,str)
// collect calls to f which are returned in 
// a list. Each element of the list is an ast.
  
  function rep=ast_collect_funcall_visit(ast,str)
  // a visitor 
    select ast.get_op[] 
     case %ast.CALLEVAL then
      // Optional values specified by name = val ; we do not rename name
      if  ast.get_args[](1).get_str[] == str then 
	rep = list(ast)
      else
	rep = list();
      end
      return;
    else
      rep=list();
      args = ast_visit_args(ast,1,ast.get_arity[],ast_collect_funcall_visit,str);
      for i=1:length(args) ;rep.concat[args(i)];  end
    end
  endfunction
  rep =ast_visit(ast,ast_collect_funcall_visit,str);
endfunction

function ast_collect_funcall_test();
  // test file 
  printf("Test: call of f in ");
  ast = ast_expr('f(5,6)+7+f(4)*3;");
  ast.print[];
  printf("\n");
  printf("Results:\n");
  rep = ast_collect_funcall(ast,'f');
  for i=1:length(rep)
    rep(i).print[];
    printf("\n");
  end
endfunction


