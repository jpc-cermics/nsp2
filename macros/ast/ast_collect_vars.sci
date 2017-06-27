function rep=ast_collect_vars(ast)
// collect variables found in ast 
// i.e all the nodes of type NAME except optional named arguments.
    
  function H=ast_vars_visit(ast,H)
    select ast.get_op[] 
     case %ast.OPT then
      // Optional values specified by name = val ; we do not rename name
      H = ast_visit_arg(ast,2,ast_vars_visit,H);
      return;
     case %ast.NAME  then
      if ~H.iskey[ast.get_str[]] then 
	H(ast.get_str[])=%t;
      end
    else
      rep =  ast_visit_args(ast,ast_vars_visit,H);
      for i=1:length(rep) do H.merge[rep(i)];  end
    end
  endfunction
    
  rep=ast_visit(ast,ast_vars_visit,hash(0));
endfunction

function ast_collect_vars_test()
  ast = ast_expr('z=f(5,x)+7+sin(4)*3+y;");
  H = ast_collect_vars(ast);
  rep=['z';'f';'x';'y';'sin'];
  if ~sort(rep).equal[sort(H.__keys)] then pause;end 
endfunction


