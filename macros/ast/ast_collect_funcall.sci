function rep=ast_collect_funcall(ast,str)
// collect calls to f which are returned in 
// a list. Each element of the list is an ast.
  
  function rep=ast_collect_funcall_visit(ast,str)
  // a visitor 
    rep=list();
    args = ast_visit_args(ast,ast_collect_funcall_visit,str);
    for i=1:length(args) do rep.concat[args(i)];  end
    select ast.get_op[] 
     case %ast.CALLEVAL then
      // Optional values specified by name = val ; we do not rename name
      if  ast.get_args[](1).get_str[] == str then 
	rep($+1)=ast;
      end
    end
  endfunction
  rep =ast_visit(ast,ast_collect_funcall_visit,str);
endfunction

function ast_collect_funcall_test()
  // ex1 
  str=['f(5,6)','f(x)'];
  str(3)=sprintf('g(%s)+%s*3',str(1),str(2));
  ast = ast_expr(str(3));
  rep = ast_collect_funcall(ast,'f');
  for i=1:length(rep) do
    if ~rep(i).equal[ast_expr(str(i))] then pause;end 
  end
  // ex2 
  str=['f(x)','f(5,f(x))','f(y)'];
  str(4)=sprintf('%s + %s',str(2),str(3));
  ast = ast_expr(str(4));
  rep = ast_collect_funcall(ast,'f');
  for i=1:length(rep) do
    if ~rep(i).equal[ast_expr(str(i))] then pause;end 
  end

  function y=f(x); sin(x)+f(1)+[f(2);f(3)];endfunction;
  rep = ast_collect_funcall(pl2ast(f),'f');
  for i=1:length(rep) do
    if ~rep(i).equal[ast_expr(sprintf('f(%d)',i))] then pause;end 
  end

endfunction


