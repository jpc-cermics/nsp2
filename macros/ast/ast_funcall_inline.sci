function [ast,H]=ast_funcall_inline(ast,f)
// ast: contains a simple call to f  ex: ast_expr('f(4)');
// f: can be a function or a function given by its ast.
// the call to f is replaced by inline call.
  
  function [ok,ast]=ast_funcall_expand(ast,L)
  // utility function expand a funcall 
  // (calleval f (args arg1 ... argn))
  // L gives the names to be used for arguments
    ok=%t;
    if ~ast.is['CALLEVAL'] then ok=%f;return;end
    rep=list()
    args = ast.get_args[](2);
    if ~args.is['ARGS'] then ok = %f;return;end
    args= args.get_args[];
    for i=1:length(args) do
      ast1=ast_create(%ast.MLHS);
      ast1.set_args[list(ast_create(%ast.NAME,str=L(i)))];
      ast2=ast_create(%ast.EQUAL_OP);
      ast2.set_args[list(ast1,args(i))];
      ast= ast_create(%ast.SEMICOLON_OP);
      ast.set_args[list(ast2)];
      rep($+1)=ast;
    end
    ast = ast_create(%ast.STATEMENTS);
    ast.set_args[rep];
  endfunction

  // rename variables in the ast for function f
  [f_ast]=ast_function_rename_vars(f);
  // variable names in new ast 
  [ok,H]= ast_function_vars(f_ast);
  // expand the call to f given in ast
  [ok,ast]=ast_funcall_expand(ast,H.in);
  // get the body of the function;
  f_ast=f_ast.get_args[](2);
  L= ast.get_args[];
  L.concat[f_ast.get_args[]];
  ast.set_args[L];
endfunction


function ast_funcall_inline_test()
  expr='f(4)';
  printf('Test: inline a call to '+expr+'\n');
  ast=ast_expr(expr);
  function y=f(x); y=sin(x)+cos(x)+x(1);endfunction;
  ast1=ast_funcall_inline(ast,f);
  ast1.print[];
  printf('\n");
endfunction
  
function ast_funcall_inline_test2()
  expr='f(a,b)';
  printf('Test: inline a call to '+expr+'\n');
  ast=ast_expr(expr);
  function z=f(x,y); 
    [m,n]=size(x);
    [p,q]=size(y);
    if n == p then 
      z=zeros(m,q);
      for i=1:m do 
	for j=1:q do 
	  for k=1:n do
	    z(i,j)= z(i,j)+x(i,k)*y(k,j);
	  end
	end
      end
    end
  endfunction
  ast1=ast_funcall_inline(ast,f);
  ast1.print[];
  printf('\n");
endfunction

