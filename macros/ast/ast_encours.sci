function [ast]=ast_funcall_inline(ast,f)
// expand a funcall 
// (calleval f (args arg1 ... argn))
// using code in f 
// Attention il reste à renommer les variables 
// locales de f et la variable de retour;

  function [ok,ast]=ast_funcall_expand(ast,L)
  // expand a funcall 
  // (calleval f (args arg1 ... argn))
  // L gives the names to be used for arguments
    ok=%t;
    if ~ast.is['CALLEVAL'] then ok=%f;return;end
    rep=list()
    args = ast.get_args[](2);
    if ~args.is['ARGS'] then ok = %f;return;end
    args= args.get_args[];
    for i=1:length(args)
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
  
  // rename variables in tha ast for function f
  [f_ast]=ast_function_rename_vars(f);
  // variable names in new ast 
  [ok,H]= ast_function_vars(f_ast);
  [ok,ast]=ast_funcall_expand(ast,H.in);
  f_ast=f_ast.get_args[](2);
  L= ast.get_args[];
  L.concat[f_ast.get_args[]];
  ast.set_args[L];
endfunction

function y=ast_new_var(str)
// generate a new variable 
  global count;
  if isempty(count) then count=0;end
  y =sprintf('%s%d',str,count);
  count = count+1;
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
  

