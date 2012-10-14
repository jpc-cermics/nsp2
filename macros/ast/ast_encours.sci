function [ok,L,ast]=ast_funcall_expand(ast)
// expand a funcall 
// (calleval f (args arg1 ... argn))
  ok=%t;
  if ~ast.is['CALLEVAL'] then ok=%f;return;end
  L=m2s([]);
  rep=list()
  args = ast.get_args[](2);
  if ~args.is['ARGS'] then ok = %f;return;end
  args= args.get_args[];
  for i=1:length(args)
    L(i)=ast_new_var('t');
    ast1=ast_create(%ast.MLHS);
    ast1.set_args[list(ast_create(%ast.NAME,str=L(i)))];
    ast2=ast_create(%ast.EQUAL_OP);
    ast2.set_args[list(ast1,args(i))];
    ast= ast_create(%ast.SEMICOLON_OP);
    ast.set_args[list(ast2)];
    rep(i)=ast;
  end
  ast = ast_create(%ast.STATEMENTS);
  ast.set_args[rep];
endfunction

function [ast]=ast_funcall_inline(ast,f)
// expand a funcall 
// (calleval f (args arg1 ... argn))
// using code in f 
// Attention il reste à renommer les variables 
// locales de f et la variable de retour;
  
  [ok,L,ast]=ast_funcall_expand(ast)
  [a,b]=f.get_args[];
  if size(b,'*') <> 1 then 
    error("Expecting just one returned argument\n");
  end
  if size(a,'*') <> size(L,'*') then 
    error("the function call is not coherent with function def\n");
  end
  H=hash(size(a,'*'));
  for i=1:size(a,'*'); H(a(i))=L(i);end
  f_ast=pl2ast(f);
  f_ast=f_ast.get_args[](2);
  f_ast= ast_rename(f_ast,H);
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

// visitor to get the variables in an ast 
// 
