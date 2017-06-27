function [ast]=ast_function_rename_vars(ast)
  H=hash(0);
  if type(ast,'short')=='pl' then 
    ast=pl2ast(ast);
  end
  if type(ast,'short') <> 'ast' then 
    error("Error: expecting an ast object\n");
    return;
  end
  [ok,H]= ast_function_vars(ast);
  if ~ok then error('failed\n');return;end 
  H1=hash(20);
  for name = H.lhs.__keys' do
    H1(name)= ast_new_var('t');
  end
  for name = H.in' do
    H1(name)= ast_new_var('t');
  end
  for name = H.out' do
    H1(name)= ast_new_var('t');
  end
  ast=ast_rename_vars(ast,H1);
endfunction



