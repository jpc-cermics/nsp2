function rep=ast_check_callfun(ast,str)
// checks if ast contains a call to the function whose 
// name is given by str 
// maybe not so usefull since superseded by ast_collect_funcall 
  
  function rep=ast_checker(ast,str)
  // a visitor 
  //printf("-->ast_renamer\n");
    select ast.get_op[] 
     case %ast.CALLEVAL then
      // Optional values specified by name = val ; we do not rename name
      rep = ast.get_args[](1).get_str[] == str;
      return;
    else
      args = ast_visit_args(ast,ast_checker,str);
      rep = %f 
      for i=1:length(args) do
	if type(args(i),'short')== 'b' && args(i) == %t then 
	  rep = %t;
	  return 
	end
      end
    end
    //printf("<--arg_checker\n");
  endfunction
  rep =ast_visit(ast,ast_checker,str);
endfunction
  
function ast_check_callfun_test() 
  r1=ast_check_callfun(parse("x+f(56+x);y=x+3;"),'f');
  if r1 == %f then pause;end
  r2=ast_check_callfun(parse("x+f(56+x);y=x+3;"),'f2');
  if r2 == %t then pause;end 
endfunction
