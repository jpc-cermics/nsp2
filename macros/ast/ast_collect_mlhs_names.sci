function rep=ast_collect_mlhs_names(ast)
// collect variables found in ast which are 
// in an mlhs position. Note that the returned 
// values of a function are in a MLHS.
  
  function H=ast_mlhs_names_visit(ast,H)
    select ast.get_op[] 
     case %ast.MLHS then
      // Optional values specified by name = val ; we do not rename name
      args = ast.get_args[];
      for i=1:length(args)  do
	if args(i).is['NAME'] then 
	  H(args(i).get_str[])=%t;
	else 
	  args1 = args(i).get_args[](1);
	  if args1.is['NAME'] then 
	    H(args1.get_str[])=%t;
	  end
	end
      end
      return;
    else
      rep =  ast_visit_args(ast,ast_mlhs_names_visit,H);
      for i=1:length(rep) do H.merge[rep(i)];  end
    end
  endfunction
  rep=ast_visit(ast,ast_mlhs_names_visit,hash(0));
endfunction

function ast_collect_mlhs_names_test()
  function [o1,o2]=f(xi); [x(1).o,z(2)(:),w]=f(5,x);endfunction;
  H = ast_collect_mlhs_names(pl2ast(f));
  rep=['z';'o1';'o2';'w';'x'];
  if ~sort(rep).equal[sort(H.__keys)] then pause;end 
endfunction


