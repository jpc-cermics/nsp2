function g=ast_closure(f,H)
  
  function ast= ast_model_internal(ast,H)
    
    function rep= ast_model_args(ast,start,last,H)
      L= ast.get_args[];
      rep=list();
      for j = start:last do
	rep($+1) =ast_model_internal(L(j),H);
      end
    endfunction 

    function rep=ast_model_arg(ast, elt,H)
      L= ast.get_args[];
      if length(L) < elt then rep=0; return;end 
      ast1 = L(elt);
      rep= ast_model_internal(ast1,H);
    endfunction 
    
    if ( ast.get_op[] > 0 ) then 
      // expression operators
      ast = ast;
      return;
    else 
      // set of commands 
      select ast.get_op[] 
       case %ast.FUNCTION then
	ast = ast;
	L= ast.get_args[];
	arg2 = L(2);
	L1 = arg2.get_args[];
	names = H.__keys;
	for i=1:size(names,'*') do
	  val = sprint(H(names(i)),as_read=%t,name="")
	  str = [sprintf('persistent(%s = ... ',names(i));
		 val;
		 ')'];
	  astn=ast_expr(str);
	  astnew= ast_create(%ast.SEMICOLON_OP);
	  astnew.set_args[list(astn)];
	  L1.add_first[astnew];
	end
	arg2.set_args[L1];
	L(2) = arg2;
	ast.set_args[L];
	return;
      else
	// do nothing 
	ast = ast;
	return;
      end
    end
  endfunction

  if nargin <= 1 then H=hash(0);end
  ast= ast_model_internal(pl2ast(f),H);
  S=ast.sprint[];
  execstr(S);
  g=f;
endfunction


  

  
