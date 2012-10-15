// work in progress 

function rep=ast_insert(ast,f)
// test function which insert an ast when a call to f is found
  
  function ast=ast_insert_build_exprs(L)
    rep=list();
    ast= ast_create(%ast.COMMENT,str="-->insertion");
    ast1 = ast_create(%ast.RETURN_OP);
    ast1.set_args[list(ast)];
    rep($+1) = ast1;
    for i=1:length(L)
      ast= ast_create(%ast.SEMICOLON_OP);
      ast.set_args[list(L(i))];
      rep($+1)=ast;
    end
    ast= ast_create(%ast.COMMENT,str="<--insertion");
    ast1 = ast_create(%ast.RETURN_OP);
    ast1.set_args[list(ast)];
    rep($+1)=ast1;
    ast = ast_create(%ast.STATEMENTS1);
    ast.set_args[rep];
  endfunction
    
  function ast=ast_inserter(ast,H)
  // a visitor 
  //printf("-->ast_inserter\n");
    select ast.get_op[] 
     case {%ast.STATEMENTS, %ast.STATEMENTS1}  then
      L= ast.get_args[];
      rep=list();
      for j = 1:length(L)
	fc = ast_collect_funcall(L(j),H.name);
	if length(fc)<>0 then 
	  for i=1:length(fc);
	    pause xxx;
	    astn=ast_funcall_inline(fc(i),H.code);
	    rep($+1)= astn;
	  end
	  newast= ast_insert_build_exprs(fc);
	  rep($+1) = newast;
	end
	rep($+1) = ast_visit_internal(L(j),ast_inserter,H);
      end
      ast.set_args[rep];
      return; 
    else
      newargs =  ast_visit_args(ast,1,ast.get_arity[],ast_inserter,H);
      ast.set_args[newargs];
    end
    //printf("<--arg_inserter\n");
  endfunction
  H=hash(code=pl2ast(f),name=f.get_name[]);
  pause xxx;
  rep =ast_visit(ast,ast_inserter,H);
endfunction
  
function ast_insert_test()
  function y=f(x); y=sin(x)+cos(x)+x(1);endfunction;
  function test()
    x=89;
    y=f(5)+7;
    y=7;
    x=6;
    z=78+f(6)+f(7);
    z=8,f(56);
  endfunction
  rep=ast_insert(pl2ast(test),f);
  rep.print[];
  printf('\n');
endfunction
