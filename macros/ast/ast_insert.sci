// work in progress 

function rep=ast_insert(ast,f)
// test function which insert an ast when a call to f is found
  
  function ast=ast_insert_build_exprs(L)
  // build expression to be inserted.
    rep=list();
    ast= ast_create(%ast.COMMENT,str="-->insertion");
    ast1 = ast_create(%ast.RETURN_OP);
    ast1.set_args[list(ast)];
    rep($+1) = ast1;
    for i=1:length(L) do
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
    
  function [ok,rep]=ast_special_funcall(ast,H)
    ok=%t;rep=ast;
    astif =     ast.get_args[](1);
    // check special construction like if 
    if astif.is['IF'] then 
      args = astif.get_args[];
      // do not touch the conditions; XXXXX 
      for i=2:2:length(args) do
	args(i) = ast_inserter(args(i),H);
      end
      astif.set_args[args];
      ast.set_args[list(astif)];
      rep=ast;
    else
      ok=%f;
    end
  endfunction
  
  function ast=ast_inserter(ast,H)
  // a visitor 
  //printf("-->ast_inserter\n");
    select ast.get_op[] 
     case {%ast.STATEMENTS, %ast.STATEMENTS1}  then
      L= ast.get_args[];
      rep=list();
      for j = 1:length(L) do
	Lj = L(j);
	[ok1,rep1]= ast_special_funcall(Lj,H);
	//pause yyy;
	if ok1 then 
	  rep($+1) = rep1;
	else
	  // test if calls to H.name are performed in L(j)
	  fc = ast_collect_funcall(Lj,H.name);
	  if length(fc)<>0 then 
	    // inline the calls 
	    out=m2s([]);
	    for i=1:length(fc) do
	      [astn,Ho]=ast_funcall_inline(fc(i),H.code);
	      out(i)=Ho.out;
	      rep($+1)= astn;
	    end
	    //newast= ast_insert_build_exprs(fc);
	    //rep($+1) = newast;
	    for i=1:length(fc) do
	      Lj = ast_replace_funcall(Lj,fc(i),ast_expr(out(i)));
	    end
	    rep($+1) = ast_visit_internal(Lj,ast_inserter,H);
	  else
	    rep($+1) = ast_visit_internal(Lj,ast_inserter,H);
	  end
	end
      end
      ast.set_args[rep];
      return; 
    else
      newargs =  ast_visit_args(ast,ast_inserter,H);
      ast.set_args[newargs];
    end
    //printf("<--arg_inserter\n");
  endfunction
  H=hash(code=pl2ast(f),name=f.get_name[]);
  rep =ast_visit(ast,ast_inserter,H);
endfunction
  
function ast_insert_test()
  function y=f(x); y=sin(x);endfunction;
  function test()
    y=f(5)+7;
  endfunction
  rep=ast_insert(pl2ast(test),f);
  rep.print[];
  printf('\n');

  function y=f(x); y=sin(x);endfunction;
  function test()
    x=4;
    y=f(5)+f(x);
  endfunction
  rep=ast_insert(pl2ast(test),f);
  rep.print[];
  printf('\n');
  function y=f(x); y=sin(x);endfunction;

  
// XXXX attention cet example est faux les 
// insertions doivent être faites dans 
// chaque branche.
  function test()
    x=4;
    if %t then
      x=8;
      y=f(5)+f(x);
    else
      y=78;
    end
  endfunction
  rep=ast_insert(pl2ast(test),f);
  rep.print[];
  printf('\n');
  
endfunction
