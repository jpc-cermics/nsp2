
function ast=ast_expand(ast,reset=%t)
// fully expand ast just to obtain binary ops.
  
  function [rep,flag] = ast_expand_internal(ast)
    // expand ast and return a list of ast 
    // when the last element of the list rep($) have a value 
    // i.e can be used as a rhs element of EQUAL_OP then 
    // flag is set to %true.
      
    function [rep,newargs]=ast_expand_to_funcall(ast,fname)
    // ast= op args -> ast = (funcall fname (Args args))
      args = ast.get_args[];
      [rep,newargs]= ast_expand_args(ast,list(),list());
      ast_args=ast_create(%ast.ARGS,args=newargs);
      ast_f = ast_create(%ast.NAME,str=fname);
      astn=ast_create(%ast.CALLEVAL,args=list(ast_f,ast_args));
      rep($+1)= astn;
    endfunction
        
    function [rep,newargs]=ast_expand_default(ast)
      [rep,newargs]= ast_expand_args(ast,list(),list());
      ast.set_args[newargs];
      rep($+1)= ast;
    endfunction;
    
    function [rep,newargs]=ast_expand_args(ast,rep,newargs)
      L= ast.get_args[];
      for j = 1:ast.get_arity[] do
	[rep,newargs]=ast_expand_arg(L(j),rep,newargs)
      end
    endfunction 
    
    function [rep,newargs]=ast_expand_arg(ast,rep,newargs) 
      [loc,flag] = ast_expand_internal(ast)
      for j=1:length(loc)-1 do rep($+1)=loc(j);  end
      // Set of ast for which introducing extra variable is useless
      vals =['INUMBER32','INUMBER64','UNUMBER32','UNUMBER64','NAME','NUMBER','PARENTH'];
      if or(loc($).get_codename[]== vals) then 
	newargs($+1) = loc($);
      elseif flag == %f then 
	// the expanded ast do not return a value
	rep($+1)=loc($);
      else 
	name = ast_new_var('t');
	rep($+1)= ast_create_equalop(loc($),name);
	newargs($+1) = (ast_create(%ast.NAME,str=name));
      end
    endfunction
    
    function ast=ast_create_equalop(ast,name);
    // return ast= (; (= name ast))
      ast1=ast_create(%ast.MLHS,args=list(ast_create(%ast.NAME,str=name)));
      ast2=ast_create(%ast.EQUAL_OP,args=list(ast1,ast));
      ast= ast_create(%ast.RETURN_OP,args=list(ast2));
    endfunction;
       
    // real start of code for  ast_expand_internal 
    flag = %f 
    if ( ast.get_op[] > 0 ) then 
      // expression operators 
      // sans doute voir ce qu'on fait de , ; et \n 
      select ast.get_op[] 
	case {%ast.RETURN_OP, %ast.COMMA_OP, %ast.SEMICOLON_OP, %ast.COMMA_RET_OP, %ast.SEMICOLON_RET_OP} then 
	 flag = %f;
	 L=ast.get_args[];
	 [rep]= ast_expand_internal(L(1))
	 ast.set_args[list(rep($))];
	 rep($)= ast;
	 newargs=list();
	 return;
      else
	flag = %t;
	[rep,newargs]=ast_expand_default(ast); return;
      end
    else 
      // set of commands 
      select ast.get_op[] 
       case %ast.OPT then
	// affectations Attention c'est faux 
	L=ast.get_args[];
	[rep]= ast_expand_internal(L(2))
	ast.set_args[list(L(1),rep($))];
	rep($)= ast;
       case %ast.EQUAL_OP then
	// affectations 
	L=ast.get_args[];
	[rep]= ast_expand_internal(L(2))
	ast.set_args[list(L(1),rep($))];
	rep($)= ast;
       case %ast.MLHS   then
	newargs=list();	rep=list(ast);return;
       case %ast.ARGS  then
	[rep,newargs]=ast_expand_default(ast); return;
       case %ast.CELLARGS  then
	[rep,newargs]=ast_expand_default(ast); return;
       case %ast.METARGS  then
	[rep,newargs]=ast_expand_default(ast); return;
       case %ast.DOTARGS  then
	newargs=list();	rep=list(ast);return;
       case { %ast.CALLEVAL, %ast.LISTEVAL}  then
	// just expand the arguments not the function name
	flag = %t;
	rep = list();
	args = ast.get_args[](2); // arguments (ARGS ....)
	elts = args.get_args[];
	newargs=list();
	for i=1:length(elts) do
	  [rep,newargs]=ast_expand_arg(elts(i),rep,newargs);
	end
	args.set_args[newargs];
	ast.set_args[list(ast.get_args[](1),args)];
	rep($+1)= ast;
	return;
       case %ast.FEVAL  then
	// calling list in function definition
	newargs=list();	rep=list(ast);return;
       case %ast.PLIST  then
	newargs=list();	rep=list(ast);return;
       case %ast.COMMENT then	newargs=list();	rep=list(ast);return;
       case %ast.NAME    then 	flag=%t;newargs=list();	rep=list(ast);return;
       case %ast.OPNAME  then	flag=%t;newargs=list();	rep=list(ast);return;
       case %ast.NUMBER then flag=%t;newargs=list();	rep=list(ast);return;
       case %ast.STRING then flag=%t;newargs=list();	rep=list(ast);return;
       case {%ast.INUMBER32,%ast.INUMBER64,%ast.UNUMBER32,%ast.UNUMBER64}  then
	newargs=list();	flag=%t;rep=list(ast);return;
       case %ast.OBJECT then   newargs=list();	rep=list(ast);return;
       case %ast.EMPTYMAT then flag=%t; newargs=list();	rep=list(ast);return;
       case %ast.EMPTYCELL then flag=%t; newargs=list();	rep=list(ast);return;
       case {%ast.P_MATRIX, %ast.P_CELL} then
	flag=%t;[rep,newargs]=ast_expand_default(ast); return;
       case { %ast.ROWCONCAT,  %ast.COLCONCAT, %ast.DIAGCONCAT} then
	flag=%t;[rep,newargs]=ast_expand_to_funcall(ast,ast.get_codename[]); return;
       case { %ast.CELLROWCONCAT, %ast.CELLCOLCONCAT, %ast.CELLDIAGCONCAT} then 
	flag=%t;[rep,newargs]=ast_expand_to_funcall(ast,ast.get_codename[]); return;
       case %ast.WHILE then
	// do not expand conditions for a start  XXXX 
	L=ast.get_args[];
	[rep]=ast_expand_internal(L(2));
	L(2)=rep(1);
	ast.set_args[L];rep = list(ast);newargs=list();
	return;
       case %ast.FUNCTION then
	// just expand the inside block 
	L=ast.get_args[];
	[rep]=ast_expand_internal(L(2));
	// rep should be here a list of length 1 since we have expanded 
	// statements.
	L(2)=rep(1);
	ast.set_args[L];
	rep = list(ast);
	newargs=list();
	return;
       case %ast.FOR then
	// just expand the inside block 
	L=ast.get_args[];
	[rep]=ast_expand_internal(L(3));
	L(3)=rep(1); // argument are cases which return an ast;
	ast.set_args[L];
	rep = list(ast);
	newargs=list();
	return;
       case %ast.IF then
	// do not expand conditions for a start 
	L=ast.get_args[]; Ln= length(L); R=L;
	for j=1:Ln do
	  if modulo(j,2)==0 || (modulo(j,2)==1 && j ==Ln) then 
	    [rep]=ast_expand_internal(L(j));
	    R(j)=rep(1); // 
	  end
	end
	ast.set_args[R];rep = list(ast);newargs=list();
	return;
       case %ast.TRYCATCH  then
	L=ast.get_args[]
	for j =1:length(L) do
	  [rep,newargs]=ast_expand_default(L(j));
	  L(j)=rep; // argument are cases which return an ast;
	end
	ast.set_args[L];
	rep = ast;
	newargs=list();
	return;
       case %ast.SELECT  then
	// arity N. ast argument is the test other arguments are 
	// the cases 
	L=ast.get_args[]
	for j =2:length(L) do
	  [rep]=ast_expand_internal(L(j));
	  L(j)=rep(1); // argument are cases which return an ast;
	end
	ast.set_args[L];
	rep = list(ast);
	newargs=list();
	return;
       case {%ast.STATEMENTS, %ast.STATEMENTS1}   then
	newargs=list();
	L=ast.get_args[];
	R=list();
	for j =1:length(L) do
	  [rep]=ast_expand_internal(L(j))
	  R.concat[rep]; // argument are cases which return an ast;
	end
	ast.set_args[R];
	rep = list(ast);
	return;
       case %ast.PARENTH  then
	[rep,newargs]=ast_expand_default(ast);
	flag = %t;
	return;
       case %ast.CASE  then 
	// do not expand the test 
	L=ast.get_args[];
	rep =ast_expand_internal(L(2))
	ast.set_args[list(L(1),rep(1))];
	rep =list(ast);
	newargs=list();
       case %ast.LASTCASE  then 
	L=ast.get_args[];
	rep =ast_expand_internal(L(1))
	ast.set_args[rep];
	rep =list(ast);
	newargs=list();
	return;
       case %ast.GLOBAL then [rep,newargs]=ast_expand_default(ast); return 
       case %ast.CLEAR then [rep,newargs]=ast_expand_default(ast); return 
       case %ast.CLEARGLOBAL then [rep,newargs]=ast_expand_default(ast); return;
       case %ast.PAUSE then [rep,newargs]=ast_expand_default(ast); return;
       case %ast.HELP then [rep,newargs]=ast_expand_default(ast); return;
       case %ast.WHO then [rep,newargs]=ast_expand_default(ast); return;
       case %ast.EXEC then [rep,newargs]=ast_expand_default(ast); return;
       case %ast.APROPOS then [rep,newargs]=ast_expand_default(ast); return;
       case %ast.CD_COMMAND then [rep,newargs]=ast_expand_default(ast); return;
       case %ast.LS_COMMAND then [rep,newargs]=ast_expand_default(ast); return;
       case %ast.PWD_COMMAND then [rep,newargs]=ast_expand_default(ast); return;
       case %ast.BREAK then newargs=list();rep=list(ast);return
       case %ast.PRETURN then newargs=list();rep=list(ast);return 
       case %ast.QUIT  then newargs=list();rep=list(ast);return   
       case %ast.NSP_EXIT  then newargs=list();rep=list(ast);return
       case %ast.ABORT  then newargs=list();rep=list(ast);return 
       case %ast.CONTINUE  then newargs=list();rep=list(ast);return
       case %ast.WHAT  then newargs=list();rep=list(ast);return  
      else
	// default behaviour 
	newargs=list();
	rep=list(ast);
      end
    end
  endfunction
  if reset then ast_reset_var();end
  rep = ast_expand_internal(ast)
  for i=1:length(rep) do
    printf("arg %d\n",i);
    rep(i).print[];printf("\n");
  end
  ast=rep;
endfunction


function ast_expand_test() 
  function y=f(x1,x2)
    if %t then 
      x=89+x1*x2;
    else
      x=89+x1/x2;
    end,
    while %t then 
      x=x+8/(x+6);
    end,
    for i=1:6 do
      x=x+8/(x+6);
      return;
      break;
      pwd;
    end
    select rand(4,5) 
     case 5 then    x=x+8/(x+6);
     case 6 then    x=x+8/(x+6);
    else
      x=89+x1/x2;
    end
  endfunction
  ast_expand(pl2ast(f));
endfunction;
