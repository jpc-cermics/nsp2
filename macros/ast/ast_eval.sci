function [rep,H,ast]=ast_eval(ast,H)
// evaluation for ast 
// using astv variables.

  function [str]=ast_suffix(arg)
    str=type(arg.get_value[],'short');
    dims=arg.get_dims[];
    if and(dims==[1,1]) then 
      str = str + "s";
    end
  endfunction
    
  function [rep]=PLUS_OP(v1,v2)
    v=  v1.have_value[] && v2.have_value[];
    rep=astv_create(v1.get_value[] + v2.get_value[], value= v);
  endfunction

  function rep=STAR_OP(v1,v2)
    v=  v1.have_value[] && v2.have_value[];
    rep=astv_create(v1.get_value[] * v2.get_value[], value= v);
  endfunction
  
  function rep=DOTSTAR(v1,v2)
    v=  v1.have_value[] && v2.have_value[];
    rep=astv_create(v1.get_value[] * v2.get_value[], value= v);
  endfunction
  
  function rep=COLCONCAT(v1,v2)
    v=  v1.have_value[] && v2.have_value[];
    rep=astv_create([v1.get_value[];v2.get_value[]],value=v);
  endfunction

  function rep=ROWCONCAT(v1,v2)
    v=  v1.have_value[] && v2.have_value[];
    rep=astv_create([v1.get_value[],v2.get_value[]],value=v);
  endfunction

  function rep=GEQ(v1,v2)
    v=  v1.have_value[] && v2.have_value[];
    rep=astv_create(v1.get_value[] >= v2.get_value[],value=v);
  endfunction

  function str=ast_str_funcall(ast,name)
  // regenerate a fun call with rep replacing 
  // initial arguments 
  // example ast_str_funcall(ast_expr('f(4,5,x,poo=5)'),'g')
    L = ast.get_args[];
    args= L(2);
    args1 = args.get_args[];
    for i=1:length(args1);
      if args1(i).is['OPT'] then 
	L1=args1(i).get_args[];
	args1(i).set_args[list(L1(1),ast_expr(sprintf('rep(%d)',i)))];
      else
	args1(i)= ast_expr(sprintf('rep(%d)',i));
      end
    end
    args.set_args[args1];
    ast.set_args[list(ast_create(%ast.NAME,str=name),args)];
    str=ast.sprint[];
  endfunction
        
  function [y,name,ast1]=ast_is_simple_assign(ast)
  // checks that ast is of kind x=expr.
    if ~ast.is["EQUAL_OP"] then y=%f;return;end
    args= ast.get_args[];
    mlhs=args(1);
    ast1=args(2);
    if ~mlhs.is['MLHS'] then y=%f;return;end;
    args= mlhs.get_args[];
    if length(args) <> 1 then y=%f;return;end;
    if ~args(1).is['NAME'] then y=%f;return;end;
    name=args(1).get_str[];
    y=%t;
  endfunction  

  function y=ast_is_pervasive(str)
    y= ~isempty(find(str==['sin','cos']));
  endfunction

  function y=asteval_sin(varargin)
    if length(varargin)<> 1 then 
      error('Illegal number of arguments in sin\n');
      return;
    end
    arg= varargin(1);
    y = astv_create(sin(arg.get_value[]),value=varargin(1).have_value[]);
  endfunction

  function y=asteval_ones(varargin)
    n = length(varargin)
    if n == 1 then 
      arg= varargin(1);
      y = astv_create( ones(arg.get_value[]),value=arg.have_value[]);
    elseif n== 2 then 
      arg1=varargin(1);arg2=varargin(2);
      y = astv_create( ones(arg1.get_value[],arg2.get_value[]),...
		       value=arg1.have_value[]& arg2.have_value[]);
    else
      error('Illegal number of arguments in ones\n');
      return;
    end
  endfunction

  function y=asteval_rand(varargin)
    n = length(varargin)
    if n == 1 then 
      arg= varargin(1);
      y = astv_create( rand(arg.get_value[]),value=%f);
    elseif n== 2 then 
      arg1=varargin(1);arg2=varargin(2);
      y = astv_create( rand(arg1.get_value[],arg2.get_value[]),...
		       value=%f);
    else
      error('Illegal number of arguments in ones\n');
      return;
    end
  endfunction
  
  function y=asteval_struct(varargopt)
  // The struct function 
  // loop on elements to see if they have a value.
    S=varargopt.__keys;
    V=hash(size(S,'*'));
    ok=%t;
    for i=1:size(S,'*') 
      if ~varargopt(S(i)).have_value[] then 
	ok=%f;break;
      else
	V(S(i))= varargopt(S(i)).get_value[];
      end 
    end
    if ok then 
      y = astv_create(V,value=%t);
    else
      y = astv_create(S,value=%f);
    end
    y.set_args[varargopt];
  endfunction

  function rep=TILDE_OP(v1)
    v=  v1.have_value[];
    rep = astv_create(~ v1.get_value[], value=v);
  endfunction
  
  function [rep,H,ast]= ast_eval_internal(ast,H)
    
    function [rep,H,ast]= ast_eval_args(ast,start,last,H)
      L= ast.get_args[];
      rep=list();
      for j=start:last
	[rep($+1),H,ast1] =ast_eval_internal(L(j),H);
	L(j)=ast1;
      end
      ast.set_args[L];
    endfunction 
    
    function [rep,H]=ast_eval_arg(ast, elt,H)
      L= ast.get_args[];
      if length(L) < elt then rep=0; return;end 
      ast1=L(elt);
      [rep,H]=ast_eval_internal(ast1,H)
    endfunction 

    function [rep,ret]= ast_eval_arg_ret(L,elt,H)
    // similar to ast_eval_arg_ 
    // but add a newline if ar is a comment 
      ast=L(elt);
      rep=ast_eval_internal(ast,H)
      ret=(ast.get_op[] == %ast.COMMENT);
    endfunction
    
    function rep=ast_equalop_mlhs_length(ast)
    // returns the length of a mlhs  in an equal_op 
      L= ast.get_args[];
      if length(L) < 1 then rep=-1;return; end;
      mlhs=L(1);
      if mlhs.get_op[] <> %ast.MLHS then rep=-1;return; end;
      rep=length(mlhs.get_args[]);
    endfunction 
    
    if ( ast.get_op[] > 0 ) then 
      // expression operators
      select  ast.get_arity[] 
       case 0 then  // 0-ary operators */
	rep= ast.get_opname[];
	return;
       case 1 then
	select  ast.get_op[] 
	 case {%ast.COMMA_OP, %ast.SEMICOLON_OP,%ast.RETURN_OP} then
	  [rep,H,ast]=ast_eval_args(ast,1,1,H);
	  return;
	else
	  [arg1,H,ast1] =ast_eval_args(ast,1,1,H);
	  execstr("rep="+ast.get_codename[]+"(arg1);");
	  ast.set_args[list(ast1)];
	  return;
	end
       case 2 then
	L = ast.get_args[];
	[arg1,H,ast1] =ast_eval_internal(L(1),H);
	[arg2,H,ast2] =ast_eval_internal(L(2),H);
	str1 = ast_suffix(arg1);
	str2 = ast_suffix(arg2);
	fname = sprintf("%s_%s_%s",ast.get_codename[],str1,str2);
	str = sprintf("[rep]=%s(arg1,arg2)",fname);
	execstr("[rep]="+ast.get_codename[]+"(arg1,arg2);");
	// new ast 
	ast_args=ast_create(%ast.ARGS,args=list(ast1,ast2));
	ast_f = ast_create(%ast.NAME,str=fname);
	ast=ast_create(%ast.CALLEVAL,args=list(ast_f,ast_args));
	return;
      else
	// n-ary 
	args=list();
	for j = 1:ast.get_arity[]
	  [args(j),H] =ast_eval_arg(ast,j,H);
	end
	execstr("rep="+ast.get_codename[]+"(args);");
	return;
      end
    else 
      // set of commands 
      select ast.get_op[] 
       case %ast.OPT then
	// val=value in a calling list */
	//newpos=ast_eval_arg(ast,1,H);
	[rep,H]=ast_eval_arg(ast,2,H);
       case %ast.EQUAL_OP then
	// affectations A revoir 
	[y,name,ast1]=ast_is_simple_assign(ast);
	if ~y then 
	  error("use only simple =\n");
	  return;
	end
	[rep,H]=ast_eval_internal(ast1,H);
	if type(rep,'short')== 'l' then 
	  error(sprintf("rhs evaluation returns too many values (%d)\n",length(rep)));
	  return;
	end
	if H.iskey[name] then 
	  pause xxx
	else
	  H(name)= rep;
	end
	return;
       case %ast.MLHS   then
	[rep,H]=ast_eval_args(ast,1,ast.get_arity[],H);
	return; 
       case %ast.ARGS  then
	// a sequence of expressions inside () for x()*/
	[rep,H]=ast_eval_args(ast,1,ast.get_arity[],H);
	return;
       case %ast.CELLARGS  then
	// a sequence of expressions inside {} for x{} */
	newpos=pos +  Sciprintf1(indent,"{");
	newpos=ast_eval_args(ast,1,ast.get_arity[],H);
	newpos=newpos + Sciprintf("}");
	rep=newpos;return;
       case %ast.METARGS  then
	// a sequence of expressions inside [] for x[] */
	newpos=pos +  Sciprintf1(indent,"[");
	newpos=ast_eval_args(ast,1,ast.get_arity[],H);
	newpos=newpos + Sciprintf("]");
	rep=newpos;return;
       case %ast.DOTARGS  then
	L=ast.get_args[];
	if length(L) < 1 then rep=newpos;return;end
	ast1=L(1);
	if ast1.get_op[] <> %ast.STRING then rep=newpos;return;end
	newpos=pos + Sciprintf1(0,sprintf(".%s", ast1.get_str[]));
	rep=newpos;return;
       case { %ast.CALLEVAL, %ast.LISTEVAL}  then
	if ast.get_arity[] <> 2 then 
	  error("only simple calls are accepted\n");
	  return;
	end
	// evaluate the arguments 
	[rep,H]=ast_eval_arg(ast,2,H);
	args= ast.get_args[];
	if ~args(1).is['NAME'] then
	  error("expecting a function name\n");
	  return;
	end;
	name=args(1).get_str[];
	if H.iskey[name] then 
	  str=ast_str_funcall(ast,name)
 	  execstr('rep='+str,env=H);
	  return;
	else 
	  ast_is_pervasive(name)  // a revoir 
	  str=ast_str_funcall(ast,'asteval_'+name);
	  execstr('rep='+str);
	end
	return;
       case %ast.FEVAL  then
	pause feval
	newpos =ast_eval_arg(ast,1,H);
	newpos=newpos + Sciprintf("(");
	newpos=ast_eval_args(ast,2,ast.get_arity[],H);
	newpos=newpos + Sciprintf(")");
	rep=newpos;return;
       case %ast.PLIST  then
	// such node should not appear here */
	rep=newpos;return;
       case %ast.COMMENT then
	rep= sprintf("//%s",ast.get_str[]);return;
       case %ast.NAME  then
	if find(ast.get_str[]==['%t','%f','%pi','%e']) then 
	  rep= astv_create(evstr(ast.get_str[]),value=%t);
	else
	  if H.iskey[ast.get_str[]] then 
	    rep = H(ast.get_str[]);
	  else
	    error(sprintf("Error: variable %s is not known\n",ast.get_str[]));
	  end
	end
	return;
       case %ast.OPNAME  then
	rep= pos+Sciprintf1(indent,sprintf("''%s''",ast.get_str[]));return;
       case %ast.NUMBER then
	rep= astv_create(evstr(ast.get_str[]),value=%t);return; // OK 
       case %ast.STRING then
	rep= astv_create(ast.get_str[],value=%t); // OK 
	return;
       case {%ast.INUMBER32,%ast.INUMBER64,%ast.UNUMBER32,%ast.UNUMBER64}  then
	rep=astv_create(evstr(ast.get_str[]),value=%t);
	return;
       case %ast.OBJECT then 
	rep=pos;return;
       case %ast.EMPTYMAT then  
	rep = astv_create([],value=%t);return;
       case %ast.EMPTYCELL then 
	rep=  astv_create({},value=%t);return;
       case %ast.P_MATRIX  then
	[rep,H] =ast_eval_arg(ast,1,H);return;
       case %ast.P_CELL  then
	[rep,H] =ast_eval_arg(ast,1,H);return;
       case { %ast.ROWCONCAT,  %ast.COLCONCAT, %ast.DIAGCONCAT} then
	[arg1,ret] =ast_eval_arg_ret(ast.get_args[],1,H);
	if ret == %t then pause concat1;end 
	op=ast.get_codename[];
	[arg2,ret] =ast_eval_arg_ret(ast.get_args[],2,H);
	execstr("rep="+op+"(arg1,arg2);");
	return;
       case { %ast.CELLROWCONCAT, %ast.CELLCOLCONCAT, %ast.CELLDIAGCONCAT }  then 
	for j=0:( ast.get_arity[]-1)
	  rep =ast_eval_arg(ast,j+1,H);
	  if ( j < ast.get_arity[]-1) then 
	    newpos =ast_eval_opname(ast.get_opname[],0,newpos,H);
	  end
	end
	rep=newpos;return;
       case %ast.WHILE then
	// condition rep =ast_eval_arg(ast,1,H);
	// body 
	[rep,H] =ast_eval_arg(ast,2,H);return;
       case %ast.FUNCTION then
	L=ast.get_args[];
	[rep,H,ast1]= ast_eval_internal(L(2),H);
	ast.set_args[list(L(1),ast1,L(3))];
	return 
       case %ast.FOR then
	rep1 =ast_eval_arg(ast,1,H);
	rep2 =ast_eval_arg(ast,2,H);
	// do body 	
	rep =ast_eval_arg(ast,3,H);
	return;
       case %ast.IF then
	L= ast.get_args[];Ln = length(L);
	ok=-1;
	if modulo(Ln,2)==0 then Last=Ln;else Last=Ln-1;end
	reps=(1:2:Last) == 0;
	for i=1:2:Last
	  rep = ast_eval_internal(L(i),H);
	  reps(i)=  rep.have_value[];
	  if rep.have_value[] && and(rep.get_value[]) then 
	    ok=i;
	  end
	end
	reps=reps(1:2:Last);
	if ok == -1 && and(reps) then 
	  ast = L($);
	  printf("We can select the last else\n",ok);
	elseif ok <> -1 then 
	  printf("We can select branch %d\n",ok);
	  ast = L(ok);
	end
	for j=1:Ln
	  if modulo(j,2)==0 || (modulo(j,2)==1 && j ==Ln) then 
	    [Hj]=ast_eval_internal(L(j),H);
	  end
	end
	rep=list();
	return;
       case %ast.TRYCATCH  then
	// try catch sequence */
	newpos =ast_eval_arg(ast,1,0,H);
	//newpos= ast_eval_key("catch",newpos,posret,H);
	Sciprintf1(1,"\n");newpos= Sciprintf1(posret+2,"");
	newpos =ast_eval_arg(ast,2,0,H);
	if ( ast.get_arity[] == 2 ) then
	  //newpos= ast_eval_key("end",newpos,posret,H);
	else 
	  // newpos= ast_eval_key("finally",newpos,posret,H);
	  Sciprintf("\n");newpos= Sciprintf1(posret+2,"");
	  newpos =ast_eval_arg(ast,3,H);
	  // newpos= ast_eval_key("end",newpos,posret,H);
	end
	rep=newpos;return;
       case %ast.SELECT  then
	// arity N. ast argument is the test other arguments are 
	// the cases 
	for j=0:(ast.get_arity[]-1)
	  if ( j==0) then
	    ast_eval_arg(ast,j+1,H);
	    Sciprintf("\n");newpos= Sciprintf1(posret+2,"");
	  else
	    newpos=ast_eval_arg(ast,j+1,H);
	  end
	end
	// newpos= ast_eval_key("end",newpos,posret);
	rep=newpos;return;
       case %ast.STATEMENTS  then
	[rep,H,ast]= ast_eval_args(ast,1,ast.get_arity[],H)
	return;
       case %ast.STATEMENTS1  then
	[rep,H,ast]= ast_eval_args(ast,1,ast.get_arity[],H)
	return;
       case %ast.PARENTH  then
	[rep,H]=ast_eval_args(ast,1,ast.get_arity[],H);
	return;
       case %ast.CASE  then 
	newpos=ast_eval_arg(ast,1,H);
	Sciprintf1(1,"then\n") ;
	newpos= Sciprintf1(posret+2,"");
	newpos=ast_eval_arg(ast,2,H);
	rep=newpos;return;
       case %ast.LASTCASE  then
	Sciprintf("\n");newpos= Sciprintf1(posret+2,"");
	newpos =ast_eval_arg(ast,1,H);
	rep=newpos;return;
       case %ast.GLOBAL then
	// n-ary global */
	// newpos=Sciprintf1(posret,"global ") ; */
	newpos=ast_eval_args(ast,1,ast.get_arity[],H);
	rep=newpos;return;
       case %ast.CLEAR then
	// n-ary clear */
	newpos=ast_eval_args(ast,1,ast.get_arity[],H);
	rep=newpos;return;
       case %ast.CLEARGLOBAL then
	// n-ary global */
	newpos=ast_eval_args(ast,1,ast.get_arity[],H);
	rep=newpos;return;
       case %ast.PAUSE then
	// can be 0 or 1-ary pause */
	newpos=ast_eval_args(ast,1,ast.get_arity[],H);
	rep=newpos;return;
       case %ast.HELP then
	// 0 or  1-ary help */
	newpos=ast_eval_args(ast,1,ast.get_arity[],H);
	rep=newpos;return;
       case %ast.WHO then
	// 0 or 1-ary who */
	newpos=ast_eval_args(ast,1,ast.get_arity[],H);
	rep=newpos;return;
       case %ast.EXEC then
	// 1-ary exec */
	newpos=ast_eval_args(ast,1,ast.get_arity[],H);
	rep=newpos;return;
       case %ast.APROPOS then
	// 1-ary apropos */
	newpos=ast_eval_args(ast,1,ast.get_arity[],H);
	rep=newpos;return;
       case %ast.CD_COMMAND then
	// 1-ary cd */
	newpos=ast_eval_args(ast,1,ast.get_arity[],H);
	rep=newpos;return;
       case %ast.LS_COMMAND then
	// 1-ary ls */
	newpos=ast_eval_args(ast,1,ast.get_arity[],H);
	rep=newpos;return;
       case %ast.PWD_COMMAND then
	// 1-ary pwd */
	newpos=ast_eval_args(ast,1,ast.get_arity[],H);
	rep=newpos;return;
       case %ast.BREAK then rep= pos+Sciprintf1(indent,"break");return;
       case %ast.PRETURN then  rep= pos+Sciprintf1(indent,"return"); return;
       case %ast.QUIT  then   rep= pos+Sciprintf1(indent,"quit");   return;
       case %ast.NSP_EXIT  then  rep= pos+Sciprintf1(indent,"exit"); return; 
       case %ast.ABORT  then  rep= pos+Sciprintf1(indent,"abort");  return;
       case %ast.CONTINUE  then rep= pos+Sciprintf1(indent,"continue"); return; 
       case %ast.WHAT  then  rep= pos+Sciprintf1(indent,"what");  return;
      else
	Sciprintf("Warning in PlistPrettyPrint :");
	pause xx
	// s=astcode_to_name(ast.get_op[]);
      end
    end
  endfunction
  
  if nargin <= 1; H=hash(0);end 
  [rep,H,ast]=ast_eval_internal(ast,H)
endfunction

function [rep,H,ast]= ast_eval_test()
  function y=f()
    x=%f
    if x then 
      y=6;
    elseif %f then 
      y=5;
    else
      y=7;
    end
  endfunction
  [rep,H,ast]=ast_eval(pl2ast(f));
endfunction

