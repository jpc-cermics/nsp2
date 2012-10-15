function [rep,H]=ast_eval(ast,H)
// evaluation for ast 
// using astv variables.

  function rep=PLUS_OP(v1,v2)
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

  function y=ast_sin(varargin)
    if length(varargin)<> 1 then 
      error('Illegal number of arguments in sin\n');
      return;
    end
    arg= varargin(1);
    y = astv_create(sin(arg.get_value[]),value=varargin(1).have_value[]);
  endfunction

  function rep=TILDE_OP(v1)
    v=  v1.have_value[];
    rep = astv_create(~ v1.get_value[], value=v);
  endfunction


  
  function [rep,H]= ast_eval_internal(ast,H)
    
    function [rep,H]= ast_eval_args(ast,start,last,H)
      L= ast.get_args[];
      rep=list();
      for j=start:last
	ast1=L(j);
	[rep($+1),H] =ast_eval_internal(ast1,H)
      end
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
    
    function y=Sciprintf(x); y=length(x);printf("%s",x);endfunction;
    function y=Sciprintf1(i,x); y=length(x)+i;x=catenate(smat_create(1,i," "))+x;printf("%s",x);endfunction;
    
    if ( ast.get_op[] > 0 ) then 
      // expression operators
      select  ast.get_arity[] 
       case 0 then  // 0-ary operators */
	rep= ast.get_opname[];
	return;
       case 1 then
	select  ast.get_op[] 
	 case {%ast.COMMA_OP, %ast.SEMICOLON_OP} then
	  [rep,H]=ast_eval_arg(ast,1,H)
	  return;
	 case {%ast.QUOTE_OP, %ast.DOTPRIM} then
	  [rep,H]=ast_eval_arg(ast,1,H)
	  newpos =ast_eval_opname(ast.get_opname[],0,newpos,H);
	  rep= newpos;
	  return;
	 case %ast.RETURN_OP  then 
	  [rep,H]=ast_eval_arg(ast,1,H);return;
	 case %ast.TILDE_OP  then 
	  [rep,H]=ast_eval_arg(ast,1,H);
	  execstr("rep="+ast.get_codename[]+"(arg1);");
	  return;
	else
	  [arg1,H] =ast_eval_arg(ast,1,H);
	  execstr("rep="+ast.get_codename[]+"(arg1);");
	  return;
	end
       case 2 then
	[arg1,H] =ast_eval_arg(ast,1,H);
	[arg2,H] =ast_eval_arg(ast,2,H);
	execstr("rep="+ast.get_codename[]+"(arg1,arg2);");
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
	newpos=ast_eval_arg(ast,1,H);
	newpos=newpos + Sciprintf("=");
	newpos=ast_eval_arg(ast,2,H);
	rep=newpos;return;
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
	  execstr('rep='+name+'(rep(:))',env=H);
	  return;
	else ast_is_pervasive(name) 
	  execstr('rep='+'ast_'+name+'(rep(:))');
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
	// the function description 
	// newpos= ast_eval_arg(ast,1,H);
	// the body 
	[rep,H] =ast_eval_arg(ast,2,H);
	if ( ast.get_arity[] == 3 ) then
	  // ? table d'objects 
	  // ast_eval_arg(ast,3,H);
	end
	return 
       case %ast.FOR then
	rep1 =ast_eval_arg(ast,1,H);
	rep2 =ast_eval_arg(ast,2,H);
	// do body 	
	rep =ast_eval_arg(ast,3,H);
	return;
       case %ast.IF then
	// a sequence of if elseif etc.... */
	for  j=0:2:(ast.get_arity[]-1)
	  if  j == ast.get_arity[]-1  then
	    // we have reached the last else **/
	    //newpos= ast_eval_key("else",newpos,posret,H);
	    Sciprintf("\n");newpos= Sciprintf1(posret+2,"");
	    newpos =ast_eval_arg(ast,j+1,H);
	  else 
	    //newpos= ast_eval_key("elseif",newpos,posret,H);
	    newpos =ast_eval_arg(ast,j+1,H);
	    Sciprintf1(1,"then\n");newpos= Sciprintf1(posret+2,"");
	    newpos =ast_eval_arg(ast,j+2,H);
	  end
	end
	//newpos= ast_eval_key("end",newpos,posret,H);
	rep=newpos;return;
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
	[rep,H]= ast_eval_args(ast,1,ast.get_arity[],H)
	return;
       case %ast.STATEMENTS1  then
	[rep,H]= ast_eval_args(ast,1,ast.get_arity[],H)
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

  [rep,H]=ast_eval_internal(ast,H)
endfunction

