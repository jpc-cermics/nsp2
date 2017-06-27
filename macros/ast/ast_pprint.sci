function ast_pprint(ast,indent)
// this function can be used to obtain a pretty print 
// of an ast obtained via 
// parse(expr) or parse_file(file) or pl2ast(f)
// it is a nsp version which is just here to give 
// example of the behaviour of ast.
  
  function rep= ast_pprint_internal(ast,indent,pos,posret)
  //
    CMAX=120;
    if nargin <= 3 then posret=0;end
    if nargin <= 2 then pos=0;end
    if nargin <= 1 then indent=0;end

    
    function rep= ast_pprint_opname(str,indent, pos)
      Sciprintf1(indent,"");
      rep = pos+ Sciprintf(str) + indent;
    endfunction 

    function newpos= ast_print_tag(tag)
      if (pos <> posret ) then 
	Sciprintf("\n");newpos = Sciprintf1(posret,tag);
      else 
	newpos = pos + Sciprintf(tag);
      end
    endfunction
    
    function rep= ast_pprint_args(ast,start,last,indent,pos,posret,sep,breakable, breakstr)
      L= ast.get_args[];
      newpos=pos;
      for j = start:last do
	ast1=L(j);
	newpos =ast_pprint_internal(ast1,indent,newpos,posret);
	if ( j <> last ) then newpos = newpos + Sciprintf(sep); end
	// if we have remaining arguments and  line is too long we insert \n */
	if ( breakable==%t && newpos > CMAX && j <> last ) then
	  // wa are breaking a line */
	  newpos=posret; Sciprintf(breakstr);Sciprintf1(posret,"");
	end
      end
      // Sciprintf("|indent=%d,newpos=%d,posret=%d|>",indent,newpos,posret); */
      rep=newpos;return;
    endfunction 

    function rep=ast_pprint_arg(ast, elt, indent, pos, posret)
    //
      L= ast.get_args[];
      if length(L) < elt then rep=0; return;end 
      ast1 = L(elt);
      rep= ast_pprint_internal(ast1,indent,pos,posret);
    endfunction 

    // similar to ast_pprint_arg_ 
    // but add a newline if ar is a comment 

    function [rep,ret]= ast_pprint_arg_ret(L,elt,indent,pos,posret)
      ast = L(elt);
      newpos = ast_pprint_internal(ast,indent,pos,posret);
      if ast.get_op[] == %ast.COMMENT then
	Sciprintf("\n");Sciprintf1(posret,"");
	newpos = posret;
	ret = %t;
      else 
	ret = %f;
      end
      rep=newpos;
    endfunction

    function rep = ast_pprint_key(key,newpos,posret)
      if ( newpos < posret ) then newpos=Sciprintf1(posret-newpos,"");end
      if ( newpos <> posret ) then
	Sciprintf("\n");newpos= Sciprintf1(posret,"");
      end
      rep = Sciprintf(key);
    endfunction 

    //
    // returns the length of a mlhs  in an equal_op 
    //

    function rep = ast_equalop_mlhs_length(ast)
      L= ast.get_args[];
      if length(L) < 1 then rep = -1;return; end
      mlhs = L(1);
      if mlhs.get_op[] <> %ast.MLHS then rep = -1;return; end
      rep = length(mlhs.get_args[]);
    endfunction 
    
    function y=Sciprintf(x); y=length(x);printf("%s",x);endfunction;
    function y=Sciprintf1(i,x); y=length(x)+i;x=catenate(smat_create(1,i," "))+x;printf("%s",x);endfunction;
    
    if ( ast.get_op[] > 0 ) then 
      // expression operators
      select  ast.get_arity[] 
       case 0 then  // 0-ary operators */
	rep= ast_pprint_opname(ast.get_opname[],indent,pos);
	return;
       case 1 then
	select  ast.get_op[] 
	 case {%ast.COMMA_OP, %ast.SEMICOLON_OP,%ast.COMMA_RET_OP, %ast.SEMICOLON_RET_OP} then
	  newpos =ast_pprint_arg(ast,1,indent,pos,posret);
	  newpos =ast_pprint_opname(ast.get_opname[],0,newpos);
	  rep = newpos;
	  return;
	 case {%ast.QUOTE_OP, %ast.DOTPRIM} then
	  newpos =ast_pprint_arg(ast,1,indent,pos,posret);
	  newpos =ast_pprint_opname(ast.get_opname[],0,newpos);
	  rep= newpos;
	  return;
	 case %ast.RETURN_OP  then 
	  pause xxx
	  ast_pprint_arg(ast,1,indent,pos,posret);
	  Sciprintf("\n");newpos=0;
	  rep=newpos;return;
	 case %ast.TILDE_OP  then 
	  newpos =ast_pprint_opname(ast.get_opname[],indent,pos);
	  newpos =ast_pprint_arg(ast,1,0,newpos,posret);
	  rep=newpos;return;
	else
	  newpos =ast_pprint_opname(ast.get_opname[],indent,pos);
	  newpos =ast_pprint_arg(ast,1,0,newpos,posret);
	  rep=newpos;return;
	end
       case 2 then
	newpos =ast_pprint_arg(ast,1,indent,pos,posret);
	newpos =ast_pprint_opname(ast.get_opname[],1,newpos);
	newpos = newpos +  Sciprintf(" ");
	if ( newpos > CMAX )  then 
	  Sciprintf("\n");newpos= Sciprintf1(posret,"");
	end
	newpos =ast_pprint_arg(ast,2,0,newpos,posret);
	rep = newpos;
      else
	newpos = pos;
	for j = 0:(ast.get_arity[]-1) do
	  ii = (j==0)*indent + (j<>0);
	  newpos =ast_pprint_arg(ast,j+1,ii,  newpos,posret);
	  if ( j <> ast.get_arity[] -1 ) then 
	    newpos =ast_pprint_opname(ast.get_opname[],1,newpos);
	    newpos = newpos + Sciprintf(" ");
	  end
	end
	rep = newpos;
	return;
      end
    else 
      // set of commands 
      select ast.get_op[] 
       case %ast.OPT then
	// val = value in a calling list */
	newpos = ast_pprint_arg(ast,1,indent,pos,posret);
	newpos = newpos + Sciprintf(" = ");
	newpos = ast_pprint_arg(ast,2,0,newpos,posret);
	rep=newpos;return;
       case %ast.EQUAL_OP then
	// affectations */
	newpos = ast_pprint_arg(ast,1,indent,pos,posret);
	if (  ast_equalop_mlhs_length(ast) > 0 )  then 
	  newpos = newpos+ Sciprintf("=");
	end
	// fix new return position after = */
	newpos = ast_pprint_arg(ast,2,0,newpos, newpos);
	rep=newpos;return;
       case %ast.MLHS   then
	// left hand side of an equality 
	// we do not display the left and right bracket 
	//  if arity is one 
	if ( ast.get_arity[] > 1) then str = "["; else str= "";end
	newpos = pos +  Sciprintf1(indent,str);
	newpos = ast_pprint_args(ast,1,ast.get_arity[],0,newpos,newpos,",",%t," ...\n");
	if ( ast.get_arity[] > 1) then newpos = newpos+ Sciprintf("]"); end
	rep=newpos;return;
       case %ast.ARGS  then
	// a sequence of expressions inside () for x()*/
	newpos = pos +  Sciprintf1(indent,"(");
	newpos = ast_pprint_args(ast,1,ast.get_arity[],0,newpos,newpos,",",%t,"\n");
	newpos = newpos + Sciprintf(")");
	rep=newpos;return;
       case %ast.CELLARGS  then
	// a sequence of expressions inside {} for x{} */
	newpos = pos +  Sciprintf1(indent,"{");
	newpos = ast_pprint_args(ast,1,ast.get_arity[],0,newpos,newpos,",",%t,"\n");
	newpos = newpos + Sciprintf("}");
	rep=newpos;return;
       case %ast.METARGS  then
	// a sequence of expressions inside [] for x[] */
	newpos = pos +  Sciprintf1(indent,"[");
	newpos = ast_pprint_args(ast,1,ast.get_arity[],0,newpos,newpos,",",%t,"\n");
	newpos = newpos + Sciprintf("]");
	rep=newpos;return;
       case %ast.DOTARGS  then
	L=ast.get_args[];
	if length(L) < 1 then rep=newpos;return;end
	ast1 = L(1);
	if ast1.get_op[] <> %ast.STRING then rep=newpos;return;end
	newpos = pos + Sciprintf1(0,sprintf(".%s", ast1.get_str[]));
	rep=newpos;return;
       case { %ast.CALLEVAL, %ast.LISTEVAL}  then
	newpos = pos +  Sciprintf1(indent,"");
	newpos = ast_pprint_args(ast,1,ast.get_arity[],0,newpos,newpos,"",%f,"");
	rep=newpos;return;
       case %ast.FEVAL  then
	newpos =ast_pprint_arg(ast,1,indent,pos,posret);
	newpos = newpos + Sciprintf("(");
	newpos = ast_pprint_args(ast,2,ast.get_arity[],0,newpos,newpos,",",%t,"\n");
	newpos = newpos + Sciprintf(")");
	rep=newpos;return;
       case %ast.PLIST  then
	// such node should not appear here */
	rep=newpos;return;
       case %ast.COMMENT then
	rep= pos+Sciprintf1(indent,sprintf("//%s",ast.get_str[]));return;
       case %ast.NAME  then
	rep= pos+Sciprintf1(indent,sprintf("%s",ast.get_str[]));return;
       case %ast.OPNAME  then
	rep= pos+Sciprintf1(indent,sprintf("''%s''",ast.get_str[]));return;
       case %ast.NUMBER then
	rep= pos+  Sciprintf1(indent,sprintf("%s",ast.get_str[]));return;
       case %ast.STRING then
	newpos = pos + Sciprintf1(indent,"");
	newpos = newpos + length((ast.get_str[]));
	print_string_as_read(ast.get_str[]);
	rep=newpos;return;
       case {%ast.INUMBER32,%ast.INUMBER64,%ast.UNUMBER32,%ast.UNUMBER64}  then
	rep= pos+  Sciprintf1(indent,sprintf("%s",ast.get_str[]));
	return;
       case %ast.OBJECT then 
	rep = pos;return;
       case %ast.EMPTYMAT then  rep = pos+Sciprintf1(indent,"[]");return; 
       case %ast.EMPTYCELL then rep= pos+Sciprintf1(indent,"{}");return;
       case %ast.P_MATRIX  then
	newpos = pos + Sciprintf1(indent,"[");
	newpos =ast_pprint_arg(ast,1,0,newpos,posret+1);
	newpos = newpos + Sciprintf("]");
	rep=newpos;return;
       case %ast.P_CELL  then
	newpos = pos + Sciprintf1(indent,"{");
	newpos =ast_pprint_arg(ast,1,0,newpos,posret+1);
	newpos = newpos + Sciprintf("}");
	rep=newpos;return;
       case { %ast.ROWCONCAT,  %ast.COLCONCAT, %ast.DIAGCONCAT} then
	[newpos,ret] =ast_pprint_arg_ret(ast.get_args[],1,indent,pos,posret)
	if ( newpos == 0) then 
	  newpos =ast_pprint_opname(ast.get_opname[],posret,newpos);
	else 
	  newpos =ast_pprint_opname(ast.get_opname[],0,newpos);
	end
	if ( newpos > CMAX ) then 
	  if ( ast.get_op[] == %ast.COLCONCAT ) then Sciprintf("...");end
	  Sciprintf("\n");newpos= Sciprintf1(posret,"");
	end
	[newpos,ret] =ast_pprint_arg_ret(ast.get_args[],2,0,newpos,posret);
	rep=newpos;return;
       case { %ast.CELLROWCONCAT, %ast.CELLCOLCONCAT, %ast.CELLDIAGCONCAT }  then 
	for j = 0:( ast.get_arity[]-1) do
	  if ( j > 0 && newpos > CMAX  ) then 
	    Sciprintf("\n");newpos= Sciprintf1(posret,"");
	  end
	  newpos =ast_pprint_arg(ast,j+1,0,newpos,posret);
	  if ( j < ast.get_arity[]-1) then 
	    newpos =ast_pprint_opname(ast.get_opname[],0,newpos);
	  end
	end
	rep=newpos;return;
       case %ast.WHILE then
	newpos=ast_print_tag("while");
	newpos =ast_pprint_arg(ast,1,1,newpos,posret);
	newpos = newpos + Sciprintf1(1,"do");
	Sciprintf("\n");newpos = Sciprintf1(posret+2,"");
	newpos =ast_pprint_arg(ast,2,0,newpos,posret+2);
	newpos= ast_pprint_key("end",newpos,posret);
	rep=newpos;return;
       case %ast.FUNCTION then
	// Sciprintf("function arity %d\n",ast.get_arity[]); */
	newpos=ast_print_tag("function");
	newpos= ast_pprint_arg(ast,1,1,newpos,newpos);
	Sciprintf("\n");newpos= Sciprintf1(posret+2,"");
	newpos =ast_pprint_arg(ast,2,0,newpos,posret+2);
	if ( ast.get_arity[] == 3 ) then
	  newpos =ast_pprint_arg(ast,3,0,newpos,posret+2);
	end
	newpos= ast_pprint_key("endfunction",newpos,posret);
	rep=newpos;return;
       case %ast.FOR then
	newpos=ast_print_tag("for");
	newpos =ast_pprint_arg(ast,1,1,newpos,posret);
	newpos = newpos + Sciprintf("=") ;
	newpos =ast_pprint_arg(ast,2,0,newpos,newpos);
	Sciprintf1(1,"do\n") ;newpos= Sciprintf1(posret+2,"");
	newpos =ast_pprint_arg(ast,3,0,newpos,posret+2);
	if ( newpos <> posret+2 ) then
	  Sciprintf("\n");newpos= Sciprintf1(posret+2,"");
	end
	rep = Sciprintf("end");
	return;
       case %ast.IF then
	// a sequence of if elseif etc.... */
	newpos=ast_print_tag("if");//printf("pos=%d,posret=%d",pos,posret);
	for  j = 0:2:(ast.get_arity[]-1) do
	  if  j == ast.get_arity[]-1  then
	    // we have reached the last else **/
	    newpos= ast_pprint_key("else",newpos,posret);
	    Sciprintf("\n");newpos= Sciprintf1(posret+2,"");
	    newpos =ast_pprint_arg(ast,j+1,0,newpos,posret+2);
	  else 
	    if ( j <> 0) then
	      newpos= ast_pprint_key("elseif",newpos,posret);
	    end
	    newpos =ast_pprint_arg(ast,j+1,1,newpos+1,newpos+1);
	    Sciprintf1(1,"then\n");newpos= Sciprintf1(posret+2,"");
	    newpos =ast_pprint_arg(ast,j+2,0,newpos,posret+2);
	  end
	end
	newpos= ast_pprint_key("end",newpos,posret);
	rep=newpos;return;
       case %ast.TRYCATCH  then
	// try catch sequence */
	newpos=ast_print_tag("try");Sciprintf1(1,"\n");newpos= Sciprintf1(posret+2,"");
	newpos =ast_pprint_arg(ast,1,0,newpos,posret+2);
	newpos= ast_pprint_key("catch",newpos,posret);
	Sciprintf1(1,"\n");newpos= Sciprintf1(posret+2,"");
	newpos =ast_pprint_arg(ast,2,0,newpos,posret+2);
	if ( ast.get_arity[] == 2 ) then
	  newpos= ast_pprint_key("end",newpos,posret);
	else 
	  newpos= ast_pprint_key("finally",newpos,posret);
	  Sciprintf("\n");newpos= Sciprintf1(posret+2,"");
	  newpos =ast_pprint_arg(ast,3,0,newpos,posret+2);
	  newpos= ast_pprint_key("end",newpos,posret);
	end
	rep=newpos;return;
       case %ast.SELECT  then
	// arity N. ast argument is the test other arguments are 
	// the cases 
	newpos=ast_print_tag("select");
	for j = 0:(ast.get_arity[]-1) do
	  if ( j==0) then
	    ast_pprint_arg(ast,j+1,1,newpos,posret);
	    Sciprintf("\n");newpos= Sciprintf1(posret+2,"");
	  else
	    newpos=ast_pprint_arg(ast,j+1,0,newpos,posret+2);
	  end
	end
	newpos= ast_pprint_key("end",newpos,posret);
	rep=newpos;return;
       case %ast.STATEMENTS  then
	newpos = pos +  Sciprintf1(indent,"");
	newpos= ast_pprint_args(ast,1,ast.get_arity[],0,newpos,posret,"",%t,"\n");
	rep=newpos;return;
       case %ast.STATEMENTS1  then
	newpos = pos +  Sciprintf1(indent,"");
	newpos= ast_pprint_args(ast,1,ast.get_arity[],0,newpos,posret,"",%t,"\n");
	rep=newpos;return;
       case %ast.PARENTH  then
	newpos = pos + Sciprintf1(indent,"(") ;
	newpos = ast_pprint_args(ast,1,ast.get_arity[],0,newpos,newpos,",",%t,"\n");
	newpos = newpos + Sciprintf(")");
	rep=newpos;return;
       case %ast.CASE  then 
	newpos=ast_print_tag("case");
	newpos=ast_pprint_arg(ast,1,1,newpos,newpos+1);
	Sciprintf1(1,"then\n") ;
	newpos= Sciprintf1(posret+2,"");
	newpos=ast_pprint_arg(ast,2,0,newpos,posret+2);
	rep=newpos;return;
       case %ast.LASTCASE  then
	newpos=ast_print_tag("else");
	Sciprintf("\n");newpos= Sciprintf1(posret+2,"");
	newpos =ast_pprint_arg(ast,1,0,newpos,posret+2);
	rep=newpos;return;
       case %ast.GLOBAL then
	// n-ary global */
	newpos=ast_print_tag("global");Sciprintf(" ");
	// newpos = Sciprintf1(posret,"global ") ; */
	newpos = ast_pprint_args(ast,1,ast.get_arity[],0,newpos,newpos,",",%t,"\n");
	rep=newpos;return;
       case %ast.CLEAR then
	// n-ary clear */
	newpos=ast_print_tag("clear");Sciprintf(" ");
	newpos = ast_pprint_args(ast,1,ast.get_arity[],0,newpos,newpos,",",%t,"\n");
	rep=newpos;return;
       case %ast.CLEARGLOBAL then
	// n-ary global */
	newpos=ast_print_tag("clearglobal");Sciprintf(" ");
	newpos = ast_pprint_args(ast,1,ast.get_arity[],0,newpos,newpos,",",%t,"\n");
	rep=newpos;return;
       case %ast.PAUSE then
	// can be 0 or 1-ary pause */
	newpos=ast_print_tag("pause");Sciprintf(" ");
	newpos = ast_pprint_args(ast,1,ast.get_arity[],0,newpos,newpos,",",%t,"\n");
	rep=newpos;return;
       case %ast.HELP then
	// 0 or  1-ary help */
	newpos=ast_print_tag("help");Sciprintf(" ");
	newpos = ast_pprint_args(ast,1,ast.get_arity[],0,newpos,newpos,",",%t,"\n");
	rep=newpos;return;
       case %ast.WHO then
	// 0 or 1-ary who */
	newpos=ast_print_tag("who");Sciprintf(" ");
	newpos = ast_pprint_args(ast,1,ast.get_arity[],0,newpos,newpos,",",%t,"\n");
	rep=newpos;return;
       case %ast.EXEC then
	// 1-ary exec */
	newpos=ast_print_tag("exec");Sciprintf(" ");
	newpos = ast_pprint_args(ast,1,ast.get_arity[],0,newpos,newpos,",",%t,"\n");
	rep=newpos;return;
       case %ast.APROPOS then
	// 1-ary apropos */
	newpos=ast_print_tag("apropos");Sciprintf(" ");
	newpos = ast_pprint_args(ast,1,ast.get_arity[],0,newpos,newpos,",",%t,"\n");
	rep=newpos;return;
       case %ast.CD_COMMAND then
	// 1-ary cd */
	newpos=ast_print_tag("cd");Sciprintf(" ");
	newpos = ast_pprint_args(ast,1,ast.get_arity[],0,newpos,newpos,",",%t,"\n");
	rep=newpos;return;
       case %ast.LS_COMMAND then
	// 1-ary ls */
	newpos=ast_print_tag("ls");Sciprintf(" ");
	newpos = ast_pprint_args(ast,1,ast.get_arity[],0,newpos,newpos,",",%t,"\n");
	rep=newpos;return;
       case %ast.PWD_COMMAND then
	// 1-ary pwd */
	newpos=ast_print_tag("pwd");Sciprintf(" ");
	newpos = ast_pprint_args(ast,1,ast.get_arity[],0,newpos,newpos,",",%t,"\n");
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
    rep=newpos;
  endfunction

  CMAX=120;
  if nargin <= 1 then indent=0;end
  ast_pprint_internal(ast,indent,0,0);
  printf("\n");
  
endfunction
