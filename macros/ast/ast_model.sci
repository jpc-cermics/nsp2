
function ast=ast_model(ast,H)
// template function for ast walk.
  
  function ast= ast_model_internal(ast,H)

    function rep= ast_model_args(ast,start,last,H)
      L= ast.get_args[];
      rep=list();
      for j = start:last do
	rep($+1) =ast_model_internal(L(j),H);
      end
    endfunction 

    function rep=ast_model_arg(ast, elt, indent, pos, posret)
      L= ast.get_args[];
      if length(L) < elt then rep=0; return;end 
      ast1 = L(elt);
      rep= ast_model_internal(ast1,H);
    endfunction 
    
    if ( ast.get_op[] > 0 ) then 
      // expression operators
      newargs = ast_model_args(ast,1,ast.get_arity[],H);
      ast.set_args[newargs];
      return;
    else 
      // set of commands 
      select ast.get_op[] 
       case %ast.OPT then
	// Optional values specified by name = val ; we do not model name
	newargs = ast.get_args[];
	newargs(2) = ast_model_arg(ast,2,H);
	ast.set_args[newargs]
	return;
       case %ast.EQUAL_OP then
	newargs = ast_model_args(ast,1,ast.get_arity[],H);
	ast.set_args[newargs];
	return;
       case %ast.MLHS   then
	// left hand side of an equality 
	newargs = ast_model_args(ast,1,ast.get_arity[],H);
	ast.set_args[newargs];
	return;
       case %ast.ARGS  then
	// a sequence of expressions inside () for x() 
	newargs = ast_model_args(ast,1,ast.get_arity[],H);
	ast.set_args[newargs];
	return;
       case %ast.CELLARGS  then
	// a sequence of expressions inside {} for x{} */
	newargs = ast_model_args(ast,1,ast.get_arity[],H);
	ast.set_args[newargs];
	return;
       case %ast.METARGS  then
	// a sequence of expressions inside [] for x[] */
	newargs = ast_model_args(ast,1,ast.get_arity[],H);
	ast.set_args[newargs];
	return;
       case %ast.DOTARGS  then
	// .nom 
	ast = ast;return;
       case { %ast.CALLEVAL, %ast.LISTEVAL}  then
	newargs = ast_model_args(ast,1,ast.get_arity[],H);
	ast.set_args[newargs];
	return;
       case %ast.FEVAL  then
	newargs = ast_model_args(ast,1,ast.get_arity[],H);
	ast.set_args[newargs];
	return;
       case %ast.PLIST  then
	// such node should not appear here */
	ast = ast ;return;
       case %ast.COMMENT then
	ast = ast; return;
       case %ast.NAME  then
	if H.iskey[ast.get_str[]] then 
	  val=H(ast.get_str[]);
	  if type(val,'short')=='s' then 
	    ast.set_str[val];
	  else
	    ast = val;
	  end
	end
	return;
       case %ast.OPNAME  then
	ast = ast; return;
       case %ast.NUMBER then
	ast = ast ;return;
       case %ast.STRING then
	ast = ast ;return;
       case {%ast.INUMBER32,%ast.INUMBER64,%ast.UNUMBER32,%ast.UNUMBER64}  then
	ast = ast ;return;
       case %ast.OBJECT then 
	ast = ast ;return;
       case %ast.EMPTYMAT then  
	ast = ast ;return;
       case %ast.EMPTYCELL then 
	ast = ast ;return;
       case {%ast.P_MATRIX, %ast.P_CELL}  then
	newargs = ast_model_args(ast,1,ast.get_arity[],H);
	ast.set_args[newargs];
	return;
       case { %ast.ROWCONCAT,  %ast.COLCONCAT, %ast.DIAGCONCAT} then
	newargs = ast_model_args(ast,1,ast.get_arity[],H);
	ast.set_args[newargs];
	return;
       case { %ast.CELLROWCONCAT, %ast.CELLCOLCONCAT, %ast.CELLDIAGCONCAT }  then 
	newargs = ast_model_args(ast,1,ast.get_arity[],H);
	ast.set_args[newargs];
	return;
       case %ast.WHILE then
	newargs = ast_model_args(ast,1,ast.get_arity[],H);
	ast.set_args[newargs];
	return;
       case %ast.FUNCTION then
	// Sciprintf("function arity %d\n",ast.get_arity[]); */
	// newpos=ast_print_tag("function");
	newpos= ast_model_arg(ast,1,1,newpos,newpos);
	Sciprintf("\n");newpos= Sciprintf1(posret+2,"");
	newpos =ast_model_arg(ast,2,0,newpos,posret+2);
	if ( ast.get_arity[] == 3 ) then
	  newpos =ast_model_arg(ast,3,0,newpos,posret+2);
	end
	rep=newpos;return;
       case %ast.FOR then
	newargs = ast_model_args(ast,1,ast.get_arity[],H);
	ast.set_args[newargs];
	return;
       case %ast.IF then
	newargs = ast_model_args(ast,1,ast.get_arity[],H);
	ast.set_args[newargs];
	return;
       case %ast.TRYCATCH  then
	newargs = ast_model_args(ast,1,ast.get_arity[],H);
	ast.set_args[newargs];
	return;
       case %ast.SELECT  then
	newargs = ast_model_args(ast,1,ast.get_arity[],H);
	ast.set_args[newargs];
	return;
       case %ast.STATEMENTS  then
	L= ast.get_args[];
	rep=list();
	for j = 1:length(L) do
	  rep($+1) =ast_model_internal(L(j),H);
	end
	ast.set_args[rep];
	return; 
       case %ast.STATEMENTS1  then
	L= ast.get_args[];
	rep=list();
	for j = 1:length(L) do
	  rep($+1) =ast_model_internal(L(j),H);
	end
	ast.set_args[rep];
	return; 
       case %ast.PARENTH  then
	newargs = ast_model_args(ast,1,ast.get_arity[],H);
	ast.set_args[newargs];
	return; 
       case %ast.CASE  then 
	newargs = ast_model_args(ast,1,ast.get_arity[],H);
	ast.set_args[newargs];
	return; 
       case %ast.LASTCASE  then
	newargs = ast_model_args(ast,1,ast.get_arity[],H);
	ast.set_args[newargs];
	return; 
       case %ast.GLOBAL then
	newargs = ast_model_args(ast,1,ast.get_arity[],H);
	ast.set_args[newargs];
	return; 
       case %ast.CLEAR then
	newargs = ast_model_args(ast,1,ast.get_arity[],H);
	ast.set_args[newargs];
	return; 
       case %ast.CLEARGLOBAL then
	newargs = ast_model_args(ast,1,ast.get_arity[],H);
	ast.set_args[newargs];
	return; 
       case %ast.PAUSE then
	// can be 0 or 1-ary pause */
	// newpos=ast_print_tag("pause");Sciprintf(" ");
	newpos = ast_model_args(ast,1,ast.get_arity[],H);
	rep=newpos;return;
       case %ast.HELP then
	// 0 or  1-ary help */
	// newpos=ast_print_tag("help");Sciprintf(" ");
	newpos = ast_model_args(ast,1,ast.get_arity[],H);
	rep=newpos;return;
       case %ast.WHO then
	// 0 or 1-ary who */
	// newpos=ast_print_tag("who");Sciprintf(" ");
	newpos = ast_model_args(ast,1,ast.get_arity[],H);
	rep=newpos;return;
       case %ast.EXEC then
	// 1-ary exec */
	// newpos=ast_print_tag("exec");Sciprintf(" ");
	newpos = ast_model_args(ast,1,ast.get_arity[],H);
	rep=newpos;return;
       case %ast.APROPOS then
	// 1-ary apropos */
	// newpos=ast_print_tag("apropos");Sciprintf(" ");
	newpos = ast_model_args(ast,1,ast.get_arity[],H);
	rep=newpos;return;
       case %ast.CD_COMMAND then
	// 1-ary cd */
	// newpos=ast_print_tag("cd");Sciprintf(" ");
	newpos = ast_model_args(ast,1,ast.get_arity[],H);
	rep=newpos;return;
       case %ast.LS_COMMAND then
	// 1-ary ls */
	// newpos=ast_print_tag("ls");Sciprintf(" ");
	newpos = ast_model_args(ast,1,ast.get_arity[],H);
	rep=newpos;return;
       case %ast.PWD_COMMAND then
	// 1-ary pwd */
	// newpos=ast_print_tag("pwd");Sciprintf(" ");
	newpos = ast_model_args(ast,1,ast.get_arity[],H);
	rep=newpos;return;
       case %ast.BREAK then ast = ast;return;
       case %ast.PRETURN then  ast = ast;return;
       case %ast.QUIT  then   ast = ast;return;
       case %ast.NSP_EXIT  then ast = ast;return;
       case %ast.ABORT  then  ast = ast;return;
       case %ast.CONTINUE  then ast = ast;return;
       case %ast.WHAT  then ast = ast;return;
      else
	// do nothing 
	ast = ast;return;
      end
    end
  endfunction

  if nargin <= 1 then H=hash(0);end
  ast= ast_model_internal(ast,H)
endfunction


  
