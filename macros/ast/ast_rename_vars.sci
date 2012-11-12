function ast=ast_rename_vars(ast,H) 
// rename variables: H is a hash table 
// which values are strings are expression ast 
// ast1=ast_expr('x+8');
// rep=ast_rename(parse("f(x=56+x);y=x+3;"),hash(x=ast1));
// replace variable x by expression x+8 
  
  function ast=ast_renamer(ast,H)
  // a visitor 
  //printf("-->ast_renamer\n");
    select ast.get_op[] 
     case %ast.OPT then
      // Optional values specified by name = val ; we do not rename name
      newargs = ast.get_args[];
      newargs(2) = ast_visit_arg(ast,2,ast_renamer,H);
      ast.set_args[newargs]
      return;
     case %ast.NAME  then
      // if name is in H we use the contents of H for renaming 
      if H.iskey[ast.get_str[]] then 
	val=H(ast.get_str[]);
	if type(val,'short')=='s' then 
	  ast.set_str[val];
	else
	  ast = val;
	end
      end
    else
      newargs =  ast_visit_args(ast,ast_renamer,H);
      ast.set_args[newargs];
    end
    //printf("<--arg_renamer\n");
  endfunction
  ast=ast_visit(ast,ast_renamer,H);
endfunction

function ast_rename_vars_test() 
// test file 
  printf("Test: x -> y + 8 in z= x+3 \n");
  ast1=ast_expr('y+8');
  rep=ast_rename_vars(parse("z=x+3;"),hash(x=ast1));
  rep.print[];
  printf("\n");
  // x -> y+8 but not in optional arguments
  printf("Test: x -> y + 8 in  f(x=67)+x;\n");
  ast1=ast_expr('y+8');
  rep=ast_rename_vars(parse("f(x=67)+x;"),hash(x=ast1));
  rep.print[];
  printf("\n");
  // a -> (x+8)
  printf("Test: a -> (x + 8) in function y=f(x); y=sin(x)+cos(a*5);endfunction;\n");
  function y=f(x); y=sin(x)+cos(a*5);endfunction;
  ast1=ast_expr('(x+8)');
  ast =pl2ast(f);
  rep=ast_rename_vars(ast,hash(a=ast1));
  rep.print[];
  printf("\n");
  // other example 
  rep=ast_rename_vars(ast,hash(a='foo'));
  rep.print[];
  printf("\n");
  
endfunction
