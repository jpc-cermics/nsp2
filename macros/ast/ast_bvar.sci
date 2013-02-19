function ast=ast_bvar(ast) 
// add calls to numerics where constants (double, boolean) 
// are find in the ast 
  
  function ast1=numerics_bvar(ast)
    ast1=ast_expr('numerics(1)');
    L=ast1.get_args[];
    args=L(2);
    args.set_args[list(ast)];
    ast1.set_args[list(L(1),args)];
  endfunction;
    
  function ast=ast_bvar_insert(ast,H)
  // a visitor 
  //printf("-->ast_bvar_insert\n");
    select ast.get_op[] 
     case %ast.NUMBER then
      ast=numerics_bvar(ast);
     case {%ast.INUMBER32,%ast.INUMBER64,%ast.UNUMBER32,%ast.UNUMBER64}  then
      ast=numerics_bvar(ast);
      return;
    else
      newargs =  ast_visit_args(ast,ast_bvar_insert,H);
      ast.set_args[newargs];
    end
    //printf("<--arg_bvar_insert\n");
  endfunction
  ast=ast_visit(ast,ast_bvar_insert,[]);
endfunction

function ast_bvar_test() 
// test file 
  ast1=ast_bvar(ast_expr('8'));
  ast1=ast_bvar(parse("z=x+3;"));
endfunction
