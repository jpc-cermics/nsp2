function ast=ast_visit(ast,visitor,H)
// generic function which can be used to walk on an ast
//   
  function rep= ast_visit_args(ast,start,last,visitor,H)
    //printf("-->arg_visit_args\n");
    L= ast.get_args[];
    rep=list();
    for j = start:last
      rep($+1) =ast_visit_internal(L(j),visitor,H);
    end
    //printf("<--arg_visit_args\n");
  endfunction 
      
  function rep=ast_visit_arg(ast, elt,visitor,H)
    //printf("-->arg_visit_arg\n");
    L= ast.get_args[];
    if length(L) < elt then rep=0; return;end 
    ast1 = L(elt);
    rep= ast_visit_internal(ast1,visitor,H);
    //printf("<--arg_visit_arg\n");
  endfunction 

  function ast= ast_visit_internal(ast,visitor,H)
    //printf("-->arg_visit_internal\n");
    ast = visitor(ast,H);
    //printf("<--arg_visit_internal\n");
  endfunction
    
  if nargin <= 2 then H=hash(0);end
  ast= ast_visit_internal(ast,visitor,H)
endfunction



