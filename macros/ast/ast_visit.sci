function ast=ast_visit(ast,visitor,H)
// generic function which can be used to walk on an ast
//   
  function ast=default_visitor(ast,H)
    for i=1:H do printf(" ");end
    printf("[%s,%d]\n",ast.get_opname[],ast.get_line[]);
    rep= ast_visit_args(ast,visitor,H+1);
  endfunction
    
  function rep= ast_visit_args(ast,visitor,H)
  //printf("-->arg_visit_args\n");
    L= ast.get_args[];
    rep=list();
    for j = 1: length(L) do
      // ast.get_arity[] 
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
    
  if nargin < 3 then H=0;end
  if nargin < 2 then visitor=default_visitor;end 
  ast= ast_visit_internal(ast,visitor,H)
endfunction



