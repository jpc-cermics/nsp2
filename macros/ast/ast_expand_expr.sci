function rep=ast_expand_expr(ast)
// expand_expr the expression code (a rhs) in ast into 
// a sequence of definition (=) and call to 
// binary operators. 
// ex: rep=ast_expand_expr(ast_expr('x+y*z'));
//
  function ast=ast_create_equalop(ast,name);
  // return ast= (; (= name ast))
    ast1=ast_create(%ast.MLHS,args=list(ast_create(%ast.NAME,str=name)));
    ast2=ast_create(%ast.EQUAL_OP,args=list(ast1,ast));
    ast= ast_create(%ast.SEMICOLON_OP,args=list(ast2));
  endfunction;

  function [ok,name]=ast_is_simple_equalop(ast)
  // checks that ast is of kind x=expr; (; is part of ast).
    name="void";
    if ~ast.is["SEMICOLON_OP"] then ok=%f;return;end
    ast=ast.get_args[](1);
    if ~ast.is["EQUAL_OP"] then ok=%f;return;end
    args= ast.get_args[];
    mlhs=args(1);
    if ~mlhs.is['MLHS'] then ok=%f;return;end
    args= mlhs.get_args[];
    if length(args) <> 1 then ok=%f;return;end
    if ~args(1).is['NAME'] then ok=%f;return;end
    name=args(1).get_str[];
    ok=%t;
  endfunction  
  
  function [rep,newargs]=ast_expand_expr_arg(ast,rep,newargs)
    loc = ast_expand_expr(ast);
    for j=1:length(loc)-1 do rep($+1)=loc(j);  end
    if loc($).is['NAME'] || loc($).is['NUMBER'] then 
      // printf('already a name');
      newargs($+1) = loc($);
    else 
      name = ast_new_var('t');
      rep($+1)= ast_create_equalop(loc($),name);
      newargs($+1) = (ast_create(%ast.NAME,str=name));
    end
  endfunction
  
  select ast.get_op[] 
   case %ast.CALLEVAL then 
    // just expand the arguments not the function name
    rep = list();
    args = ast.get_args[](2); // arguments (ARGS ....)
    elts = args.get_args[];
    newargs=list();
    for i=1:length(elts) do
      [rep,newargs]=ast_expand_expr_arg(elts(i),rep,newargs);
    end
    args.set_args[newargs];
    ast.set_args[list(ast.get_args[](1),args)];
    rep($+1)= ast;
  else
    rep = list();
    newargs=list();
    // just expand operators
    // get args 
    args = ast.get_args[];
    for i=1:length(args) do
      [rep,newargs]=ast_expand_expr_arg(args(i),rep,newargs);
    end
    ast.set_args[newargs];
    rep($+1)= ast;
  end
  // for i=1:length(rep); rep(i).print[];printf('\n'); end  printf('<--\n');
endfunction

function ast_expand_expr_test()
  // 
  ast_reset_var();
  ast=ast_expr('x+3*y');
  rep=ast_expand_expr(ast);
  rep1=rep(1).get_args[](1); // remove the first operator ;
  if ~rep1.equal[ast_expr('t0=3*y')] then pause;end 
  if ~rep(2).equal[ast_expr('x+t0')] then pause;end 
  // 
endfunction

function ast_expand_expr_test1(str)
  ast_reset_var();
  ast=ast_expr(str);
  rep=ast_expand_expr(ast);
  for i=1:length(rep) do rep(i).print[];printf('\n');end
endfunction

