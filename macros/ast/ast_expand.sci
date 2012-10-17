function rep=ast_expand(ast)
// expand expression in ast in 
// a sequence of definition (=) and 
// op(args) introducing temporary variables.

  function ast=ast_create_equalop(ast,name);
  // return ast= name = ast;
    ast1=ast_create(%ast.MLHS);
    ast1.set_args[list(ast_create(%ast.NAME,str=name))];
    ast2=ast_create(%ast.EQUAL_OP);
    ast2.set_args[list(ast1,ast)];
    ast= ast_create(%ast.SEMICOLON_OP);
    ast.set_args[list(ast2)];
  endfunction;

  function [ok,name]=ast_is_simple_equalop(ast)
  // checks that ast is of kind x=expr; (; is part of ast).
    name="void";
    if ~ast.is["SEMICOLON_OP"] then ok=%f;return;end
    ast=ast.get_args[](1);
    if ~ast.is["EQUAL_OP"] then ok=%f;return;end
    args= ast.get_args[];
    mlhs=args(1);
    if ~mlhs.is['MLHS'] then ok=%f;return;end;
    args= mlhs.get_args[];
    if length(args) <> 1 then ok=%f;return;end;
    if ~args(1).is['NAME'] then ok=%f;return;end;
    name=args(1).get_str[];
    ok=%t;
  endfunction  
  
  function [rep,newargs]=ast_expand_arg(ast,rep,newargs)
    loc = ast_expand(ast);
    for j=1:length(loc)-1
      rep($+1)=loc(j);
    end
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
    for i=1:length(elts)
      [rep,newargs]=ast_expand_arg(elts(i),rep,newargs);
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
    for i=1:length(args)
      [rep,newargs]=ast_expand_arg(args(i),rep,newargs);
    end
    ast.set_args[newargs];
    rep($+1)= ast;
  end
  // for i=1:length(rep); rep(i).print[];printf('\n'); end  printf('<--\n');
endfunction

function ast_expand_test(ast)
  rep=ast_expand(ast);
  for i=1:length(rep)
    rep(i).print[];printf('\n');
  end
endfunction

