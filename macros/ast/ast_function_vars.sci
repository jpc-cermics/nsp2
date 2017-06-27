function [ok,H]=ast_function_vars(ast)
// Feb 2011: unfinished since we should 
// consider cases like y=x(5);x=67; 
// where x should be in H.called and H.lhs 
  H=hash(0);
  ok=%t;
  if type(ast,'short')=='pl' then 
    ast=pl2ast(ast);
  end
  if type(ast,'short') <> 'ast' then 
    error("Error: expecting an ast object\n");
    ok=%f;
    return;
  end
  if ~ast.is['FUNCTION'] then ok=%f;return;end
  // get the function name fname, out and in 
  args = ast.get_args[];
  sig= args(1);
  lhs = sig.get_args[](1).get_args[];
  out = m2s([]);
  for i=1:length(lhs) do
    out(i,1)=lhs(i).get_str[];
  end
  call= sig.get_args[](2).get_args[];
  fname= call(1).get_str[];
  in = m2s([]);
  for i=2:length(call) do
    in(i-1,1)=call(i).get_str[];
  end
  body = args(2);
  // function name 
  H=hash(name=fname);
  // arguments of function 
  H.in=in;
  // returned values of function 
  H.out=out;
  // all symbols found 
  H.all= ast_collect_vars(body)
  // lhs of equality arguments 
  H.lhs= ast_collect_mlhs_names(body);
  // x(...) or x. or x{...}
  // removing in out lhs and fname 
  H.called = H.all;
  H.called.delete[H.in];
  H.called.delete[H.out];
  H.called.delete[H.lhs];// .__keys];
  H.called.delete[H.name];
  // remove from called the functions or 
  // macros by searching callable symbols 
  // in the called keys (name and name_m or name_b).
  // We keep in H.funs all the callable functions.
  vals=H.called.__keys;
  H.funs = hash(10);
  for v=vals' do
    if or(exists(v+['','_m','_s','_i'],'callable')) then 
      H.called.delete[v];
      H.funs(v)=%t;
    end
  end
endfunction



