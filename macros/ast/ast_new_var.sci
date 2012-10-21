function y=ast_new_var(str)
// generate a new variable 
  global count;
  if isempty(count) then count=0;end
  y =sprintf('%s%d',str,count);
  count = count+1;
endfunction

function ast_reset_var()
// generate a new variable 
  global count;
  count=0;
endfunction

