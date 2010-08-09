function [y,err]=evstr(str, exec_context)
  if nargin < 2 then 
    exec_context = hash_create(10);
  end
  [m,n]=size(str);
  if m*n == 1 then 
    y=[];
    [ok,H]=execstr('y='+str,env=exec_context,errcatch=%t);
    err= ~ok;
    if ok then   y=H.y; else y=[];end 
  else
    A_evstr=zeros_new(m,n);
    err=%t;
    for i=1:m;
      for j=1:n,
	[ok,H]=execstr('%rep='+str(i,j),env=exec_context,errcatch=%t);
	err=err & ok;
	if ok then 
	  A_evstr(i,j)=H.%rep;
	end
      end
    end
    err = ~err;
    y=A_evstr;
  end
endfunction
