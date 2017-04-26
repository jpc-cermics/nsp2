function []=Sfgrayplot(x, y, f, varargopt)
//
// Like grayplot but the function fec is used to smooth the
// result assuming that the underlying function is linear on
// a set of triangles built from the grid (here with n1=5, n2=3):
//             _____________
//             | /| /| /| /|
//             |/_|/_|/_|/_|
//             | /| /| /| /|
//             |/_|/_|/_|/_|
//
// Author B.Pincon

  if nargin <= 0 then
    s_mat=[
	'function [z]=Surf(x,y); z=x.^3+y;endfunction';
	'xset(""colormap"",jetcolormap(64))';
	'Sfgrayplot(-1:0.1:1,-1:0.1:1,Surf,colorbar=%t);';
	'xtitle(''Sfgrayplot demo: f(x,y)=x^3+y'')'];
    printf("%s\n",s_mat);
    execstr(s_mat);
    return
  end

  // some checks
  if ~(type(x,'short')=='m' && isreal(x) &&
    type(y,'short')=='m' && isreal(y)) then
    error("two first arguments must be real matrices")
  end
  if type(f,'short') ~= 'pl' then
    error("third argument must be a function");
  end

  p = length(x); q = length(y);

  // build the datas for fec
  z = feval(x,y,f);
  [noe_x,noe_y] = ndgrid(x,y)
  nbtri = 2*(p-1)*(q-1)
  num = (1:p*(q-1))'; num(p*(1:q-1)) = []; num1 = num+1
  connect =[(1:nbtri)' , [num   num1   num+p;...
		    num1  num1+p num+p]  ,  zeros(nbtri,1)]
  fec(noe_x,noe_y,connect,z, varargopt(:));
endfunction
