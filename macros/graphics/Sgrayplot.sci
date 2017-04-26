function []=Sgrayplot(x,y,z, varargopt)
//
//    function similar to grayplot but the function fec is used to smooth the
//    result assuming that the underlying function is linear on
//    a set of triangles built from the grid (here with n1=5, n2=3):
//             _____________
//             | /| /| /| /|
//             |/_|/_|/_|/_|
//             | /| /| /| /|
//             |/_|/_|/_|/_|
//
// This function is just a wrapper around fec
// Author B.Pincon

  if nargin <= 0 then
    // demo
    s_mat = ["t=-%pi:0.1:%pi";
	     "m=sin(t)''*cos(t)";
	     "xset(''colormap'',jetcolormap(64))";
	     "Sgrayplot(t,t,m,colorbar=%t,zminmax=[-1,1])";
	     "xtitle(''Sgrayplot demo f(x,y)=sin(x)*cos(y) on [-pi,pi]x[-pi,pi]'')"]
    printf("%s\n",s_mat);
    execstr(s_mat);
    return
  end
  // some checks
  if ~(type(x,'short')=='m' && isreal(x) &&
    type(y,'short')=='m' && isreal(y) && type(z,'short')=='m' && isreal(z)) then
    error("three first arguments must be real matrices")
  end
  nx = length(x); ny = length(y); [p,q] = size(z)
  if p ~= nx | q ~= ny then
    error("third argument has incompatible dimensions with the two first")
  end
  // build the datas for fec
  [noe_x,noe_y] = ndgrid(x,y)
  nbtri = 2*(p-1)*(q-1)
  num = (1:p*(q-1))'; num(p*(1:q-1)) = []; num1 = num+1
  connect =[(1:nbtri)' , [num   num1   num+p;...
		    num1  num1+p num+p]  ,  zeros(nbtri,1)];
  // call fec
  fec(noe_x,noe_y,connect,z,varargopt(:));
endfunction
