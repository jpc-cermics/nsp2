function [nh]=h2norm(g,tol)
//
//                 /+00
//     2           |          *
//  |g| =1/(2*%pi).|trace[g(jw).g(jw)]dw
//     2           |
//                 /-00
// Copyright INRIA
  if type(g,'short')=='m',if norm(g)==0,nh=0,return,end,end,

  if nargin==1 then tol=1000*%eps,end
  g1=g(1);
  if g1(1)=='linearsys' then
    if norm(g(5))>0 then error('Error: non zero D'),end
    sp=spec(g(2)),
    if max(real(sp))>=-tol then
      error('Error: unstable system!'),
    end,
    w=obs_gram(g(2),g(4),'c'),
    nh=abs(sqrt(sum(diag(g(3)'*w*g(3))))),return,
  else,
    num=g(2),den=g(3),
    s=poly(0,(den.get_var[])),
    [t1,t2]=size(num),
    for i=1:t1,
      for j=1:t2,
	n=num(i,j),d=den(i,j),
	if coeff(n)==0 then 
	  nh(i,j)=0,
	else
	  if n.degree[]>=d.degree[] then
	    error('Error: improper system'),
	  end
	  pol=roots(d),
	  if max(real(pol))>-tol then
	    error('Error: unstable system!'),end,
	    nt=horner(n,-s),dt=horner(d,-s),
	    nh(i,j)=residu(n*nt,d,dt),
	end,
      end
    end
    nh=sqrt(sum(nh)),return,
  end
endfunction
