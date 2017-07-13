function [slb,u]=balreal(sl)
  // Copyright INRIA
  if type(sl,'short') <> 'linearsys' then
    error("Error: expecting a linear system");
    return;
  end
  [a,b,c,d]=abcd(sl);
  x0=sl.X0;
  dom=sl.dom;
  if dom=='u' then
    printf("Warning:  assuming time domain is continuous\n");
    dom='c';
  end
  if dom <> 'c' then dom='d',end
  wc=lyap(a',-b*b',dom)
  wo=lyap(a,-c'*c,dom)
  r=chol(wo);x=r*wc*r';[u,s,v]=svd(x);// already in nsp s=diag(s);
  ns=size(s,'*');
  lli=sqrt(sqrt(s));ll=ones(ns,1) ./lli
  ri=inv(r)*v;r=v'*r;
  a=r*a*ri;b=r*b;c=c*ri
  a=diag(ll)*a*diag(lli)
  b=diag(ll)*b
  c=c*diag(lli)
  slb=syslin(dom,a,b,c,d,diag(ll)*r*x0),
  if nargout==2 then u=ri*diag(lli),end
endfunction
