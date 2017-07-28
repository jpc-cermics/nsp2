function [t,siz]=equil1(p,q,tol)
  //
  // Copyright INRIA
  [n,vn]=size(p);
  // t1
  if nargin==2 then
    [u,p,v,np]=svd(p);
  else
    [u,p,v,np]=svd(p,tol=tol);
  end
  p=sqrt(p);
  p=[p(1:np);ones(n-np,1)];
  t1=diag(ones(n,1) ./p)*u';
  //
  // t2
  q=diag(p)*(u'*q*u)*diag(p);// q1=t1**-1'*q*t1**-1
  if nargin==2 then
    [u,sigma1,v,nq]=svd(q(1:np,1:np))
  else
    [u,sigma1,v,nq]=svd(q(1:np,1:np),tol=tol)
  end
  sigma1=sqrt(sigma1(1:nq));
  t2=[u',0*ones(np,n-np);0*ones(n-np,np),eye(n-np,n-np)];
  //
  // t3
  q=t2*q*t2';t3=eye(n,n);
  if np <> n then
    x=diag(ones(nq,1) ./(sigma1 .*sigma1))*q(1:nq,np+1:n)
    t3(1:nq,np+1:n)=-x;
    q=t3'*q*t3;// ici t3 vaut en fait t3**-1
    t3(1:nq,np+1:n)=x;
  end
  //
  // t4
  if nargin==2 then
    [u,q,v,n3]=svd(q(np+1:n,np+1:n));
  else
    [u,q,v,n3]=svd(q(np+1:n,np+1:n),tol=tol);
  end
  t4=[diag(sqrt(sigma1)),0*ones(nq,n-nq);
      0*ones(np-nq,nq),eye(np-nq,np-nq),0*ones(np-nq,n-np);0*ones(n-np,np),u']
  t=t4*t3*t2*t1
  siz=[nq,np-nq,n3]
endfunction
