function slc=contrss(sl,tol)
  // Copyright INRIA
  if nargin <= 1 then tol=sqrt(%eps);end
  if type(sl,'short') <> 'linearsys' then
    error("Error: expecting a linear system\");
  end
  [a,b,c,d]=abcd(sl);x0=sl.X0;dom=sl.dom;dt=sl.dt;
  [nc,u]=contr(a,b,tol*norm([a,b],1))
  u=u(:,1:nc)
  a=u'*a*u;b=u'*b;c=c*u
  slc=linear_system(a,b,c,d,u'*x0,dom = sl.dom,sample = sl.dt);
endfunction
