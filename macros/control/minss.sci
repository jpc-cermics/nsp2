function Slmin=minss(Sl,tol)
  // Copyright INRIA
  if type(Sl,'short') <> 'linearsys' then
    error("Error: expecting a linear system");
    return;
  end
  select nargin
    case 1 then tol=[]
    case 2 then tol=tol
    else
      error('1 or 2 inputs: sl [,tol]')
  end
  [a,b,c,d]=abcd(Sl);
  //
  if ~isempty(tol) then
    [nc,u1]=contr(a',c',tol)
  else
    if isempty([a;c]) then reltol=0;else reltol=1.E-10*norm([a;c],1);end
    [nc,u1]=contr(a',c',reltol)
  end
  u=u1(:,1:nc)
  c=c*u;a=u'*a*u;b=u'*b,x0=u'*Sl.X0;
  if ~isempty(tol) then
    [no,u2]=contr(a,b,tol)
  else
    if isempty([a,b]) then reltol=0;else reltol=1.E-10*norm([a,b],1);end
    [no,u2]=contr(a,b,reltol)
  end
  u=u2(:,1:no)
  a=u'*a*u;b=u'*b;c=c*u
  Slmin=linear_system(a,b,c,d,u'*Sl.X0,dom = Sl.dom,sample = Sl.dt);
endfunction
