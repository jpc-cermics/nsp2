function slm=arhnk(sl,ordre,tol)
  // Copyright INRIA

  if type(sl,'short') <> 'linearsys' then
    error("Error: expecting a linear system"),
  end
  if sl(7) <> 'c' then error("Error: system should be continuous"),end
  select nargin
    case 2 then istol=0;
    case 3 then istol=1;
  end
  [a,b,c,d]=abcd(sl);
  x0=sl.X0;
  dom=sl.dom;
  if max(real(spec(a))) > 0 then
    error('unstable system!');return;
  end
  domaine='c'
  wc=lyap(a',-b*b',domaine)
  wo=lyap(a,-c'*c,domaine)
  if istol==0 then
    [t,nn]=equil1(wc,wo);
  else
    [t,nn]=equil1(wc,wo,tol);
  end
  n1=nn(1);
  ti=inv(t);a=t*a*ti;b=t*b;c=c*ti
  wc=t*wc*t';wo=ti'*wo*ti;
  ordre=min(ordre,n1);
  if ordre==n1 then
    a=a(1:n1,1:n1);b=b(1:n1,:);c=c(:,1:n1);
    if nargout==1 then slm=syslin('c',a,b,c,d,0*ones(n1,1)),end
    return,
  end
  sigma=wc(ordre+1,ordre+1)

  r=max(n1-ordre-1,1)

  n=n1
  sel=[1:ordre,ordre+r+1:n];seleq=ordre+1:ordre+r
  b2=b(seleq,:);c2=c(:,seleq);
  u=-c(:,seleq)*pinv(b(seleq,:)')
  a=a(sel,sel);b=b(sel,:);c=c(:,sel);
  wo=wo(sel,sel);wc=wc(sel,sel);

  Gamma=wc*wo;
  Gamma=Gamma-sigma*sigma*eye(size(Gamma))
  a=Gamma\(sigma*sigma*a'+wo*a*wc-sigma*c'*u*b')
  b1=Gamma\(wo*b+sigma*c'*u)
  c=c*wc+sigma*u*b';b=b1;
  d=-sigma*u+d
  //
  [n,vn]=size(a);// a should be square
  [u,m]=schur(a,sort = 'c')
  a=u'*a*u;b=u'*b;c=c*u;
  if m < n then
    t=sylv(a(1:m,1:m),-a(m+1:n,m+1:n),-a(1:m,m+1:n),'c')
    a=a(1:m,1:m)
    b=b(1:m,:)-t*b(m+1:n,:)
    c=c(:,1:m)
  end
  slm=syslin('c',a,b,c,d,0*ones(m,1));
endfunction
