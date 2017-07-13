function [y,xf]=flts(u,sl,x0)
  // Copyright INRIA

  if type(u,'short') <> 'm' then
    error("Error: expecting a real or complex matrix as first argument");
  end
  if nargin <= 1 then error("Error: incorrect number of input arguments"),end

  [nu,mu]=size(u);
  if ~or(type(sl,'short')==['r','linearsys']) then
    error("Error: second argument must be a linear ssystem or transfer function");
  end
  select type(sl,'short')
    case 'linearsys' then
     if nargin==2 then x0=sl(6),end
     [nb,mb]=size(sl(3))
     if mb <> nu then
       error("Error: arguments have incompatible dimensions");end
     select sl(7)
       case 'c' then error(94,2),
     end
     np=max(sl.D.degree[]);
     [xf,x]=ltitr(sl(2),sl(3),u(:,1:(mu-np)),x0)
     if type(sl(5),'short')=='m' then
       y=sl(4)*x+sl(5)*u
     else
       y=sl(4)*x+rtitr(sl(5),eye(sl(5)),u)
     end
    case 'r' then
     if nargout > 1 then error('flts: invalid nargout'),end
     num=sl.num;
     den=sl.den;
     [ns,ne]=size(num)
     dom=sl.dom;
     select dom
       case 'c' then error("Error: expecting a discrete system");return;
     end
     if ne <> nu then error("Error: arguments have incompatible dimensions");end
     nden=m2p([],var = den.get_var[]);
     nnum=m2p([],var = den.get_var[]);
     for l=1:1:ns do
       pp=den(l,1);
       for k=2:ne do [pg,uu]=bezout(pp,den(l,k)),pp=pp*uu(1,2),end
       nden(l)=pp
       for k=1:ne do nnum(l,k)=num(l,k)*pdiv(pp,den(l,k)),end,
     end
     for l=1:ns do nm(l)=(nden(l).degree[])-max(nnum(l,:).degree[]),end
     ly=mu+min(nm)
     if nargin==3 then
       [mx,nx]=size(x0);maxdgd=max(nden.degree[])
       if nx < maxdgd then
         error('Error: need at least '+string(maxdgd)+' past values');
         return;
       end
       if mx <> ns+ne then
         error("Error: arguments have incompatible dimensions");end
     end
     y=0*ones(ns,ly);
     for l=1:ns do
       ddl=nden(l).degree[]
       dnl=max(nnum(l,:).degree[]);
       lent=ly-ddl+dnl
       select nargin
         case 2 then
          yl=rtitr(nnum(l,:),nden(l),u(:,1:lent));
          [nn,mm]=size(yl);
          y(l,1:mm)=yl;
          //                    y=y(:,1:lent);
         case 3 then
          up=x0(1:ne,maxdgd-ddl+1:maxdgd);
          yp=x0(ne+l,maxdgd-ddl+1:maxdgd);
          y(l,:)=rtitr(nnum(l,:),nden(l),u(:,1:lent),up,yp);
       end
     end,
     l=size(y,2);
     y=y(:,1:min(mu,l));
    else
      error("Error: expecting a linear system");
      return;
  end
endfunction
