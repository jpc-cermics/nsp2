function [n,nc,u,sl,v]=st_ility(sl,tol)
  //stabilizability test
  // Copyright INRIA

  if type(sl,'short')=='m' then
    // means st_ility(A,B);
    sl=syslin('c',sl,tol,[]);
  end
  a=sl.A,b=sl.B,c=sl.C,d=sl.D,x0=sl.X0,dom=sl.dom,sample=sl.dt;
  if dom=='u' then
    dom='c';printf('st_ility: time domain not given => sl assumed continuous!');
  end
  [na,vna]=size(a);
  [nw,nb]=size(b);
  if nb==0 then
    b=zeros(na,1);
  end
  // controllable part
  if nargin==1 then
    [n,u,ind,V,a,b]=contr(a,b);
  else
    [n,u,ind,V,a,b]=contr(a,b,tol);
  end
  if nb==0 then
    b=[];
  end
  n=sum(n);nc=n;
  if nargout==4 then c=c*u;x0=u'*x0;end
  if n <> na then
    //order evals uncont. part
    nn=n+1:na;
    typ='c';if dom <> 'c' then typ='d',end
    [v,n1]=schur(a(nn,nn),sort = part(typ,1))
    n=n+n1
    //new realization
    if nargout > 2 then
      u(:,nn)=u(:,nn)*v
      if nargout==4 then
        a(:,nn)=a(:,nn)*v;a(nn,nn)=v'*a(nn,nn)
        b(nn,:)=v'*b(nn,:)
        // take care that c could be empty 
        cc=c(:,nn)*v
        if ~isempty(cc) then
          c(:,nn)=cc
        end
        x0(nn)=v'*x0(nn)
      end
    end
  end
  //
  if nargout==4 then sl=syslin(dom,a,b,c,d,x0),end
  if nargout==5 then v=sl.B;sl=sl.A;end
endfunction
