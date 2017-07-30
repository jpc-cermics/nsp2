function [Stmp,Ws]=colregul(Sl,Alfa,Beta)
  // [Stmp,Ws]=regul(Sl) computes a polynomial-state-space prefilter 
  // Ws such that Stmp=Sl*Ws is proper and non singular.
  // Poles at infinity of Sl are moved to Alfa;
  // Zeros at infinity of Sl are moved to Beta;
  // Sl is asummed left invertible i.e. ss2tf(Sl) full column rank
  //!
  // Copyright INRIA
  if nargin <= 1 then Alfa=0;Beta=0;end
  flag=0;
  if type(Sl,'short')=='r' then
    flag=1;
    Sl=tf2ss(Sl);
  end
  if type(D,'short')=='p' then var=D.get_var[];else var='s';end;
  s=poly(0,var);
  D=Sl(5);// will 
  n=size(D);n1=n(1);n2=n(2);
  Ws=syslin([],[],[],[],eye(n2,n2));
  Ws.D.set_var[var];
  Stmp=Sl;
  if type(D,'short')=='p' then
    m=max(D.degree[]);
    //     moving poles @ infinity to poles @ Alfa
    while m > 0 do
      Dm=coeff(D,m);
      [W,r]=colcomp(Dm);
      if r==0 then Wstmp=W;else
        dim=n2-r;
        Wstmp=W*[eye(dim,dim),zeros(dim,r);zeros(r,dim),tf2ss(1/(s-Alfa)*eye(r,r))];
      end
      Ws=Ws*Wstmp;
      Stmp=Stmp*Wstmp;
      D=clean(Stmp.D);// always as a polynomial
      m=max(D.degree[]);
    end
  end
  D=Stmp.D;
  if type(D,'short')=='p' then D=coeff(Stmp.D,0);end
  [W,r]=colcomp(D);
  if r==n1 then
    Ws=Ws*W;Stmp=Stmp*W;
    if flag==1 then Ws=ss2tf(Ws);Stmp=ss2tf(Stmp);end
    return;
  end
  [nx,nvx]=size(Stmp(2));
  //      moving zeros @ infinity to zeros @ Beta
  i=0;
  while r < n2 do
    i=i+1;
    if r==n2 then
      Wstmp=W;
    else
      dim=n2-r;Wstmp=W*[(s-Beta)*eye(dim,dim),zeros(dim,r);zeros(r,dim),eye(r,r)];
    end
    Wstmp=syslin([],[],[],[],Wstmp);
    Ws=Ws*Wstmp;
    Stmp=Stmp*Wstmp;
    Stmp.D=clean(Stmp(5));
    rold=r;
    D=Stmp(5);
    if type(D,'short')=='p' then D=coeff(Stmp(5),0);end
    [W,r]=colcomp(D);
    if i >= nx then break;end
  end
  if flag==1 then
    Ws=ss2tf(Ws);
    Stmp=ss2tf(Stmp);
  end
endfunction
