function sl=tf2ss(h,tol)
  // Transfer function to state-space.
  //Syntax : sl=tf2ss(h [,tol])
  // h = transfer matrix
  // sl = linear system in state-space representation (syslin list)

  if or(type(h,'short')==['m','p']) then
    sl=syslin([],[],[],[],h);
    return;
  end
  num=h.num;
  den=h.den;
  [nd,md]=size(den)
  a=[];b=[];c=[];d=[];n1=0;// s=[]
  dk=m2p([],var = den.get_var[]);
  for k=1:md do
    for l=1:nd do
      [r,q]=pdiv(num(l,k),den(l,k));
      dk(l,1)=q;
      num(l,k)=r;
    end
    if nd <> 1 then
      [nk,pp]=cmndred(num(:,k),den(:,k));
    else
      pp=den(k);nk=num(k);
    end

    slk=cont_frm(nk,pp);
    // [ak,bk,ck,dk1]=slk(2:5);
    // [s sk]
    [n2,m2]=size(slk.B);
    if n2 <> 0 then
      a(n1+1:n1+n2,n1+1:n1+n2)=slk.A
      b(n1+1:n1+n2,k)=slk.B;
      c=[c,slk.C];
      n1=n1+n2;
    else
      if n1 <> 0 then b(n1,k)=0;end
    end
    d=[d,dk];
  end

  // if d.degree[]==0 then d=coeff(d),end

  if n1 <> 0 then
    nrmb=norm(b,1);
    nrmc=norm(c,1);
    fact=sqrt(nrmc*nrmb);
    b=b*fact/nrmb;
    c=c*fact/nrmc;
    atmp=a;
    [a,u]=balanc(a);
    //next lines commented out to fix bug 3796
    //   if rcond(u)< %eps*n1*n1*100
    //     nn=size(a,1);u=eye(nn,nn);a=atmp;
    //   end

    //apply transformation u without matrix inversion
    [k,l]=find(u <> 0);//get the permutation
    u=u(k,l);c=c(:,k)*u;b=diag(1 ./diag(u))*b(k,:);

    if nargin < 2 then
      [no,u]=contr(a',c',%eps);
    else
      [no,u]=contr(a',c',tol);
    end
    u=u(:,1:no);
    a=u'*a*u;b=u'*b;c=c*u;
    sl=linear_system(a,b,c,d,zeros(size(a,1),1),dom = h.dom,sample = h.dt);
  else
    sl=linear_system([],[],[],d,zeros(0,1),dom = h.dom,sample = h.dt);
  end
endfunction
