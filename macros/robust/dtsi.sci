function [ga,gs,gi]=dtsi(g,tol)
//[ga,gs,gi]=dtsi(g,[tol]) stable-antistable decomposition of g:
//    g = ga + gs + gi   (gi = g(oo))
//    g can be given in state-space form or in transfer form.
//    (see syslin)
//  - ga antistable and strictly proper.
//  - gs  stable and strictly proprer.
//  - gi = g(oo)
// tol optional parameter for detecting stables poles. 
// Default value: 100*%eps
//!
// Copyright INRIA

  if nargin==1 then tol=100*%eps,end,
  if type(g,'short')=='r' then
    //transfer function 
    //----------------------------
    num=g.num,den=g.den,var= den.get_var[],
    [t1,t2]=size(num),
    s=poly(0,var=var);
    num1=0*ones(t1,t2)+0*s;
    num2=num1;
    den1=ones(t1,t2)+0*s;
    den2=den1;
    for i=1:t1,
      for j=1:t2,
	n=num(i,j),d=den(i,j),
	dn=n.degree[],dd=d.degree[],
	if dn>dd then error('Error: degree  num. > degree  den.'),end,
	//alf and bet /alf*d1(unstable) and bet*d2(stable)=n.
	if dd==0 then 
	  num2(i,j)=n,
	else
	  pol=roots(d),
	  k=1,no=1,
	  [a,l]=sort(real(pol)),pol=pol(l),
	  while k<=dd,
	    if real(pol(k))<=tol then 
	      k=dd+1,
	    else
	      k=k+1,no=no+1,
	    end,
	  end,
	  select no,
	   case 1 then num2(i,j)=n,den2(i,j)=d,
	   case dd+1 then num1(i,j)=n,den1(i,j)=d,
	  else
	    d1=poly(pol(1:no-1),var),d2=poly(pol(no:dd),var),
	    if dn==dd then 
	      d1o=poly([coeff(d1),0],var,'c'),
	      dd=dd+1,no=no+1,
	    else
	      d1o=d1,
	    end
	    u=sylm(d1o,d2),cn=[coeff(n),0*ones(1,dd-dn-1)],
	    x=u\cn',
	    alf=poly(x(1:dd-no+1),var,'c'),
	    bet=poly(x(dd-no+2:dd),var,'c'),
	    num1(i,j)=bet,den1(i,j)=d1,
	    num2(i,j)=alf,den2(i,j)=d2,
	  end,
	end,
      end,
    end,
    ga=syslin('c',num1,den1),gs=syslin('c',num2,den2),
    gi1=ginfini(ga),gi2=ginfini(gs),
    ga=ga-gi1,gs=gs-gi2,gi=gi1+gi2,return,
  else
    //state space:
    //---------------------------
    [a,b,c,d]=abcd(g);
    gi=d;
    [n1,n2,t]=size(g),
    [a,u0]=balanc(a);b=u0\b;c=c*u0;
    [u,n]=schur(a,sort='c'),
    a=u'*a*u,
    if n==t then ga=0,
      gs=g,return,
    end,
    if n==0 then gs=0,
      ga=g,return,
    end,
    //      [ab,w,bs]=bdiag(a);
    a1=a(1:n,1:n),a4=a(n+1:t,n+1:t),x=a(1:n,n+1:t),
    z=sylv(a1,-a4,-x,'c'),
    w=[eye(n,n),z;0*ones(t-n,n),eye(t-n,t-n)],
    wi=[eye(n,n),-z;0*ones(t-n,n),eye(t-n,t-n)],
    tr=u*w,tri=wi*u';
    bb=tri*b,b1=bb(1:n,:),b2=bb(n+1:t,:),
    cc=c*tr,c1=cc(:,1:n),c2=cc(:,n+1:t),
    ga=syslin('c',a4,b2,c2),
    gs=syslin('c',a1,b1,c1);
  end
endfunction

function D=ginfini(g)
//gi=ginfini(g) computes D = g(infinity) for the proper transfer matrix g
//!
  if type(g,'short')=='m' then D=g,return,end,
  if type(g,'short')<>'r' then 
    error("Error: expecting a transfer matrix");
    return;
  end
  num=g.num,den=g.den;
  [nn,mm]=size(num),D=0*ones(nn,mm),
  for i=1:nn,
    for j=1:mm,
      n=num(i,j),d=den(i,j),
      dn=n.degree[],dd=d.degree[],
      if dn>dd then 
	error('Error: Improper system'),
      else
	if dn==dd then
	  D(i,j)=coeff(n,dn)/coeff(d,dd)
	end,
      end
    end
  end
endfunction
