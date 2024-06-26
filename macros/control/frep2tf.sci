function [best_h,best_w]=frep2tf(frq,repf,dg,dom,tols,weight)
  // iterative use of frep2tf_b jpc fd 1997 

  // Copyright INRIA

  if nargin <= 3 then dom='c';end
  if nargin <= 4 then
    rtol=1.E-2;atol=1.E-4,N=10;
  else
    rtol=tols(1);atol=tols(2);N=tols(3);
  end
  if isempty(dom) then dom='c';end
  if dom.equal['d'] then dom=1;end
  if nargin <= 5 then
    [h,err]=frep2tf_b(frq,repf,dg,dom);
    best_w=[];
  else
    [h,err]=frep2tf_b(frq,repf,dg,dom,weight);
    best_w=weight;
  end
  best_h=h;
  for k=1:N do
    if dom.equal['c'] then
      //    weight=(1)./abs(freq(h('den'),1,%i*frq*2*%pi));
      weight=(1) ./horner(h.den,%i*frq*2*%pi,ttmode = %t);
    else
      weight=(1) ./horner(h.den,exp(dom*%i*frq*2*%pi),ttmode = %t);
    end
    [h,err1]=frep2tf_b(frq,repf,dg,dom,weight);
    if ((abs(err-err1) < rtol*err & err > err1) | err1 < atol) then break;end
    if err1 < err then best_err=err1;best_h=h;end
    err=err1;
    printf('iteration '+string(k+1)+', error='+string(err1)+'\n');
  end
endfunction

function [h,err]=frep2tf_b(frq,repf,dg,dom,weight)
  // steer, jpc, fd 1997 (Nov)
  //============================

  // test the system type 'c' 'd' or dt 
  if nargin <= 3 then dom='c';end
  if nargin <= 4 then weight=[];end
  if isempty(dom) then dom='c';end
  if dom.equal['d'] then dom=1;end
  n=size(frq,'*');
  if dom.equal['c'] then
    w=2*%i*%pi*matrix(frq,n,1);
  else
    w=exp(2*%i*%pi*dom*matrix(frq,n,1));
  end
  //initialization
  m=2*dg
  //We compute the linear system to be solved: 
  //w(k)=%i* frq(k)*2pi 
  //for k=1,n  sum(a_i*(w(k))^i,i=1,dg)
  //		-repf(k)*sum(b_i*(w(k))^i,i=1,dg) = 0 
  //with sum x_i = 1
  //building Van der monde matrix ( w_i^j ) i=1,n j=0:dg-1
  a1=w .*.[ones(1,dg)];
  //0.^0 is not accepted in Scilab....
  a1=[ones(n,1),a1 .^(ones(n,1) .*.[1:(dg)])];
  a2=a1;for k=1:n do a2(k,:)=-repf(k)*a2(k,:);end
  a=[a1,a2];
  // Computing constraints
  // We impose N(i wk) - repfk D(i wk) =0 for k=imax
  // as follows:
  // N(i wk) = repfk*(1+%i*b)
  // D(i wk) = 1+%i*b
  // L*[x;b]=[repfk;1]
  // Least squ. pb is  min norm of [A,0] [x;b]
  //  under constraint         L*[x;b]=[repfk;1]              
  [rmax,imax]=max(abs(repf))
  L2=a(imax,1:dg+1);
  L=[zeros(size(L2)),L2,%i;L2,zeros(size(L2)),repf(imax)*%i];
  BigL=[real(L);imag(L)]
  c=[1;repf(imax)];
  Bigc=[real(c);imag(c)];
  [ww,dim]=rowcomp(BigL);
  BigL=ww*BigL;Bigc=ww*Bigc;
  BigL=BigL(1:dim,:);Bigc=Bigc(1:dim,:);

  a=[a,zeros(size(a,1),1)];
  // auto renormalization : if weight is not given 
  if dom.equal['c'] then
    if isempty(weight) then
      nn=sqrt(sum(abs(a) .^2,'c'))+ones(n,1);
      a=a ./(nn*ones(1,size(a,2)));
    end
  end
  // user given renormalization
  if ~isempty(weight) then
    if size(frq,'*') <> size(weight,'*') then
      error('Error: frq and weight must have same size');
      return;
    end
    w1=weight(:)*ones(1,size(a,2));
    a=w1 .*a;
  end
  BigA=[real(a);imag(a)];
  // Constraints BigL x =Bigc
  // 
  x=LSC(BigA,BigL,Bigc);
  x=x(1:$-1);

  h=syslin(dom,poly(x(1:dg+1),'s','c'),poly([x((dg+2):$)],'s','c'))
  if nargout==2 then
    repf1=repfreq(h,frq = frq);
    err=sum(abs(repf1(:)-repf(:)))/n;
  end
endfunction

function x=LSC(A,L,c)
  // Ax=0 Least sq. + Lx = c
  [W,rk]=colcomp(L);
  LW=L*W;
  Anew=A*W
  A1=Anew(:,1:($-rk))
  A2=Anew(:,($-rk+1:$));
  x2=inv(LW(:,$-rk+1:$))*c
  b=-A2*x2
  x1=A1\b
  x=W*[x1;x2]
endfunction
