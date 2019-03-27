function [h,num,den]=ss2tf(sl,rmax)
  // State-space to transfer function.
  // Syntax:
  //   h=ss2tf(sl) 
  //   h=ss2tf(sl,'b')
  //   h=ss2tf(sl,rmax)
  //
  //   sl   : linear system (syslin list)
  //   h    : transfer matrix 
  //   rmax : optional parameter controlling the conditioning in
  //          block diagonalization method is used (If 'b' is entered
  //          a default value is used)
  //   Method: By default, one uses characteristic polynomial and 
  //   det(A+Eij)=det(A)+C(i,j) where C is the adjugate matrix of A
  //   Other method used : block-diagonalization (generally 
  //   this gives a less accurate result).
  //
  //!
  // Copyright INRIA
  if type(sl,'short')=='m' || type(sl,'short')=='p' then
    h=sl
    return
  end
  if type(sl,'short') <> 'linearsys' then
    error('Error: First argument must be in state-space form')
  end
  if isempty(sl(3)) || isempty(sl(4)) || isempty(sl(2)) then
    h=sl(5);num=sl(5);den=eye(sl(5));
    return;
  end
  //1
  if or(sl.dom==['c','u']) then var='s';else var='z';end
  if sl.dom=='u' && type(sl(5),'short')=='p' then var=sl(5).get_var[];end
  //
  meth='p'
  if nargin==2 then
    if type(rmax,'short')=='s' then
      meth=part(rmax,1),
      nargin=1
    else
      meth='b'
    end
  end
  //
  select meth
    case 'b' then
     a=sl(2)
     [n1,vn1]=size(a)
     z=poly(0,var);
     if nargin==1 then
       [a,x,bs]=bdiag(a)
     else
       [a,x,bs]=bdiag(a,rmax)
     end
     k=1;m=[];v=ones(1,n1)+0*s;den=1+0*s;
     for n=bs do
       k1=k:k-1+n;
       // leverrier
       h=z*eye(n,n)-a(k1,k1)
       f=eye(n,n)
       for kl=1:n-1 do
         b=h*f,
         d=-sum(diag(b))/kl,
         f=b+eye(size(b))*d,
       end
       d=sum(diag(h*f))/n
       //
       den=den*d;
       l=[1:k-1,k+n:n1],
       if ~isempty(l) then v(l)=v(l)*d,end
       m=[m,x(:,k1)*f];
       k=k+n;
     end
     if nargout==3 then h=sl(5),num=sl(4)*m*diag(v)*(x\sl(3));return;end
     m=sl(4)*m*diag(v)*(x\sl(3))+sl(5)*den;
     [m1,n1]=size(m);[m,den]=simp(m,den*ones(m1,n1))
     h=p2r(m,den);
     h=syslin('c',h);
     h.set_dom[sl.dom]
     h.set_dt[sl.dt];

    case 'p' then
     Den=poly(sl(2),var)
     na=Den.degree[];
     den=m2p([],var = var);
     num=m2p([],var = var);
     [m,n]=size(sl(5))
     c=sl(4)
     for l=1:m do
       [m,i]=max(abs(c(l,:)));
       if m <> 0 then
         ci=c(l,i)
         t=eye(na,na)*ci;t(i,:)=[-c(l,1:i-1),1,-c(l,i+1:na)]
         al=sl(2)*t;
         t=eye(na,na)/ci;t(i,:)=[c(l,1:i-1)/ci,1,c(l,i+1:na)/ci]
         al=t*al;ai=al(:,i),
         b=t*sl(3)
         for k=1:n do
           al(:,i)=ai+b(:,k);
           [nlk,dlk]=simp(poly(al,var),Den)
           den(l,k)=dlk;
           num(l,k)=-(nlk-dlk)*ci
         end
       else
         num(l,1:n)=m2p(0*ones(1,n),dim = ".",var = var);
         den(l,1:n)=m2p(ones(1,n),dim = ".",var = var);
       end
     end
     if nargout==3 then h=sl(5);return;end
     w=num ./den+sl(5);
     if type(w,'short')=='m' then h=w;return;end;//degenerate case
     h=w;
     h.set[dom = sl.dom];
     h.set[dt = sl.dt];
  end
endfunction
