function [a,b,c]=minreal(a,b,c,domaine,tol)
  select type(a,'short')
    case 'linearsys' then
     if nargout <> 1 then error('Error: output should be sle'),end
     select nargin
       case 1 then istol=0
       case 2 then istol=1,tol=b,
       else
         error('Error: 2 inputs to minreal: sl [,tol]'),
     end
     sl=a;
     [a,b,c,d]=abcd(sl);x0=sl.X0;
     dom=sl.dom;
     if sl.dom=='u' then dom='d';end
     if dom <> 'c' then dom='d',end
    case 'm' then
     if nargout <> 3 then
       error('Error: 3 outputs to minreal: a,b,c.'),
     end
     select nargin
       case 4 then istol=0
       case 5 then istol=1,
       else
         error('Error: 4 or 5 inputs :a,b,c,domaine [,tol]'),
     end
     dom=domaine;
    else
      error("Error: first argument should be a matrix or a linear system");
      return;
  end
  //
  wc=lyap(a',-b*b',dom);
  wo=lyap(a,-c'*c,dom);
  if istol==0 then
    [r,n]=equil1(wc,wo);
  else
    [r,n]=equil1(wc,wo,tol);
  end
  n1=n(1);
  ri=inv(r);r=r(1:n1,:);ri=ri(:,1:n1)
  a=r*a*ri;b=r*b;c=c*ri
  if nargout==1 then a=syslin(dom,a,b,c,d,r*x0),end
endfunction
