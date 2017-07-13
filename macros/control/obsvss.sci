function [a,b,c]=obsvss(a,b,c,tol)
  typ=type(a,'short');
  select typ
    case 'm' then
     if nargin <= 4 then tol=100*%eps;end
    case 'linearsys' then
     sl=a;
     select nargin
       case 1 then tol=100*%eps
       case 2 then tol=b
       else
         error('Error: when first argument is a linear system arguments are sl [,tol]')
     end
     [a,b,c,d]=abcd(sl);
  end
  [no,u]=contr(a',c',tol)
  u=u(:,1:no)
  a=u'*a*u;b=u'*b;c=c*u
  if nargout==1 then
    if typ=='linearsys' then
      a=linear_system(a,b,c,d,u'*sl.X0,dom = sl.dom,sample = sl.dt);
    else
      x0=zeros(size(a,1),1);
      a=linear_system(a,b,c,d,u'*x0,dom = 'c');
    end
  end
endfunction
