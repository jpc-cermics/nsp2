function [W,rk]=rowcomp(A,flag,tol)
//Row compression of A <--> computation of im(A)
//flag and tol are optional parameters
//flag='qr' or 'svd' (default 'svd')
//tol tolerance parameter (sqrt(%eps)*norm(A,1) as default value)
//the rk first (top) rows of W span the row range of A
//the rk first columns of W' span the image of A
//W'*A = [Abar;0] where Abar has rk rows and is full row rank.
//F.D.
//!
  if A==[] then W=[];rk=0;return;end
  [ma,na]=size(A)
  if norm(A,1) < sqrt(%eps)/10 then rk=0,W=eye(ma,ma),return;end
  
  select nargin
  case 1 then     // default values for flag and tol
    flag='svd',tol=sqrt(%eps)*norm(A,1);
  case 2 then     //default value for tol
    tol=sqrt(%eps)*norm(A,1)
  else
    if size(tol,'*')>1|~isreal(tol)|tol<0 then
      error('rowcomp: third argument should be a real non negative scalar')
    end
  end
  select flag
  case 'qr' then 
    [q,r,p,rk,e]=qr(A,tol=tol);W=q';
  case 'svd' then 
    [u,s,v,rk]=svd(A,tol=tol);W=u' ;
  else
    error('rowcomp: second argument should be ''qr'' or ''svd''')
  end
endfunction


