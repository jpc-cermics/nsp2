function [W,rk]=colcomp(A, meth='svd', tol=[])
//Syntaxes : [W,rk]=colcomp(A,  meth='svd'| 'qr' , tol=tol)
// computes a column compression of matrix A 
// i.e. computation of ker(A)
// meth='qr' or 'svd' (defaut 'svd')
// tol tolerance parameter (of order %eps as defaut value)
// the ma-rk first columns of W span the kernel of A when size(A)=(na,ma)
// A*W=[0 Abar] where Abar is full column rank, and has rk columns.
// 
// Copyright (C) 2007-2016 François Delebecque (GPL, scilab INRIA)
//
      
  [ma,na]=size(A)
  if isempty(A) then W=[];rk=0;return;end
  if norm(A,1) < sqrt(%eps)/10 then rk=0,W=eye(na,na),return,end
  if isempty(tol) then tol=sqrt(%eps)*norm(A,1)*max(ma,na),end
  select meth
   case 'qr' then [q,r,e,rk]=qr(A',tol=tol);  W=q(:,na:-1:1)
   case 'svd' then [u,s,v,rk]=svd(A',tol=tol);  W=u(:,na:-1:1)
  else
    error('rowcomp: meth optional argument should be ''qr'' or ''svd''');
  end
endfunction
