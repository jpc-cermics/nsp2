function [W,rk]=rowcomp(A, meth='svd', tol=[])
//Row compression of matrix A <--> computation of im(A)
// tol tolerance parameter (sqrt(%eps)*norm(A,1) as default value)
// the rk first (top) rows of W span the row range of A
// the rk first columns of W' span the image of A
// W'*A = [Abar;0] where Abar has rk rows and is full row rank.
//
// Copyright (C) 2007-2016 François Delebecque (GPL, scilab INRIA)
//
  if isempty(A) then W=[];rk=0;return;end
  [ma,na]=size(A)
  if norm(A,1) < sqrt(%eps)/10 then rk=0,W=eye(ma,ma),return;end
  if isempty(tol) then tol=sqrt(%eps)*norm(A,1);end 
  select meth
   case 'qr' then [q,r,p,rk,e]=qr(A,tol=tol);W=q';
   case 'svd' then [u,s,v,rk]=svd(A,tol=tol);W=u' ;
  else
    error('rowcomp: meth optional argument should be ''qr'' or ''svd''');
  end
endfunction


