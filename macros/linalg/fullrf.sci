function [Q,M,rk]=fullrf(A, tol=[])
//[Q,M,rk]=fullrf(A)
// Full rank factorization: A=Q.M
// with range(Q)=range(A) and ker(M)=ker(A),
// Q full column rank , M full row rank
// rk = rank(A) = #columns(Q) = #rows(M)
// 
// Copyright (C) 2007-2016 François Delebecque (GPL, scilab INRIA)
//
  if norm(A,1) < 1.d-10 then Q=[];M=[];rk=0;return;end
  if isempty(tol) then tol=sqrt(%eps)*norm(A,1);end
  [U,s,V,rk]=svd(A,tol=tol);
  sq=sqrt(diag(s));
  Q=U*sq;M=sq*V';
  Q=Q(:,1:rk);M=M(1:rk,:);
endfunction



