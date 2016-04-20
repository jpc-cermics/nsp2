function [Bk,Ck]=fullrfk(A,k)
// This macro computes the full rank factorization of A^k i.e.
// Bk*Ck=A^k where Bk is full column rank and Ck full row rank.
// One has Range(Bk)=Range(A^k) and ker(Ck)=ker(A^k).
// For k=1 fullrfk is the same as fullrf.
//
// Copyright (C) 2007-2016 François Delebecque (GPL, scilab INRIA)
//
  if nargin <= 1 then k=1;end 
  [n,m]=size(A);
  if m<>n then error("fullrfk: matrix should be square");return;end
  if k==0 then Bk=eye(n,n);Ck=Bk; return; end
  if k==1 then [Bk,Ck]=fullrf(A); return; end 
  [Bk,Ck]=fullrf(A);B=Bk;C=Ck;
  for l=2:k
    [B,C,dim]=fullrf(C*B);
    Bk=Bk*B;Ck=C*Ck;     // Bk*Ck = A^k  (Full rank factorization)
  end;
endfunction
