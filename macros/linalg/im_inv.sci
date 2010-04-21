function [X,dim,Y]=im_inv(A,B,tol)
//[X,dim]=im_inv(A,B [,tol]) computes (A^-1)(B) i.e the span of vectors whose
// image through A are in range(B).
// This subspace is kernel(pi_B *A) where pi_B is the canonical projection
// y -> y mod(Im(B)).
// The dim first columns de X span (A^-1) (B)
// tol is a threshold to test if a  subspace is included in an other
// default value tol = 100*%eps;
// [Y*A*X,Y*B]=[A11 A12,B1
//              0   A22,0]
//with B1 full row rank and rank(B1) = rank(B), A22 full column rank and
//dim = # columns of A11.
// F.D.
//!
  [nA,mA]=size(A);[nB,mB]=size(B);
  if nargin==2 then tol=100*%eps*mA*nA*nB*mB,end;
  if nA<>nB then error ('im_inv: uncompatible dimensions!'),return,end
  // basis for im(B)
  [Y,rB]=rowcomp(B);//u=Y'
  
  //Trivial cases
  if rB >= nA then X=eye(mA,mA);dim=mA,return;end;
  if rB ==0 then [X,k1]=colcomp(A);dim=mA-k1,return,end
  //
  up=1:rB;low=rB+1:nA;
  A=Y*A;   //update 

  //vectors with image in B
  [X,r1]=colcomp(A(low,:))
  A1=A*X;    //update
  Aup=A1(up,:);
  Alow=A1(low,:);    //Alow(:,1:ma-r1)=0 by construction
  if norm(Alow,1) <= tol*norm(Aup,1) then dim=mA,return,end
  dim=mA-r1;
endfunction



