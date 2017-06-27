function [X,dim,Y]=im_inv(A,B,tol)
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 1987-2016 - F. Delebecque (Inria)
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
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
//
  [nA,mA]=size(A);[nB,mB]=size(B);
  if nargin==2 then tol=100*%eps*mA*nA*nB*mB,end
  if nA<>nB then error ("im_inv: uncompatible dimensions!"),return,end
  // basis for im(B)
  [Y,rB]=rowcomp(B);//u=Y'
  
  //Trivial cases
  if rB >= nA then X=eye(mA,mA);dim=mA,return;end
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



