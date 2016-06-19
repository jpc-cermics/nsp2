function [la,sig,lb]=levin(n,cov)
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 1987-2016 - G. Le Vey (INRIA)
//
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
  
//[la,sig,lb]=levin(n,cov)
//macro which solves recursively on n
//the following Toeplitz system (normal equations)
//
//
//		|R1   R2   . . . Rn  |
//		|R0   R1   . . . Rn-1|
//		|R-1  R0   . . . Rn-2|
//		| .    .   . . .  .  |
// |I -A1 . -An|| .    .   . . .  .  |
//		| .    .   . . .  .  |
//		| .    .   . . .  .  |
//		|R2-n R3-n . . . R1  |
//		|R1-n R2-n . . . R0  |
//
//  where {Rk;k=1,nlag} is the sequence of nlag empirical covariances
//
//  n   : maximum order of the filter
//  cov : matrix containing the Rk (d*d matrices for a
//      : d-dimensional process). It must be given the
//      : following way:
//
//		|  R0  |
//		|  R1  |
//		|  R2  |
//		|  .   |
//		|  .   |
//		| Rnlag|
//
//  la  : list-type variable, giving the successively calculated
//      : polynomials (degree 1 to degree n), with coefficients Ak
//  sig : list-type variable, giving the successive
//      : mean-square errors.
// Author: G. Le Vey
// 

  if nargin<>2,error('wrong number of arguments');end
  [l,d]=size(cov);
  if d>l,error('bad dimension for the covariance sequence');end
  //
  //   Initializations
  //
  a=eye(d,d);b=a;
  z=poly(0,'z');la=list();lb=list();sig=list();
  p=n+1;cv=cov;
  for j=1:p,cv=[cov(j*d+1:(j+1)*d,:)';cv];end
  for j=0:n-1,
    //
    //   Bloc permutation matrix
    //
    jd=jmat(j+1,d);
    //
    //   Levinson algorithm
    //
    r1=jd*cv((p+1)*d+1:(p+2+j)*d,:);
    r2=jd*cv(p*d+1:(p+1+j)*d,:);
    r3=jd*cv((p-1-j)*d+1:p*d,:);
    r4=jd*cv((p-j)*d+1:(p+1)*d,:);
    c1=coeff(a);c2=coeff(b);
    sig1=c1*r4;gam1=c2*r2;
    k1=(c1*r1)*inv(gam1);
    k2=(c2*r3)*inv(sig1);
    a1=a-k1*z*b;
    b=-k2*a+z*b;a=a1;
    la(j+1)=a;lb(j+1)=b;sig(j+1)=sig1;
  end
endfunction
