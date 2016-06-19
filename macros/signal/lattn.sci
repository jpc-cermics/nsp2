function [la,lb]=lattn(n,p,vcov)
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
  
//[la,lb]=lattn(n,p,vcov)
//macro which solves recursively on n (p being fixed)
//the following system (normal equations), i.e. identifies
//the AR part(poles) of a vector ARMA(n,p) process
//
//                        |Rp+1 Rp+2 . . . . . Rp+n  |
//                        |Rp   Rp+1 . . . . . Rp+n-1|
//                        | .   Rp   . . . . .  .    |
//                        |                          |
//    |I -A1 -A2 . . .-An|| .    .   . . . . .  .    |=0
//                        | .    .   . . . . .  .    |
//                        | .    .   . . . . .  .    |
//                        | .    .   . . . . . Rp+1  |
//                        |Rp+1-n.   . . . . . Rp    |
//
//    where {Rk;k=1,nlag} is the sequence of empirical covariances
//
//   n   : maxmum order of the filter
//   p   : fixed dimension of the MA part. If p is equal to -1,
//       : the algorithm reduces to the classical Levinson recursions.
//   cov : matrix containing the Rk(d*d matrices for
//       : a d-dimensional process).It must be given the
//       : following way:
//
//                        |  R0 |
//                        |  R1 |
//                    cov=|  R2 |
//                        |  .  |
//                        |  .  |
//                        |Rnlag|
//
//   la  : list-type variable, giving the successively calculated
//       : polynomials (degree 1 to degree n),with coefficients Ak
// Author: G. Le Vey  Date: 9 Febr. 1989
// 

   [l,d]=size(vcov);
   if d>l,error('bad dimension for the covariance sequence');end
   if nargin <>3 then error('wrong number of arguments');end
   a=eye_deprecated(d);b=eye_deprecated(d);
   z=poly(0,'z');la=list();lb=list();
   no=p-n-1;cv=vcov;
   if no<0 then 
     for j=1:-no,cv=[vcov(j*d+1:(j+1)*d,:)';cv];end;p=p-no;
   end
   for j=0:n-1,
   jd=jmat(j+1,d);
   r1=jd*cv((p+1)*d+1:(p+2+j)*d,:);
   r2=jd*cv(p*d+1:(p+1+j)*d,:);
   r3=jd*cv((p-1-j)*d+1:p*d,:);
   r4=jd*cv((p-j)*d+1:(p+1)*d,:);
   c1=coeff(a);c2=coeff(b);
   k1=(c1*r1)*inv(c2*r2);
   k2=(c2*r3)*inv(c1*r4);
   a1=a-k1*z*b;
   b=-k2*a+z*b;a=a1;
   a=clean(a);b=clean(b);
   la(j+1)=a;lb(j+1)=b;
   end
endfunction

function [la,lb]=lattp(n,p,vcov)
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
  
// G Levey
  [l,d]=size(vcov);
  id=eye_deprecated(d);
  [a,b]=lattn(n,0,vcov);a=a(n);b=b(n);
  z=poly(0,'z');la=list();lb=list();
  jd=jmat(n+1,d);
  for j=0:p-1,
    r1=jd*vcov((j+1)*d+1:(j+n+2)*d,:);
    r2=jd*vcov(j*d+1:(j+n+1)*d,:);
    c1=coeff(a);c2=coeff(b);
    k=(c1*r1)*inv(c2*r2);
    hst=-inv(c1(:,n*d+1:(n+1)*d));
    r=k*hst;
    a=(id-r*z)*a-k*z*b;
    b=-hst*a;
    a=clean(a);
    b=clean(b);
    la(j+1)=a;
  end
endfunction
