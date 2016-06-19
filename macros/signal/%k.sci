function [K]=%k(m)
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 1987-2016 - F. Delebecque et all (INRIA)
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
  
//K=%k(m)
//Calculates Jacobi's complete elliptic integral
//of the first kind:
//  K = integral from 0 to 1 of
//      [(1-t**2)(1-m*t**2)]**(-1/2)
//m is allowed to be a vector
//Ref :Abramowitz and Stegun page 598
//  m :Parameter used in calculating the elliptic
//    :integral where 0<m<1.
//  K :Value of the elliptic integral from 0 to 1
//    :on the real axis.
//
// 
//Author F.D.

  [n1,n2]=size(m);
  un=ones(n1,n2);
  a=un;
  b=sqrt(un-m);
  c=sqrt(m);
  while max(abs(c)) > %eps,
    an=0.5*(a+b);
    bn=sqrt(a.*b);
    cn=0.5*(a-b);
    a=an;
    b=bn;
    c=cn;
  end
  K=%pi*un./(2*a);
endfunction
