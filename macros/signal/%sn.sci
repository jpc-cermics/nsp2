function [y]=%sn(x,m)
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
  
//Jacobi 's elliptic function with parameter m
//which computes the inverse of the elliptic
//integral for the parameter m.
//x may be a vector.
//The amplitude is computed in fortran and apply
//the addition formulas for elliptic functions
//  x :A point inside the fundamental rectangle
//    :defined by the elliptic integral
//  m :Parameter of the elliptic integral (0<m<1)
//  y :Result
//
//Author F.D.


  [n1,n2]=size(x);
  n=n1*n2;
  a=amell(real(x),sqrt(m));
  s=sin(a);
  c=cos(a);
  d=sqrt(ones(n1,n2)-m*s.*s);
  m1=1-m;
  a1=amell(imag(x),sqrt(m1));
  s1=sin(a1);
  c1=cos(a1);
  d1=sqrt(ones(n1,n2)-m1*s1.*s1);
  y=(s.*d1+%i*c.*d.*s1.*c1)./(c1.*c1+m*s.*s.*s1.*s1);
endfunction
