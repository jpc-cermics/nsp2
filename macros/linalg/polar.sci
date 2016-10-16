function [ro,teta]=polar(a)
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 1987-2016 - F. Delebecque (Inria)
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
//[ro,Teta]=polar(A);
// Polar form of A
// A=Ro*exp(%i*Teta) Ro symmetric >=0 
// Teta hermitian >=0

  [U,s,V]=svd(a);
  ro1=U*sqrt(diag(s));// s is a vector in nsp
  ro=ro1*ro1';
  W=U*V';
  // A = Ro*W   (Ro sdp ; W unit)
  // W=exp(%i*Teta)
  //
  [ab,x,bs]=bdiag(W+0*%i*W);
  z=log(diag(ab));
  lw=x*diag(z)*inv(x);
  teta=-%i*lw;
endfunction
