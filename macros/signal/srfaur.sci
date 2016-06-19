function [p,s,t,l,rt,tt]=srfaur(h,f,g,r0,n,p,s,t,l)
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
  
//square-root algorithm for the algebraic Riccati equation.
//
//   h,f,g  : convenient matrices of the state-space model.
//   r0     : E(yk*yk').
//   n      : number of iterations.
//
//   p      : estimate of the solution after n iterations.
//   s,t,l  : intermediate matrices for
//          : successive iterations;
//   rt,tt  : gain matrices of the filter model after n iterations.
//
//          : p,s,t,l may be given as input if more than one recursion
//          : is desired (evaluation of intermediate values of p, e.g.).
// 

  [d,m]=size(h);
  if nargin==5,
    //initializations
    r0=.5*(r0+r0');
    s=sqrtm(r0);
    t=(g/s)';s=s';
    l=-%i*t;
    p=l'*l;
  else
    if nargin<>9,
      error('wrong number of arguments');end
  end
  //recursion
  for j=1:n,
    a=[s t;l*h' l*f'];
    [q,r]=qr(a);
    s=r(1:d,1:d);
    t=r(1:d,d+1:d+m);
    l=r(d+1:2*d,d+1:d+m);
    p=p+l'*l;
  end
  //gain matrices of the filter.
  rt=r0-h*p*h';
  tt=(g-f*p*h')*inv(rt);
endfunction
