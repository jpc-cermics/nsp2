function [Si,Pi,Di,order]=penlaur(E,A)
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 1988-2016 - F. Delebecque (Inria)
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
//[Si,Pi,Di,order]=penlaur(E,A)
// First Laurent coefficients of (s*E-A)^-1;
// (s*E-A)^-1 = ... + Si/s - (Pi + s*Di + ... +s^order Ni) at s = infinity
// order = order of the singularity
// The matrix s*E-A should be invertible.
// F.D. (1988,1990) (Experimental version: troubles when bad conditioning of
// (so*E-A)...)
  
  if nargin==1 then [E,A]=pen2ea(E);end
  seed=grand("getsd");
  grand("setsd",0);
  tests=grand(1,10,"nor",0,1);
  conditions=0*tests;k=1;
  for s0=tests do conditions(k)=cond(s0*E-A);k=k+1;end
  [w,k1]=min(conditions);
  grand("setsd",seed)

  if w>1.E+20 then error("Singular pencil!");return;end
  s0=tests(k1);
  J=inv(s0*E-A);
  [Se,Pe,De,i1]=projspec(J*E);
  [Sa,Pa,Da,i2]=projspec(J*A);
  order=i1-1;
  Si=Se*J;
  Pi=Pe*Sa*J;
  Di=Pi*E*Pi;
  if order==0 then Di=0*Di;end
  //[S,P,D,index]=projspec(Di*E);
endfunction
