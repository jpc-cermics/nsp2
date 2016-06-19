function [s]=sincd(n,flag)
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
  
//<s>=sincd(n,flag)
//macro which calculates the function Sin(N*x)/Sin(x)
//  n    : value of N in the preceding function
//  flag : flag=1 : the function is centered around the origin
//       : flag=2 : the function is delayed by %pi/(2*n)
//  s    : vector of values of the function on a dense
//       : grid of frequencies
// Author: G. Le Vey  Date: 1 Febr 1989
// 

  npt=4*n;
  pas=%pi/npt;
  om=0:pas:%pi;
  eps=(-1)**(n-1);
  select flag
   case 1,
    s1=sin(n*om);s2=sin(om);
    s1(1)=n;s2(1)=1;s1(npt+1)=n*eps;s2(npt+1)=1;
    s=s1./s2;
    s=[s(npt+1:-1:2) s];
    s=s/n;
   case 2,
    om=om-ones_deprecated(om)*%pi/2/n;
    s1=sin(n*om);
    s2=sin(om);
    s1(3)=n;s2(3)=1;
    s=s1./s2;
    s=[eps*s s(2:npt+1)];
    s=s/n;
  else error('flag must be equal to 1 or 2')
  end
endfunction
