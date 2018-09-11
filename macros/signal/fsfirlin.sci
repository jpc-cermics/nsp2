function [hst]=fsfirlin(hd,flag)
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
  
//<hst>=fsfirlin(hd,flag)
//macro for the design of FIR, linear phase filters
//using the frequency sampling technique
//  hd   : vector of desired frequency response samples
//  flag : is equal to 1 or 2,
//         according to the choice of type 1 or type 2 design
//  hst  : vector giving the approximated continuous response
//         on a dense grid of frequencies
// Author: G. Le Vey  Date: 1 Febr. 1989

  n1=prod(size(hd));//filter length
  if int(n1/2)==n1/2 then 
    n=2*n1;
  else
    n=2*n1+1;
  end
  scd=sincd(n,flag);//calculates the function Sin(N*x)/Sin(x)
  hst=hd(1)*scd(4*n+1:6*n+1);
  eps=(-1)**(n-1);
  for j=1:n1-1,
    hst=hst+hd(j+1)*[scd(-4*j+4*n+1:-4*j+6*n+1)+eps*scd(4*j+1:4*j+2*n+1)];
  end
endfunction
