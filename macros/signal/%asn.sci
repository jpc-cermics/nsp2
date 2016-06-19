function [y]=%asn(x,m)
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 1987-2016 - F. Delebecque (INRIA)
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
  
//Calculates the elliptic integral:
//  y = integral from 0 to x of
//       [1/(((1-t*t)^(1/2))(1-m*t*t)^(1/2))]
//For vector arguments y is a vector
//  x :Upper limit of integral (x>0)
//  m :Parameter of integral (0<m<1)
//  y :Value of the integral

  m=real(m);
  if m<0 then error('m must be positive');end 
  if m>1 then error('m must be lower than 1');end 
  y=delip(x,sqrt(m));
endfunction
