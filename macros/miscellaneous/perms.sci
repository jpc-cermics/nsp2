function y=perms(x)

// Copyright (C) 2007 Bruno Pinçon
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
// purpose
// -------  
//  computes all permutations of the vector x
//  x must be a "general" (numerical, string, cell,...) full matrix 
//  that is must implement the Matint interface
//  
//  x is considered as a row vector with n components and all the
//  n! permutations are given as rows of y, so y has n! rows and 
//  n columns.

  if ~implements(x,%types.Matint) then
    error("argument have not the good type (must implement matint)")
  end
  x.redim[1,-1]
  n = size(x,"*");
  if n == 1 | n == 0 then
    y=x
  else
    p = perms(x(2:$))
    m = prod(1:n-1)
    y = repmat(x(1),m*n,n)
    for j = 1:n
      i1 = (j-1)*m+1
      i2 = j*m
      y(i1:i2,1:j-1) = p(:,1:j-1)
      y(i1:i2,j+1:n) = p(:,j:n-1)
    end
  end
endfunction
