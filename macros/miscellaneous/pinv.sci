function [B] = pinv(A,tol=[])

// Copyright (C) 2005 Bruno Pinçon
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
//  Purpose
//  -------
//    computes the pseudo inverse of the matrix A
//
//    tol is the cut level for small singular values : all
//    singular values under tol * max(singular values) are
//    not taken into account.
//   
//    rmk: uses at the end the new .* feature
  
  if type(A,"short")~="m" then
    error("first arg must be a matrix of numbers"), 
  end
  
  if isempty(tol) then, tol = max(size(A))*%eps, end
  
  [m,n] = size(A)
  
  [U,s,V] = svd(A,mode="e")
  if s(1) == 0 then
    B = zeros_new(n,m)
  else
    k = max(find(s >= s(1)*tol))
    si = 1 ./s(1:k)
    if k < min(m,n) then
      B = V(:,1:k).*si'*U(:,1:k)'
    else
      B = V.*si'*U'
    end
  end
  
endfunction
