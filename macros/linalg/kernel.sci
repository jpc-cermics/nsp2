function [kerA] = kernel(A,tol=[],meth="svd")

// Copyright (C) 2007-2010 Bruno Pinçon
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
//    computes the kernel (nullspace) of the matrix A (computes
//    an orthonormal basis of the kernel).
//
//    tol is the cut level for small singular values : all
//    singular values under tol * max(singular values) are
//    considered as zero.
//
//    using the qr method is faster (than svd) but the cut level is  
//    computed from an estimation only of the min and max singular 
//    values by the function qr see :  int[dz]geqrpf in 
//    src/lapack/lapack.c).
//    
//   
   if nargin < 1 || nargin > 3 then
      error("Error: bad number of input arguments")
   end
   
   if ~is(A,%types.Mat) then
      error("Error: first argument should be of type Mat")
   end
  
   if isempty(tol) then
      tol = max(size(A))*%eps
   else
      if ~( is(tol,%types.Mat) && isreal(tol) && isscalar(tol) && 0 < tol && tol <= 1 ) then
	 error("Error: tol should be a real scalar in (0,1]")
      end
   end  
  
  [m,n] = size(A)

  if meth.equal["qr"] then
    [Q,R,p,rk] = qr(A',tol=tol)
    kerA = Q(:,rk+1:n)
  elseif meth.equal["svd"] then
    [U,s,V] = svd(A)
    rk = max(find(s >= s(1)*tol))
    kerA = V(:,rk+1:n)
  else
    error("bad optional argument meth (should be ""qr"" or ""svd"")")
  end
  
endfunction
