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


function [A,jb]=rref(A,tol)
  error("Error: rref only implemented for scalar or sparse matrices");
  return;
endfunction
  
function [A,jb]=rref_m(A,tol)
//R = rref(A) produces the reduced row echelon form of A.  

  if nargin < 2 then  tol=2*%eps*norm(A,"inf")*max(size(A)); end
  
  if isempty(A) then R=[],return,end
  [m,n]=size(A)
  k = 1;l = 1;jb = [];
  while (k <= m) & (l <= n) do
    // Find value and index of largest element in the remainder of column l.
    [p,i] = max(abs(A(k:$,l))); i = i+k-1;
    if (p <= tol) then 
      // The column is negligible
      A(k:$,l) = zeros(m-k+1,1);
      l = l + 1;
    else 
      jb = [jb, l]; // Remember column index
      A([k,i],l:$) = A([i,k],l:$); //swap rows i and j
      A(k,l:$) = A(k,l:$)/A(k,l); // Normalize the pivot row
      
      i = [1:k-1 , k+1:m]; //other rows
      A(i,l:$) = A(i,l:$) - A(i,l)*A(k,l:$); //eliminate
      k = k + 1;l = l + 1;
    end
  end
endfunction


function [A,jb]=rref_sp(A,tol)
//R = rref(A) produces the reduced row echelon form of A.  

  if nargin < 2 then  tol=2*%eps*norm(A,"inf")*max(size(A)); end
  
  if isempty(A) then R=[],return,end
  [m,n]=size(A)
  k = 1;l = 1;jb = [];
  while (k <= m) & (l <= n) do
    // Find value and index of largest element in the remainder of column l.
    [p,i] = max(abs(A(k:$,l))); i = i+k-1;
    if ( full(p) <= tol) then 
      // The column is negligible; WARNING: <= not
      // implemented for sparses 
      A(k:$,l) = zeros(m-k+1,1);
      l = l + 1;
    else 
      jb = [jb, l]; // Remember column index
      A([k,i],l:$) = A([i,k],l:$); //swap rows i and j
      A(k,l:$) = A(k,l:$) ./ A(k,l); // Normalize the pivot row
      
      i = [1:k-1 , k+1:m]; //other rows
      A(i,l:$) = A(i,l:$) - A(i,l)*A(k,l:$); //eliminate
      k = k + 1;l = l + 1;
    end
  end

endfunction

