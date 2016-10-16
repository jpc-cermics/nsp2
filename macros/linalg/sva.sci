function [U,S,V]=sva(A,tol)
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
// matrix rank approximation 
  if isempty(A) then U=[],S=[],V=[],return,end
  [U,S,V]=svd(A,mode="e")
  if nargin <=1  then
    tol = max(size(A)) * S(1) * %eps;
    rk = size(find(diag(S) > tol),"*");
  else
    if tol > 1 then 
      //rank given
      rk=tol
      if rk>min(size(A)) then 
	error("Requested rank is greater than matrix dimension")
      end
    else
      rk = size(find(diag(S) > tol),"*");
    end
  end
  S=diag(S);
  U=U(:,1:rk);S=S(1:rk,1:rk),V=V(:,1:rk)
endfunction

    
    
