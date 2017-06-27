function [U,dim]=range(A,k)
// Copyright (C) 2007-2016 François Delebecque (GPL, scilab INRIA)
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
// Computes the Range of A^k ; 
// the first dim columns of U' span the range of A^k.
//
//
  if nargin <= 1 then k=1;end 
  [m,n]=size(A);
  if m<>n then error("range: first argument should be a square matrix");return;end
  if k==0 then dim=m;U=eye(m,m);return; end
  [U,dim]=rowcomp(A);
  if k==1 then return; end
  for l=2:k do B=A*U'; [U,dim]=rowcomp(B(:,1:dim)); end
endfunction
