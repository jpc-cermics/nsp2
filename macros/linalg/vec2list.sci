function l=vec2list(vect,sizes,ind)
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
// vect: a vector 
// sizes a kx2 matrix giving sizes 
//
  msizes = prod(sizes,"c")
  n = sum(msizes);
  if isempty(vect) then vect=zeros(1,n);end
  if size(vect,"*") < n then 
    error(sprintf("Error: first argument of vec2list is too small %d, expecting %d",...
		  size(vect,"*"),n));
    return 
  end
  l=list();
  start=1;
  for k=1:size(sizes,"r") do
    m= vect(start:start + msizes(k)-1);
    start.add[ msizes(k)];
    l.add_last[matrix(m,sizes(k,1),sizes(k,2))];
  end
  if nargin == 3 then 
    ZZZ
  end
endfunction

function V=list2vec(L)
// L: a list of matrices 
  V=[]; for i=1:length(L) do V.concatd[L(i)(:)];end
endfunction 
