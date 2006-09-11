function [y,ind,occ] = unique(x,first_ind=%f)

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
//   A unique function for nsp, to extract unique components of a vector.
//   Currently Matrix, String Matrix, Lists and Cells are
//   treated (list and cells with the slow method). For Cells and Lists 
//   unique is implemented in C (in cells.c and List.c).
//
//  Inputs
//  ------
//   x : a vector of numbers or strings  (x may be a
//       matrix and in this case it is considered as a big column 
//       vector using the column major order)
//   first_ind : optional, a boolean scalar (%f by default)  
//
//  Outputs
//  ------- 
//   y : vector of unique components of x 
//   ind : ind(i) gives one index of y(i) in x, the smaller one if
//         first_ind is true else any index such that x(ind(i)) = y(i). 
//   occ : occ(i) gives the number of occurence of y(i) in x
//
   type_x = type(x,"string")
   
   if type_x == "Mat" || type_x == "SMat" then
      n = size(x,"*")
      if n == 0 then
	 y = []; ind = []; occ = []; return
      end
      if nargout == 1 then 
	 y = sort(x(:),"g","i")
	 id = [find(y(1:$-1)~=y(2:$)), n]
	 y = y(id);
	 if  size(x,1) == 1 then, y.redim[1,-1]; end 
      else
	 if first_ind then, type_sort = "gm", else, type_sort = "g", end
	 if size(x,'*')== 1 then 
	   y = x; ind = 1;
	 else
	   [y,ind] = sort(x,type_sort,"i");
	   id = find(y(1:$-1)==y(2:$))         // id = 1 + find(y(1:$-1)==y(2:$))
	   if id ~= [] then, id = id + 1, end  // directly if mtlb mode is used
	   y(id) = []; ind(id) = []
	 end
	 if nargout == 3 then
	    occ = 1:n+1; if size(x,1) ~= 1 then, occ.redim[n+1,1], end
	    occ(id) = []; occ = occ(2:$)-occ(1:$-1)
	 end
	 if size(x,1) == 1 then  // x is a row vector => outputs in row form too
	    y.redim[1,-1]; ind.redim[1,-1];
	 end
      end
   else
      error("unique not implemented for "+type_x)
   end
endfunction
