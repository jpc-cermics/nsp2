function [c,ka] = setdiff(a,b)

// Copyright (C) 2006 Bruno Pinçon
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
//   A setdiff function for nsp: c = a - b (or c= a \ b in french notation)
//
//  Inputs
//  ------
//   a : row or column vector (*) of numbers or strings.
//       a is considered as a set of elements.
//       
//   b : row or column vector (*) of numbers or strings.
//       b must be same type than a and like a, b 
//       is considered as a set of elements.
//
//   (*) a or b could be matrices and in this case each one is considered
//       as a big column vector using the column major order.
//
//  Outputs
//  ------- 
//   c : vector with the components of a (considered as a set) which
//       are not in b ; c is a row vector if a is row vector and a
//       column vector in the other cases (that is when a is a column 
//       vector or a matrix).
//   ka: ka(i) is an index of c(i) in a, that is a(ka(i)) = c(i)
//       (a(ka) must be equal to c).
//
   
   type_a = type(a,"string")
   if type_a ~= type(b,"string") then
      error(" both argument must be of the same type")
   end
   
   if type_a == "Mat" || type_a == "SMat" then
      if size(a,"*") == 0 then
	 c = a; ka = []; return
      elseif size(b,"*") == 0 then
	 [c,ka] = unique(a); return
      end
      [c,ka] = unique(a)
      [kb,occ,missed] = bsearch(b,c,match="v") 
      if missed > 0 then, kb(kb==0) = [], end
      c(kb) = []; ka(kb) = [];
   else
      error("setdiff not currently implemented for "+type_a)
   end

endfunction

function [c,ka] = setdiff_l_l(a,b)
   [c,ka] = unique(a)
   for i = 1:length(b)
      [found,k] = c.has[b.item[i]];
      if found then, c.remove[k], ka(k) = [], end
   end
endfunction

function [c,ka] = setdiff_ce_ce(a,b)
   [c,ka] = unique(a)
   for i = 1:length(b)
      [found,k] = c.has[b{i}];
      if found then, c(k) = [], ka(k) = [], end
   end
endfunction

   
