function [c,ka,kb] = setxor(a,b)

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
//  Purpose
//  -------
//   c = (a U b) \ { elements which are both in a and b }
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
//   c : vector with the elements which are in a and in b but not
//       both in a and b
//       c is a row vector if both a and b are row vectors, otherwise
//       c is a column vector.
//   ka, kb: index vectors such that c = a(ka) U b(kb) 
//

  type_a = type(a,"string")
  if type_a ~= type(b,"string") then
     error(" both arg must be of same type")
  end
  
  if type_a == "Mat" | type_a == "SMat" then
     na = size(a,"*")
     nb = size(b,"*")      
     if na == 0 then
	[c,kb] = unique(b); ka = []
     elseif nb == 0 then
	[c,ka] = unique(a); kb = []
     else
	row_flag = size(a,1)==1 & size(b,1)==1
	a.redim[-1,1]; b.redim[-1,1]
	if nargout == 1 then
	   a = unique(a)
	   b = unique(b)
	   if size(b,"*") <= size(a,"*") then
	      [ind,occ] = bsearch(b,a,match="v")
	      c = sort([a(occ==0);b(ind==0)], "g", "i")
	   else
	      [ind,occ] = bsearch(a,b,match="v")
	      c = sort([b(occ==0);a(ind==0)], "g", "i")
	   end
	   if row_flag then, c.redim[1,-1]; end
	else
	   [a,ka] = unique(a)
	   [b,kb] = unique(b)
	   if size(b,"*") <= size(a,"*") then
	      [ind,occ] = bsearch(b,a,match="v")
	      inda = find(occ==0); indb = find(ind==0)
	   else
	      [ind,occ] = bsearch(a,b,match="v")
	      inda = find(ind==0); indb = find(occ==0)
	   end
	   c = sort( [a(inda);b(indb)], "g", "i" )
	   ka = ka(inda); kb = kb(indb)
	   if row_flag then
	      c.redim[1,-1]; ka.redim[1,-1]; kb.redim[1,-1]
	   end
	end
     end
  else
     error("setxor not currently implemented for "+type_a)
  end
  
endfunction


function [c,ka,kb] = setxor_l_l(a,b)
   // version for list
   [a, inda] = unique(a)
   [b, indb] = unique(b)
   na = length(a)
   [c, k, occ] = unique(list_concat(a,b))
   ind = find(occ==1)
   c = c.sublist[ind]
   k = k(ind)
   ka = inda(k(k <= na))
   kb = indb(k(k > na)-na)
endfunction


function [c,ka,kb] = setxor_ce_ce(a,b)
   // version for cell array
   row_flag = size(a,1)==1 & size(b,1)==1
   a.redim[-1,1]; b.redim[-1,1]
   
   [a, inda] = unique(a)
   [b, indb] = unique(b)
   na = length(a)
   [c, k, occ] = unique([a;b])
   ind = find(occ==1)
   c = c(ind)
   k = k(ind)
   ka = inda(k(k <= na))
   kb = indb(k(k > na)-na)

   if row_flag then
      c.redim[1,-1]; ka.redim[1,-1]; kb.redim[1,-1]
   end

endfunction


  
   
