function [c,ka,kb] = intersect(a,b)

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
//   An intersect function for nsp: c = { elements which are both in a and b }
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
//   c : vector with the elements which are both in a and in b
//       c is a row vector if both a and b are row vectors, otherwise
//       c is a column vector.
//   ka: is an index vector such that c = a(ka)
//   kb: is an index vector such that c = b(kb)
//

  type_a = type(a,"string")
  if type_a ~= type(b,"string") then
    error(" both arg must be of same type")
  end
  
  if type_a == "Mat" | type_a == "SMat" then
    na = size(a,"*")
    nb = size(b,"*")      
    if na == 0 then
      c = a; ka = []; kb = [];
    elseif nb == 0 then
      c = b; ka = []; kb = [];
    else
      row_flag = size(a,1)==1 & size(b,1)==1
      a.redim[-1,1]; b.redim[-1,1]
      
      if nargout == 1 then
	if na <= nb then
	  a = unique(a)
	  [ind,occ] = bsearch(b,a,match="v")
	  c = a(occ>0)
	else
	  b = unique(b)
	  [ind,occ] = bsearch(a,b,match="v")
	  c = b(occ>0)
	end
	if row_flag then, c.redim[1,-1]; end
	
      else
	[a,ka] = unique(a)
	[b,kb] = unique(b)
	if size(a,"*") < size(b,"*") then
	  ind = bsearch(b,a,match="v")
	  k = find(ind > 0)
	  c = b(k)
	  kb = kb(k)
	  ka = ka(ind(k))
	else
	  ind = bsearch(a,b,match="v")
	  k = find(ind > 0)
	  c = a(k)
	  ka = ka(k)
	  kb = kb(ind(k))
	end
	if row_flag then
	  c.redim[1,-1]; ka.redim[1,-1]; kb.redim[1,-1]
	end
      end
    end
  else
    error("intersect not currently implemented for "+type_a)
  end
  
endfunction
