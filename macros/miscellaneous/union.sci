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
//   A union function for nsp, to build the union of 2 vectors a and b
//   considered as set (but uniqueness of each element in a or b is not
//   needed).
//   
//   Currently Matrix, String Matrix, Lists and Cells are
//   treated.
//
//  Inputs
//  ------
//   a,b : both vectors of numbers or strings or cells or lists (they
//         can be matrices and in this case they are considered as 
//         big column vectors using the column major order)
//
//  Outputs
//  ------- 
//   c: vector (union of components of a and b), c is sorted in
//      case of matrices of numbers or strings
//   ka, kb: vectors of indices such that c = a(ka) U b(kb)
//

function [c,ka,kb] = union(a,b,ind_type="double",which="elements")
  
  type_a = type(a,"string")
  if type_a ~= type(b,"string") then
    error(" both arg must be of the same type")
  end
  if ind_type ~= "double" && ind_type ~= "int" then
     error(" optional ind_type argument should be ""double"" or ""int""")
  end
  
  num = is_string_in_array(which, ["elements","rows","columns"], names=["which","union"])
  if num == 1 then
     if type_a == "Mat" || type_a == "SMat" || type_a == "Cells" || type_a == "IMat" then
	row_flag = size(a,1)==1 & size(b,1)==1
	a.redim[-1,1]; b.redim[-1,1]
	n = size(a,"*")
	if ind_type.equal["int"] then, n = m2i(n); end
	if nargout == 1 then
	   c = unique([a;b]);
	   if row_flag then, c.redim[1,-1]; end
	else
	   [c,ka] = unique([a;b],ind_type=ind_type)	 
	   ind = find(ka > n, ind_type)
	   kb = ka(ind) - n
	   ka(ind) = []
	   if row_flag then
	      c.redim[1,-1]; ka.redim[1,-1]; kb.redim[1,-1]
	   end
	end
     else
	error("union not implemented for "+type_a)
     end
  else
     if type_a == "Mat" || type_a == "IMat" then
	if num==2 then  // union of rows
	   // a and b should have the same number of columns
	   if size(a,2) ~= size(b,2) then
	      error(" first and second args should have the same number of columns ")
	   end
	   if nargout == 1 then
	      c = unique([a;b],which="rows",ind_type=ind_type)
	   else
	      m = size(a,1)
	      if ind_type.equal["int"] then, m = m2i(m); end
	      [c,ka] = unique([a;b],which="rows",ind_type=ind_type)
	      ind = find(ka > m, ind_type)
	      kb = ka(ind) - m
	      ka(ind) = []
	   end
	else // num==1 union of columns
	   // a and b should have the same number of rows
	   if size(a,1) ~= size(b,1) then
	      error(" first and second args should have the same number of columns ")
	   end
	   if nargout == 1 then
	      c = unique([a,b],which="columns",ind_type=ind_type)
	   else
	      n = size(a,2)
	      if ind_type.equal["int"] then, n = m2i(n); end
	      [c,ka] = unique([a,b],which="columns",ind_type=ind_type)
	      ind = find(ka > n, ind_type)
	      kb = ka(ind) - n
	      ka(ind) = []
	   end
	end
     else
	error("union of rows or columns not implemented for "+type_a)
     end
  end
endfunction

function [c,ka,kb] = union_l_l(a,b,ind_type="double")    
  if ind_type ~= "double" && ind_type ~= "int" then
     error(" optional ind_type argument should be ""double"" or ""int""")
  end
  n = length(a)
  if ind_type.equal["int"] then, n = m2i(n); end
  [c,ka] = unique(list_concat(a,b),ind_type=ind_type)
  ind = find(ka > n,ind_type)
  kb = ka(ind) - n
  ka(ind) = []
endfunction
