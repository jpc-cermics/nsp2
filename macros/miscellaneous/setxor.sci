function [c,ka,kb] = setxor(a,b,ind_type="double",which="elements")

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
//   c = (a U b) \ { elements which are both in a and b }
//
//  Inputs
//  ------
//   a : matrix or vector of numbers or strings.
//       a is considered as a set (collection) of elements (scalars), or
//       of row vectors or column vectors
//       
//   b : matrix or vector of numbers or strings.
//       b is considered as a set (collection) of elements (scalars), or
//       of row vectors or column vectors
//
//   which=... : optional argument (default is elements) telling if the
//       set xor operation should be done on elements, rows or columns
//
//       when which="elements" a or b could be matrices and in this case 
//       each one is considered as a big column vector using the column
//       major order.
//
//       setxor of rows or columns is only available for Mat and IMat
//       
//  ind_type : lets to precise the container type of ka and kb index vectors. By
//       default it is double, that is ka and kb are stored in Mat, and
//       when ind_type="int", ka and kb are stored in IMat with
//       integer type "int32"
//
//  Outputs
//  ------- 
//   c : components (elements/rows or columns) of a U b which are not
//   in the intersection of a and b
//   
//   ka and kb:  index vectors such that c == a(ka) U b(kb)
//
//   setxor for lists or cells are also available (only in the
//   "elements" meaning), see the two last macros of this file.

  type_a = type(a,"string")
  if type_a ~= type(b,"string") then
     error(" both arg must be of same type")
  end

  if ind_type ~= "double" && ind_type ~= "int" then
     error(" optional ind_type argument should be ""double"" or ""int""")
  end

  if ~(type_a == "Mat" || type_a == "SMat" || type_a == "IMat") then
     error("setxor not currently implemented for "+type_a)
     
  elseif type_a == "IMat" && a.itype[] ~= b.itype[] then
    error(" both argument must be of the same integer type")
  
  elseif type_a == "Mat" && ~(isreal(a,%t) && isreal(b,%t)) then
     error("setxor not implemented for arrays of complex numbers")
  end
   
  num = is_string_in_array(which, ["elements","rows","columns"], names=["which","setxor"])

  if num == 1 then  // setxor of elements
     na = numel(a)
     nb = numel(b)
     
     if na == 0 then
	[c,kb] = unique(b); ka = []
	return
     elseif nb == 0 then
	[c,ka] = unique(a); kb = []
	return
     end
     
     row_flag = size(a,1)==1 & size(b,1)==1
     a.redim[-1,1]; b.redim[-1,1]

     if nargout == 1 then
	a = unique(a);
	b = unique(b);
	// en fait il faudrait un "merge"
	c = sort([a;b],dir="i",type="gs")
	doublons = find(c(1:$-1) == c(2:$), ind_type="int");
	c([doublons;doublons+1i]) = []
     else
	[a,ka] = unique(a,ind_type=ind_type);
	na = m2i(numel(a));
	[b,kb] = unique(b,ind_type=ind_type);
	[c,ind] = sort([a;b],dir="i",type="gs",ind_type="int")
	doublons = find( c(1:$-1) == c(2:$), ind_type="int" );
	irem = [doublons;doublons+1i] 
	c(irem) = []
	ind(irem) = []
	ka = ka(ind(ind<=na))
	kb = kb(ind(ind>na)-na)
     end
     	
     if row_flag then
	c.redim[1,-1]; 
	if nargout > 1 then
	   ka.redim[1,-1]; kb.redim[1,-1]
	end
     end

  else
     
     if type_a == "SMat" then
     	error("setxor of rows or columns not implemented for "+type_a)
     end
     
     if num == 2 then  // setxor of rows
	if size(a,2) ~= size(b,2) then
	   error(" first and second args should have the same number of columns")
	end
	if nargout == 1 then
	   a = unique(a,which="rows");
	   b = unique(b,which="rows");
	   c = sort([a;b],dir="i",type="ldr")
	   doublons = find(and(c(1:$-1,:)==c(2:$,:),dim=2),ind_type="int")
	   c([doublons;doublons+1i],:) = []
	else
	   [a,ka] = unique(a,ind_type=ind_type,which="rows");
	   na = m2i(size(a,1));
	   [b,kb] = unique(b,ind_type=ind_type,which="rows");
	   [c,ind] = sort([a;b],dir="i",type="ldr",ind_type="int")
	   doublons = find(and(c(1:$-1,:)==c(2:$,:),dim=2), ind_type="int" );
	   irem = [doublons;doublons+1i] 
	   c(irem,:) = []
	   ind(irem) = []
	   ka = ka(ind(ind<=na))
	   kb = kb(ind(ind>na)-na)
	end
 	
     else              // setxor of columns
	if size(a,1) ~= size(b,1) then
	   error(" first and second args should have the same number of rows")
	end
	if nargout == 1 then
	   a = unique(a,which="columns");
	   b = unique(b,which="columns");
	   c = sort([a,b],dir="i",type="ldc")
	   doublons = find(and(c(:,1:$-1)==c(:,2:$),dim=1),ind_type="int")
	   c(:,[doublons;doublons+1i]) = []
	else
	   [a,ka] = unique(a,ind_type=ind_type,which="columns");
	   na = m2i(size(a,2));
	   [b,kb] = unique(b,ind_type=ind_type,which="columns");
	   [c,ind] = sort([a,b],dir="i",type="ldc",ind_type="int")
	   doublons = find(and(c(:,1:$-1)==c(:,2:$),dim=1), ind_type="int" );
	   irem = [doublons;doublons+1i] 
	   c(:,irem) = []
	   ind(irem) = []
	   ka = ka(ind(ind<=na))
	   kb = kb(ind(ind>na)-na)
	end
	
     end
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

