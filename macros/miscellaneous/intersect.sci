function [c,ka,kb] = intersect(a,b,ind_type="double",which="elements")

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
//   An intersect function for nsp: c = { elements/rows or columns which are both in a and b }
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
//       intersection operation should be done on elements, rows or columns
//
//       when which="elements" a or b could be matrices and in this case 
//       each one is considered as a big column vector using the column
//       major order.
//
//       intersection of rows or columns is only available for Mat and IMat
//       
//  ind_type : lets to precise the container type of ka and kb index vectors. By
//       default it is double, that is ka and kb are stored in Mat, and
//       when ind_type="int", ka and kb are stored in IMat with
//       integer type "int32"
//
//  Outputs
//  ------- 
//   c : components (elements/rows or columns) of the intersection
//   
//   ka: is an index vector such that c == a(ka), c == a(ka,:) or c == a(:,ka)
//   kb: is an index vector such that c == b(kb), c == b(kb,:) or c == b(:,kb)
//
//   intersection for lists or cells are also available (only in the
//   "elements" meaning), see the two last macros of this file.

   type_a = type(a,"string")
   if type_a ~= type(b,"string") then
      error(" both arg must be of same type")
   end

  if ind_type ~= "double" && ind_type ~= "int" then
     error(" optional ind_type argument should be ""double"" or ""int""")
  end

  if ~(type_a == "Mat" || type_a == "SMat" || type_a == "IMat") then
     error("intersect not currently implemented for "+type_a)
  
  elseif type_a == "IMat" && a.itype[] ~= b.itype[] then
     error(" both argument must be of the same integer type")
  
  elseif type_a == "Mat" && ~(isreal(a,%t) && isreal(b,%t)) then
     error("intersect not implemented for arrays of complex numbers")
  end
   
  num = is_string_in_array(which, ["elements","rows","columns"], names=["which","intersect"])

  if num == 1 then  // intersection of elements
     na = numel(a)
     nb = numel(b)      

     if na == 0 || nb == 0 then
	c = [];  ka = []; kb = []; // fixme...
	return
     end
     
     row_flag = size(a,1)==1 & size(b,1)==1
     a.redim[-1,1]; b.redim[-1,1]

     if ~(type_a == "Mat" && ~(and(isfinite(a)) && and(isfinite(b)))) then
	if nargout == 1 then
	   if na <= nb then
	      a = unique(a)
	      [ind,occ] = bsearch(b,a,match="v",ind_type="int")
	      c = a(find(occ))
	   else
	      b = unique(b)
	      [ind,occ] = bsearch(a,b,match="v",ind_type="int")
	      c = b(find(occ))
	   end
	else
	   [a,ka] = unique(a,ind_type=ind_type)
	   [b,kb] = unique(b,ind_type=ind_type)
	   if numel(a) >= numel(b) then
	      ind = bsearch(b,a,match="v",ind_type="int")
	      k = find(ind,ind_type="int")
	      c = b(k)
	      kb = kb(k)
	      ka = ka(ind(k))
	   else
	      ind = bsearch(a,b,match="v",ind_type="int")
	      k = find(ind,ind_type="int")
	      c = a(k)
	      ka = ka(k)
	      kb = kb(ind(k))
	   end
	end
     else  //  Mat with +_Inf or Nan entries in a or b: bsearch method can't work
	if nargout == 1 then
	   a = unique(a);
	   b = unique(b);
	   c = sort([a;b],dir="i")
	   doublons = find(c(1:$-1) == c(2:$), ind_type="int");
	   c = c(doublons);
	else
	   [a,ka] = unique(a,ind_type=ind_type);
	   na = m2i(numel(a));
	   [b,kb] = unique(b,ind_type=ind_type);
	   [c,ind] = sort([a;b],dir="i",type="gs",ind_type="int")
	   doublons = find( c(1:$-1) == c(2:$), ind_type="int" );
	   c = c(doublons);
	   ka = ka(ind(doublons))
	   kb = kb(ind(doublons+1i)-na)
	end
     end
     if row_flag then
	c.redim[1,-1]; 
	if nargout > 1 then, ka.redim[1,-1]; kb.redim[1,-1], end
     end
     
  else
     
     if type_a == "SMat" then
     	error("intersection of rows or columns not implemented for "+type_a)
     end
     
     if num == 2 then  // intersections of rows
	if size(a,2) ~= size(b,2) then
	   error(" first and second args should have the same number of columns")
	end
	if nargout == 1 then
	   a = unique(a,which="rows");
	   b = unique(b,which="rows");
	   c = sort([a;b],dir="i",type="ldr")
	   doublons = find(and(c(1:$-1,:)==c(2:$,:),dim=2),ind_type="int")
	   c = c(doublons,:)
	else
	   [a,ka] = unique(a,ind_type=ind_type,which="rows");
	   na = m2i(size(a,1));
	   [b,kb] = unique(b,ind_type=ind_type,which="rows");
	   [c,ind] = sort([a;b],dir="i",type="ldr",ind_type="int")
	   doublons = find(and(c(1:$-1,:)==c(2:$,:),dim=2), ind_type="int" );
	   c = c(doublons,:);
	   ind = ind([doublons;doublons+1i]);
	   ka = ka(ind(ind<=na))
	   kb = kb(ind(ind>na)-na)
	end
 	
     else              // intersections of columns
	if size(a,1) ~= size(b,1) then
	   error(" first and second args should have the same number of rows")
	end
	if nargout == 1 then
	   a = unique(a,which="columns");
	   b = unique(b,which="columns");
	   c = sort([a,b],dir="i",type="ldc")
	   doublons = find(and(c(:,1:$-1)==c(:,2:$),dim=1),ind_type="int")
	   c = c(:,doublons)
	else
	   [a,ka] = unique(a,ind_type=ind_type,which="columns");
	   na = m2i(size(a,2));
	   [b,kb] = unique(b,ind_type=ind_type,which="columns");
	   [c,ind] = sort([a,b],dir="i",type="ldc",ind_type="int")
	   doublons = find(and(c(:,1:$-1)==c(:,2:$),dim=1), ind_type="int" );
	   c = c(:,doublons);
	   ind = ind([doublons;doublons+1i]);
	   ka = ka(ind(ind<=na))
	   kb = kb(ind(ind>na)-na)
	end
	
     end

  end
endfunction

function [c,ka,kb] = intersect_l_l(a,b)
   // version for list
   [a, inda] = unique(a)
   [b, indb] = unique(b)
   c = list(); ka = []; kb = [];
   if length(a) >= length(b) then
      for i = 1:length(b)
	 elem = b.item[i]
	 [found,k] = a.has[elem];
	 if found then
	    c.add_last[elem]
	    ka.concatr[inda(k)]
	    kb.concatr[indb(i)]
	 end
      end
   else
      for i = 1:length(a)
	 elem = a.item[i]
	 [found,k] = b.has[elem];
	 if found then
	    c.add_last[elem]
	    ka.concatr[inda(i)]
	    kb.concatr[indb(k)]
	 end
      end
   end
endfunction

function [c,ka,kb] = intersect_ce_ce(a,b)
   // version for cell array

   row_flag = size(a,1)==1 & size(b,1)==1
   a.redim[-1,1]; b.redim[-1,1]
   
   [a, inda] = unique(a)
   [b, indb] = unique(b)
   c = {}; ka = []; kb = []
   
   if length(a) >= length(b) then
      for i = 1:length(b)
	 elem = b{i};
	 [found,k] = a.has[elem];
	 if found then
	    c.concatd[{elem}]
	    ka.concatd[inda(k)]
	    kb.concatd[indb(i)]
	 end
      end
   else
      for i = 1:length(a)
	 elem  = a{i}
	 [found,k] = b.has[elem]
	 if found then
	    c.concatd[{elem}]
	    ka.concatd[inda(i)]
	    kb.concatd[indb(k)]
	 end
      end
   end

   if row_flag then
      c.redim[1,-1]; ka.redim[1,-1]; kb.redim[1,-1]
   end

endfunction

