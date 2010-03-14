function [c,ka] = setdiff(a,b,ind_type="double",which="elements")

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
//   a : matrix or vector of numbers or strings.
//       a is considered as a set (collection) of elements (scalars), or
//       of row vectors or column vectors
//       
//   b : matrix or vector of numbers or strings.
//       b is considered as a set (collection) of elements (scalars), or
//       of row vectors or column vectors
//
//   which=... : optional argument (default is elements) telling if the
//       setdiff operation should be done on elements/rows or columns
//
//       when which="elements" a or b could be matrices and in this case 
//       each one is considered as a big column vector using the column
//       major order.
//
//       setdiff of rows or columns is only available for Mat and IMat
//       
//  ind_type : lets to precise the container type of ka indices. By
//       default it is double, that is indices are stored in Mat, and
//       when ind_type="int", ka indices are stored in an IMat with
//       integer type "int32"
//
//  Outputs
//  ------- 
//   c : components (elements/rows or columns) of a (considered as a set) which
//       are not in b ;
//   
//   ka: ka(i) is an index of c(i), c(i,:) or c(:,i) in a, that is:
//            a(ka(i)) = c(i)      when which="elements"
//            a(ka(i),:) = c(i,:)  when which="rows"  
//            a(:,ka(i)) = c(:,i)  when which="columns"  
//
//   setdiff for lists or cells are also available (only in the
//   "elements" meaning), see the two last macros of this file.
//
//
//
  type_a = type(a,"string")
  if type_a ~= type(b,"string") then
    error(" both argument must be of the same type")
  end

  if ind_type ~= "double" && ind_type ~= "int" then
     error(" optional ind_type argument should be ""double"" or ""int""")
  end

  if type_a == "IMat" && a.itype[] ~= b.itype[] then
     error(" both argument must be of the same integer type")
  elseif type_a == "Mat" && ~(isreal(a,%t) && isreal(b,%t)) then
     error("setdiff not implemented for arrays of complex numbers")
  end
  
  num = is_string_in_array(which, ["elements","rows","columns"], names=["which","union"])

  if num == 1 then   // setdiff elements
  
     if  type_a == "Mat" || type_a == "SMat" || type_a == "IMat" then
	if size(a,"*") == 0 then
	   c = a; ka = []; return
	elseif size(b,"*") == 0 then
	   [c,ka] = unique(a,ind_type=ind_type); return
	end
	
	if type_a == "Mat" then
	   [c,ka] = unique(a,ind_type=ind_type)
	   d = unique(b)
	   if size(c,1) == 1 then
	      d.redim[1,-1]; e = [c,d]
	   else
	      d.redim[-1,1]; e = [c;d]
	   end
	   [e,ind] = sort(e,type = "gs", dir="i",ind_type="int")
	   doublons = find( e(1:$-1) == e(2:$) ,ind_type="int")
	   irem = ind(doublons) // indices to remove in c and ka
	   c(irem) = []; ka(irem) = []
	else
	   [c,ka] = unique(a,ind_type=ind_type)
	   [kb,occ,missed] = bsearch(b,c,match="v",ind_type="int") 
	   if missed > 0 then, kb = kb(find(kb)), end
	   c(kb) = []; ka(kb) = [];
	end
     else
	error("setdiff not currently implemented for "+type_a)
     end
  
  else
     if type_a == "Mat" || type_a == "IMat" then
	if num==2 then  // setdiff of rows
	   // a and b should have the same number of columns
	   if size(a,2) ~= size(b,2) then
	      error(" first and second args should have the same number of columns")
	   end
	   [c,ka] = unique(a,which="rows",ind_type=ind_type)
	   d = unique(b,which="rows")
	   [e,ind] = sort([c;d],type = "ldr", dir="i",ind_type="int")
	   doublons = find( and(e(1:$-1,:) == e(2:$,:),dim=2) , ind_type="int")
	   // if the ldr sort was stable it will be enough to do:
	   // irem = ind(doublons) // indices to remove in c and ka
	   // c(irem,:) = []; ka(irem) = []
	   // but is is not so we use:
	   ind([doublons,doublons+1i]) = [];
	   k = find(ind <= m2i(size(c,1)),ind_type="int")
	   c = c(ind(k),:);
	   ka = ka(ind(k));
	   
	else // num==1 union of columns
           // a and b should have the same number of rows
	   if size(a,1) ~= size(b,1) then
	      error(" first and second args should have the same number of rows")
	   end
	   [c,ka] = unique(a,which="columns",ind_type=ind_type)
	   d = unique(b,which="columns")
	   [e,ind] = sort([c,d],type = "ldc", dir="i",ind_type="int")
	   doublons = find( and(e(:,1:$-1) == e(:,2:$),dim=1),ind_type="int" )
	   // if the ldc sort was stable it will be enough to do:
	   // irem = ind(doublons) // indices to remove in c and ka
	   // c(:,irem) = []; ka(irem) = []
	   // but is is not so we use:
	   ind([doublons,doublons+1i]) = [];
	   k = find(ind <= m2i(size(c,2)),ind_type="int")
	   c = c(:,ind(k));
	   ka = ka(ind(k));
	   
	end
     else
	error("setdiff of rows or columns not implemented for "+type_a)
     end
  end
endfunction


function [c,ka] = setdiff_l_l(a,b,ind_type="double")
  if ind_type ~= "double" && ind_type ~= "int" then
     error(" optional ind_type argument should be ""double"" or ""int""")
  end
  [c,ka] = unique(a,ind_type="double")
  for i = 1:length(b)
    [found,k] = c.has[b.item[i]];
    if found then, c.remove[k], ka(k) = [], end
  end
endfunction

function [c,ka] = setdiff_ce_ce(a,b,ind_type="double")
  if ind_type ~= "double" && ind_type ~= "int" then
     error(" optional ind_type argument should be ""double"" or ""int""")
  end
  [c,ka] = unique(a,ind_type="double")
  for i = 1:length(b)
    [found,k] = c.has[b{i}];
    if found then, c(k) = [], ka(k) = [], end
  end
endfunction


