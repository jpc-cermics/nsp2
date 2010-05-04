function F = nchoosek(E, k)
//
// Copyright (C) 2010 Francois Delebecque (INRIA), Bruno Pinçon (ESIAL/IECN)
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
// PURPOSE
//
// if #E=1 
//    E should be a non negative integer (says n), and the function
//    compute:
//   
//      n!/(k! (n-k)!)
//
// else 
//    E should implement matint and the function computes
//    all the subsets with k elements of E (each line of F corresponds
//    to one of the n!/(k! (n-k)!) subsets)
//
   if nargin ~= 2 then
      error("two arguments are required for nchoosek")
   end
   
   if ~implements(E,%types.Matint) then
      error("first argument have not the good type (must implement matint)")
   end

   if ~( is(k,%types.Mat) && numel(k)==1 && isreal(k) && k >= 0 && floor(k) == k ) then
      error("second argument should be a non negative integer")
   end

   n = numel(E);
   if n == 1 then
      if ~( is(E,%types.Mat) && isreal(E) && E >= 0 && floor(E) == E ) then
	 error("when scalar, first argument should be a non negative integer")
      else
	 if ( 0 <= k && k <= E ) then
	    F = bincoeff(E,k)
	 else
	    error("k should be an integer between 0 and "+sprintf("%d",E)) 
	 end
      end
   else
      E.redim[1,-1]
      if n < k then
	 F = zeros(0,k);
      elseif n == k then
	 F = E
      elseif k == 0 then
	 F = repmat(E,1,0) 
      else  // use a triangular like algorithm to avoid the inefficient
	    // basic recursive algorithm
	 p = n-k+1      
	 T = cell(1,p);
	 for j=2:p, T{j} = E(n-j+1:n); T{j}.redim[-1,1]; end
	 for i=2:k
	    T{1} = E(n-i+1:n) 
	    for j=2:p
	       T{j} = [ repmat(E(n-i+2-j),size(T{j},1),1) , T{j} ; T{j-1} ]
	    end
	 end
	 F = T{$};
      end
   end
endfunction
