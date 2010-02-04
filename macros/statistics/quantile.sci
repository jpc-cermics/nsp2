// Copyright (C) Bruno Pincon
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

// a R/octave like 's quantile function (with the nine methods
// to compute quantiles).
// when dim is different from 0 the function is called
// recursively and test_arg is used to bypass all the
// argument tests stuff in the recursive calls

function q = quantile(x, p, dim=0, skip_nan=%f, meth=5, test_arg=%t)

   if test_arg then
      if ~( 2 <= nargin && nargin <= 6 && nargout <= 1 ) then
	 error("Error: bad calling sequence, usage: q = quantile(x,p,dim=,skip_nan=,meth=)")
      end
      if ~( is(x,%types.Mat) && isreal(x,%t) ) then
	 error("Error: first argument should be a real matrix or vector")
      end
      if ~( is(p,%types.Mat) && isreal(p,%t) && isvector(p) ) then
	 error("Error: 2d arg of should be a real vector")
      end

      if ~dim.equal[0] then  // verify dim arg
	 dim = parse_dim_arg(dim, names=["dim","quantile"]);
	 if dim > 2 then
	    error("Error: matrix with more than 2 dimensions not currently available")
	 elseif dim == -1 then
	    error("Error: dim should not be equal to -1")
	 elseif dim == -2 then // matlab compat
	    if isvector(x) then, dim = 0, else, dim = 1, end
	 end
      end
      
      if ~ (is(skip_nan,%types.BMat) && isscalar(skip_nan)) then
	 error("Error: skip_nan should be a boolean scalar")
      end

      if ~ (is(meth,%types.Mat) && isreal(meth,%t) && isscalar(meth) && ...
	    floor(meth)==meth && 0 <= meth && meth <= 9 ) then
	 error("Error: meth should be an integer in [0,9]")
      end
      
   end
   
   select dim
     case 0 then
	[m,n] = size(x);
	if n == 1 then
	   p.redim[-1,1]
	elseif m == 1 then
	   p.redim[1,-1];
	else
	   x.redim[-1,1]; p.redim[-1,1];
	end
	   
	if ~issorted(x,flag="g",order_nan=%t) then
	   if meth <= 3
	      x = sort(x,"g","i")
	   else
	      x = sort(x,"gb","i")
	   end
	end
	n = numel(x)
	if skip_nan then
	   while n >= 1 && isnan(x(n)), n.add[-1], end
	   x(n+1:$) = [];
	end

	if n == 0 then 
	   q = %nan*ones(size(p))
	   return
	end
	
	select meth
	  case 1 then
	     q = x(max(1,ceil(n*p)))
	  case 2 then
	     np = n*p
	     j = ceil(np)
	     q = x(max(1,j))
	     ind = find(j==np & j ~= 0 & j ~= n)
	     jind = j(ind)
	     q(ind) = 0.5*(x(jind)+x(jind+1))
	  case 3 then   
	     // m = -0.5
	     npm05 = n*p - 0.5;
	     j = floor(npm05)
	     q = x(max(1,min(n,j+1)))
	     ind = find(j==npm05 & j ~= 0 & mod(j,2) == 0)
	     q(ind) = x(j(ind))
	  case 4 then
	     // m = 0
	     np = n*p
	     j = floor(np)
	     g = np - j;
	     q = (1-g).*x(max(j,1)) + g.*x(min(j+1,n))
	  case 5 then
	     // m = 0.5
	     npp05 = n*p+0.5
	     j = floor(npp05)
	     g = npp05 - j;
	     q = (1-g).*x(min(max(j,1),n)) + g.*x(min(j+1,n))
	  case 6 then
	     // m = p
	     np1p = (n+1)*p
	     j = floor(np1p)
	     g = np1p - j;
	     q = (1-g).*x(min(max(j,1),n)) + g.*x(min(j+1,n))
	  case 7 then
	     // m = 1 - p
	     nm1pp1 = (n-1)*p+1
	     j = floor(nm1pp1)
	     g = nm1pp1 - j;
	     q = (1-g).*x(min(max(j,1),n)) + g.*x(min(j+1,n))
	  case 8 then
	     m = (p+1)/3
	     j = floor(n*p+m)
	     g = n*p + m - j;
	     q = (1-g).*x(min(max(j,1),n)) + g.*x(min(j+1,n))
	  case 9 then
	     m = p/4 + 3/8
	     j = floor(n*p+m)
	     g = n*p + m - j;
	     q = (1-g).*x(min(max(j,1),n)) + g.*x(min(j+1,n))
	end
	
     case 1 then
	[m,n] = size(x)
	mnp = numel(p)
	q = zeros(mnp,n)
	for j = 1:n
	   q(:,j) = quantile(x(:,j),p,dim=0,meth=meth,skip_nan=skip_nan,test_arg=%f)
	end
	
     case 2 then
	[m,n] = size(x)
	mnp = numel(p)
	q = zeros(m,mnp)
	for i = 1:m
	   q(i,:) = quantile(x(i,:),p,dim=0,meth=meth,skip_nan=skip_nan,test_arg=%f)
	end
   end
   
endfunction
