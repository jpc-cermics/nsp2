// Copyright (C) Jerome Lelong, Bruno Pincon
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

function [y] = mean_m(x, varargin, varargopt)

// usage:   mean(x)
//          mean(x,dim,skip_nan=, weights=, trim= )
//          mean(x,dim=, skip_nan=, weights=, trim= )

// varargin should be only of size 1 (at max) with the dim argument
// In this case the dim field should not be in the varargopt hash table
   
   dim = 0  // default value
   skip_nan = %f // default value
   trim = 0  // default value
   weighted_mean = %f 
   narg_opt = 0
   
   if numel(varargin) >= 1 then
      dim = varargin(1);
      if varargopt.iskey["dim"] then
	 error("Error: dim argument both specified as optional and named optional args")
      end
      if numel(varargin) > 1 then
	 error("Error: only one optional argument is correct for mean")
      end
   end
   
   if  varargopt.iskey["dim"] then, dim = varargopt.dim; narg_opt = narg_opt+1; end
   
   // verify dim arg
   dim = parse_dim_arg(dim, names=["dim","mean"]);
   if dim > 2 then
      error("Error: matrix with more than 2 dimensions not currently available")
   elseif dim == -1 then
      error("Error: dim should not be equal to -1")
   elseif dim == -2 then // matlab compat
      if isvector(x) then, dim = 0, else, dim = 1, end
   end     
   
   if varargopt.iskey["skip_nan"] then
      skip_nan = varargopt.skip_nan
      narg_opt = narg_opt+1;
      if ~ (is(skip_nan,%types.BMat) && isscalar(skip_nan)) then
	 error("Error: skip_nan should be a boolean scalar")
      end
   end   
   
   if varargopt.iskey["trim"] then
      trim = varargopt.trim
      narg_opt = narg_opt+1;
      if ~ (is(trim,%types.Mat) && isscalar(trim) && isreal(trim) &&  0 <= trim && trim <= 0.5) then
	 error("Error: trim should be a real number in [0,0.5]")
      end
      if ~isreal(x) then
	 error("Error: trimmed mean not implemented for complex numbers")
      end
	 
   end   
   
   if varargopt.iskey["weights"] then
      if trim ~= 0 then
	 error("Error: trimmed weighted mean is not implemented")
      end
      weights = varargopt.weights
      narg_opt = narg_opt+1;
      if ~ (is(weights,%types.Mat) && isreal(weights) &&  and(size(x)==size(weights))) then
	 error("Error: weights should be a real vector or matrix of same dimensions than first arg")
      end
      weighted_mean = %t;
   end   

   if numel(varargopt) > narg_opt then
      error("Error: only ''dim'', ''skip_nan'', ''trim'' and ''weights'' could be optional named arguments")
   end
   
   if trim == 0 then   
      if weighted_mean then   // weigthed mean
	 if skip_nan then
	    mask = isnan(x);
	    x(mask) = 0; weights(mask) = 0;
	    y = sum(x.*weights, dim)./sum(weights,dim)
	 else
	    y = sum(x.*weights, dim)./sum(weights,dim)
	 end
      else                    // usual mean
	 if skip_nan then
	    mask = isnan(x);
	    x(mask) = 0
	    y = sum(x, dim)./sum(~mask,dim)
	 else
	    y = sum(x, dim)/size(x, dim)
	 end
      end
   else                       // trimmed mean

      if isempty(x), then, y = mean(x,dim), return, end
      
      if dim == 0 then
	 x = sort(x, type="g", dir = "i")
	 n = numel(x);
	 if skip_nan then  // nan are put at the end with sort
	    while n >= 1 && isnan(x(n)), n.add[-1], end
	    if n == 0, then, y = %nan, return, end
	 end
	 ndiscard = floor(min(n*trim,(n-1)/2))
	 k1 = 1 + ndiscard
	 k2 = n - ndiscard
	 y = sum(x(k1:k2))/(k2 - k1 + 1)
	 
      elseif dim == 1 then
	 [m,n] = size(x);
	 if m == 0, then, y = mean(x,dim); return; end
	 x = sort(x, type="r", dir = "i")
	 if ~skip_nan then
	    mdiscard = floor(min(m*trim,(m-1)/2));
	    k1 = 1 + mdiscard
	    k2 = m - mdiscard
	    y = sum(x(k1:k2,:),1)/(k2 - k1 + 1)
	 else
	    y = zeros(1,n);
	    for j = 1:n
	       p = m
	       while p >= 1 && isnan(x(p,j)), p.add[-1], end
	       if p == 0 then
		  y(j) = %nan;
	       else
		  mdiscard = floor(min(p*trim,(p-1)/2))
		  k1 = 1 + mdiscard
		  k2 = p - mdiscard
		  y(j) = sum(x(k1:k2,j))/(k2 - k1 + 1)
	       end
	    end
	 end
	 
      else // dim == 2
	 x = sort(x, type="c", dir = "i")
	 [m,n] = size(x);
	 if ~skip_nan then
	    ndiscard = floor(min(n*trim,(n-1)/2))
	    k1 = 1 + ndiscard
	    k2 = n - ndiscard
	    y = sum(x(:,k1:k2),2)/(k2 - k1 + 1)
	 else
	    y = zeros(m,1);
	    for i = 1:m
	       p = n
	       while p >= 1 && isnan(x(i,p)), p.add[-1], end
	       if p == 0 then
		  y(i) = %nan
	       else
		  ndiscard = floor(min(p*trim,(p-1)/2))
		  k1 = 1 + ndiscard
		  k2 = p - ndiscard
		  y(i) = sum(x(i,k1:k2))/(k2 - k1 + 1)
	       end
	    end
	 end
      end
   end
endfunction
