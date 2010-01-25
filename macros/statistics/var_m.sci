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


function [V] = variance(x, varargin, varargopt)
// obsolete function 
  printf("\nWARNING: variance is obsolete, use var instead\n")
  V = var(x, varargin(:), varargopt(:))
endfunction

function [V] = var_m(x, varargin, varargopt)

// varargin should be only of size 1 (at max) with the dim argument
// In this case the dim field should not be in the varargopt hash table
   
   dim = 0  // default value
   skip_nan = %f // default value
   unbiased = %t // default value
   weighted_var = %f;
   narg_opt = 0

   if numel(varargin) >= 1 then
      dim = varargin(1);
      if varargopt.iskey["dim"] then
	 error("Error: dim argument both specified as optional and named optional args")
      end
      if numel(varargin) > 1 then
	 error("Error: only one optional argument is correct for var or std")
      end
   end
   
   if  varargopt.iskey["dim"] then, dim = varargopt.dim; narg_opt = narg_opt+1; end
   
   // verify dim arg
   dim = parse_dim_arg(dim, names=["dim","var or std"]);
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
   
   if varargopt.iskey["unbiased"] then
      unbiased = varargopt.unbiased
      narg_opt = narg_opt+1;
      if ~ (is(unbiased,%types.BMat) && isscalar(unbiased)) then
	 error("Error: unbiased should be a boolean scalar")
      end
   end   
   
   if varargopt.iskey["weights"] then
      weights = varargopt.weights
      narg_opt = narg_opt+1;
      if ~ (is(weights,%types.Mat) && isreal(weights) && and(size(x)==size(weights)) ) then
	 error("Error: weights should be a real vector or matrix of same dimensions than first arg")
      end
      if min(weights) < 0 then
	 error("Error: weights should be non negative")
      end
      weighted_var = %t;
   end   

   if numel(varargopt) > narg_opt then
      error("Error: only ''dim'', ''skip_nan'' and ''unbiased'' could be optional named arguments")
   end
   
   if ~weighted_var then
      if ~skip_nan then
	 if dim == 0 then
	    mn = numel(x)
	    if mn == 0 then, V = %nan, return, end
	    mean = sum(x)/mn
	    if unbiased then, denom = mn-1, else, denom = mn, end
	    x = x - mean
	    V = dot(x,x)/denom
	 elseif dim == 1 then
	    [m,n] = size(x)
	    mean = sum(x,1)/m
	    if unbiased then, denom = max(m-1,0), else, denom = m, end
	    x.blas_ger[-1, ones(m,1),mean]
	    V = dot(x,x,dim=1)/denom
	 else // dim == 2
	    [m,n] = size(x)
	    mean = sum(x,2)/n
	    if unbiased then, denom = max(n-1,0), else, denom = n, end
	    x.blas_ger[-1,mean,ones(n,1)]
	    V = dot(x,x,dim=2)/denom
	 end
      else  // skip nan 
	 mask = isnan(x);
	 imask = find(mask);
	 x(imask) = 0
	 len = sum(~mask,dim)
	 mean = sum(x, dim)./len
	 if unbiased then, denom = len-1, else, denom = len, end
	 denom = max(denom,0)
	 [m,n] = size(x)
	 if dim == 0 then
	    x = x-mean; x(imask) = 0
	    V = dot(x,x)/denom
	 elseif dim == 1 then
	    x.blas_ger[-1, ones(m,1),mean]; x(imask) = 0
	    V = dot(x,x,dim=1)./denom
	 else // dim == 2
	    x.blas_ger[-1,mean,ones(n,1)]; x(imask) = 0
	    V = dot(x,x,dim=2)./denom
	 end
      end

   else    // -- weighted variance
      if skip_nan then
	 mask = isnan(x)
	 imask = find(mask)
	 x(imask) = 0; weights(imask) = 0;
      end
      select dim
	case 0 then
	   weights = weights/sum(weights)        // normalize weights
	   mean = sum(weights.*x);
	   x = x-mean;
	case 1 then
	   m = size(x,1);
	   weights.scale_cols[1./sum(weights,1)] // normalize weights
	   mean = sum( weights.*x,1);
	   x.blas_ger[-1, ones(m,1),mean]
	case 2 then
	   n = size(x,2);
	   weights.scale_rows[1./sum(weights,2)] // normalize weights
	   mean = sum( weights.*x,2);
	   x.blas_ger[-1, mean, ones(n,1)]
      end
      V = dot(x,weights.*x,dim)
      if unbiased then  // add correction (valid when w = 1/n)
	 V = V./(1 - dot(weights,weights,dim));
      end
   end

   if ~isreal(x) then, V = real(V), end
   
endfunction
