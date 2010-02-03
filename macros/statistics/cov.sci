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

function [varargout] = cov(X, varargin, varargopt)

// varargin should be only of size 1 (at max) and corresponds to calls of
// the form: cov(X,Y)

   if nargin < 1 then
      error("Error: cov needs at least one input argument")
   end
   if nargout > 2 then
      error("Error: only one or two output arguments")
   end
   
   if ~( is(X,%types.Mat) && isreal(X,%t)) then
      error("Error: first argument should be a real matrix or vector")
   end
   
   skip_nan = %f // default value
   unbiased = %t // default value
   narg_opt = 0
   xy = %f;     // flag for cov(X,Y [,option=...) form
   weighted_cov = %f;
   
   if numel(varargin) >= 1 then
      Y = varargin(1);
      if ~ (is(Y,%types.Mat) && isreal(Y) && numel(Y)==numel(X))
	 error("Error: Y should be a real matrix with the same number of elements than X")
      end
      if numel(varargin) > 1 then
	 error("Error: bad call to cov, usage is cov(X [,option=]), or cov(X,Y [,option=])")
      end
      xy = %t   
   end
   
   if numel(varargopt) >= 1 then
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
	 if ~ (is(weights,%types.Mat) && isreal(weights) && isvector(weights) ...
	       && and(size(x,1)==numel(weights)) ) then
	    error("Error: weights should be a real vector with size(x,1) elements")
	 end
	 if min(weights) < 0 then
	    error("Error: weights should be non negative")
	 end
	 weighted_cov = %t;
      end   

      if numel(varargopt) > narg_opt then
	 error("Error: only ''skip_nan'', ''unbiased'' and ''weights'' could be optional named arguments")
      end
   end

   if xy then, X = [X(:), Y(:)], end
   
   if skip_nan then 
      // remove all observations for which at least one variable is %nan
      ind = find(~or(isnan(X),2))
      X = X(ind,:);
      if weigthed_cov then
	 weights = weights(ind,:);
      end
   end

   if ~weighted_cov then
      [m,n] = size(X)
      mean = sum(X,1)/m
      X.blas_ger[-1,ones(m,1),mean]
      if unbiased then, denom = m-1, else, denom = m, end
      cv = pmult(X,X) / denom
   else
      weights = weigths/sum(weights) // normalize weights
      [m,n] = size(X)
      mean = sum(scale_rows(X,weights),1)
      X.blas_ger[-1,ones(m,1),mean]
      cv = pmult(scale_rows(X,weights),X)
      if unbiased then
	 cv = cv/(1 - dot(weights,weights))
      end
   end

   varargout = list(cv)
   if nargout == 2 then
      sigma = sqrt(diag(cv))
      cor = cv ./ (sigma*sigma')
      cor.set_diag[ones(size(cor,1),1)]
      varargout.add_last[cor]
   end

endfunction
