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

//  computes the median of the absolute deviations around the median
//  (default or when flag="median") or the mean of the absolute
// deviations around the mean (flag="mean").

function [md] = mad(x, varargin, varargopt)

// varargin should be only of size 1 (at max) with the dim argument
// In this case the dim field should not be in the varargopt hash table
   
   if ~( 1 <= nargin && nargout <= 1 )
      error("Error: bad calling sequence, usage: mad(x, dim=, skip_nan=, flag=)")
   end

   if ~ (is(x,%types.Mat) && isreal(x,%t)) then
      error("Error: first argument should be a real matrix or vector")
   end
   
   dim = 0  // default value
   skip_nan = %f // default value
   flag = 1   // default value (correspond to "median")
   narg_opt = 0
   
   if numel(varargin) >= 1 then
      dim = varargin(1);
      if varargopt.iskey["dim"] then
	 error("Error: dim argument both specified as optional and named optional args")
      end
      if numel(varargin) > 1 then
	 error("Error: only one optional argument is correct for mad")
      end
   end
   
   if numel(varargopt) >= 1 then
      if  varargopt.iskey["dim"] then, dim = varargopt.dim; narg_opt = narg_opt+1; end
   
      if varargopt.iskey["skip_nan"] then
	 skip_nan = varargopt.skip_nan
	 narg_opt = narg_opt+1;
	 if ~ (is(skip_nan,%types.BMat) && isscalar(skip_nan)) then
	    error("Error: skip_nan should be a boolean scalar")
	 end
      end   
   
      if varargopt.iskey["flag"] then
	 flag = varargopt.flag
	 narg_opt = narg_opt+1;
	 if ~ (is(flag,%types.SMat) && isscalar(flag)) then
	    error("Error: flag should be a string")
	 end
	 flag = is_string_in_array(flag,["median","mean"],names=["flag","mad"])
      end   
   
      if numel(varargopt) > narg_opt then
	 error("Error: only ''dim'', ''skip_nan'' and ''flag'' could be optional named arguments")
      end
   end

   // verify dim arg
   if ~dim.equal[0] then
      dim = parse_dim_arg(dim, names=["dim","median"]);
      if dim > 2 then
	 error("Error: matrix with more than 2 dimensions not currently available")
      elseif dim == -1 then
	 error("Error: dim should not be equal to -1")
      elseif dim == -2 then // matlab compat
	 if isvector(x) then, dim = 0, else, dim = 1, end
      end     
   end

   
   if flag == 1 then // flag == 1 corresponds to median of the absolute deviation around the median
      m = median(x, dim, skip_nan = skip_nan);
      if dim == 0 then
	 md = median(abs(x-m),dim, skip_nan = skip_nan); 
      elseif dim == 1 then
	 x.blas_ger[-1,ones(size(x,1),1),m]
	 md = median(abs(x),dim, skip_nan = skip_nan); 
      elseif dim == 2 then
	 x.blas_ger[-1,m,ones(size(x,2),1)]
	 md = median(abs(x),dim, skip_nan = skip_nan); 
      end
   else // flag == 1 corresponds to mean of the absolute deviation around the mean
      m = mean(x, dim, skip_nan = skip_nan);
      if dim == 0 then
	 md = mean(abs(x-m),dim, skip_nan = skip_nan); 
      elseif dim == 1 then
	 x.blas_ger[-1,ones(size(x,1),1),m]
	 md = mean(abs(x),dim, skip_nan = skip_nan); 
      elseif dim == 2 then
	 x.blas_ger[-1,m,ones(size(x,2),1)]
	 md = mean(abs(x),dim, skip_nan = skip_nan); 
      end
   end

endfunction
