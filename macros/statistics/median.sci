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

function [md] = median(x, varargin, varargopt)

// varargin should be only of size 1 (at max) with the dim argument
// In this case the dim field should not be in the varargopt hash table

   if nargin < 1 then
      error("Error: median needs at least one input argument")
   end
   
   if ~( is(x,%types.Mat) && isreal(x,%t)) then
      error("Error: first argument should be a real matrix or vector")
   end
   
   dim = 0  // default value
   skip_nan = %f // default value
   narg_opt = 0

   if numel(varargin) >= 1 then
      dim = varargin(1);
      if varargopt.iskey["dim"] then
	 error("Error: dim argument both specified as optional and named optional args")
      end
      if numel(varargin) > 1 then
	 error("Error: only one optional argument is correct for median")
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
   
      if numel(varargopt) > narg_opt then
	 error("Error: only ''dim'', and ''skip_nan'' could be optional named arguments")
      end
   end
   
   if ~dim.equal[0] then  // verify dim arg
      dim = parse_dim_arg(dim, names=["dim","median"]);
      if dim > 2 then
	 error("Error: matrix with more than 2 dimensions not currently available")
      elseif dim == -1 then
	 error("Error: dim should not be equal to -1")
      elseif dim == -2 then // matlab compat
	 if isvector(x) then, dim = 0, else, dim = 1, end
      end     
   end

   if dim == 0 then
      if ~issorted(x, flag="g", order_nan=%t) then
	 x = sort(x, type="g", dir="i")
      end
      n = numel(x);
      if skip_nan then  // nan are put at the end with sort
	 while n >= 1 && isnan(x(n)), n.add[-1], end
      end
      if mod(n,2) == 0 then
	 if n >= 2 then
	    md = 0.5*x(n/2) + 0.5*x(n/2+1)
	 else
	    md = %nan  // 
	 end
      else
	 md = x((n+1)/2)
      end
      
   elseif dim == 1 then
      [m,n] = size(x)
      // this shortcut avoid some tests after:
      if m == 0 then, md = %nan*ones(1,n); return; end

      x = sort(x, type="r", dir="i")
      
      if ~skip_nan then
	 if mod(m,2) == 0 then
	    md = 0.5*x(m/2,:) + 0.5*x(m/2+1,:)
	 else
	    md = x((m+1)/2,:)
	 end
      else
	 md = %nan*ones(1,n)
	 for j = 1:n
	    p = m
	    while p >= 1 && isnan(x(p,j)), p.add[-1], end
	    if mod(p,2) == 1 then
	       md(j)= x((p+1)/2,j)
	    elseif p >= 2 then
	       md(j)= 0.5*x(p/2,j) + 0.5*x(p/2+1,j)
	    end
	 end
      end
      
   else // dim == 2
      [m,n] = size(x)
      // this shortcut avoid some tests after:
      if n == 0 then, md = %nan*ones(m,1); return; end

      x = sort(x, type="c", dir="i")
      
      if ~skip_nan then
	 if mod(n,2) == 0 then
	    md = 0.5*x(:,n/2) + 0.5*x(:,n/2+1)
	 else
	    md = x(:,(n+1)/2)
	 end
      else
	 md = %nan*ones(m,1)
	 for i = 1:m
	    p = n
	    while p >= 1 && isnan(x(i,p)), p.add[-1], end
	    if mod(p,2) == 1 then
	       md(i)= x(i,(p+1)/2)
	    elseif p >= 2 then
	       md(i)= 0.5*x(i,p/2) + 0.5*x(i,p/2+1)
	    end
	 end
      end
   end 

endfunction
