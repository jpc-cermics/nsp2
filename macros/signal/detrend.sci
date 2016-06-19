function [y] = detrend(x, flag, bp)
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 1987-2016 - Bruno Pincon 
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
//
// this function removes the constant or linear or
// piecewise linear trend from a vector x. If x is
// matrix this function removes the trend of each 
// column of x.
//
// flag = "constant" or "c" to removes the constant trend 
//        (simply the mean of the signal)
// flag = "linear" or "l" to remove the linear or piecewise 
//        linear trend.
//  
// To define the piecewise linear trend the function needs the
// breakpoints and these must be given as the third argument bp.
//
// The "instants" of the signal x are in fact from 0 to m-1 
// (m = length(x) if x is a vector and m = size(x,1) in case
// x is a matrix). So bp must be reals in [0 m-1].
//
// AUTHOR
// Bruno Pincon
//

  if nargin < 1 | nargin > 3 then
    error("detrend: bad number of arguments")
  elseif nargin == 1
    flag = "linear"; bp = []
  elseif nargin == 2
    bp = []
  end
  
  if type(x,'short') <> 'm' then
    error("detrend: x must be a vector or matrix of numbers")
  end
  if type(flag,'short') <> 's' then
    error("detrend: flag must be a string")
  end
  if ~(type(bp,'short')=='m' && isreal(bp)) then
    error("detrend: breakpoints must be a vector of real numbers")
  end
  
  [mx,nx] = size(x)
  if mx==1 | nx==1 then
    x_is_vector = %t; x = x(:); m = mx*nx; n = 1
  elseif mx*nx == 0 then
    y = []; return
  else
    x_is_vector = %f; m = mx; n = nx
  end
  
  
  if flag == "constant" | flag == "c" then
    y = x - ones(m,1)*mean(x,"r")
  elseif flag == "linear" | flag == "l"
    bp = unique([0 ; bp(:) ; m-1])
    // delete all the breakpoints outside [0,m-1]
    while bp(1) < 0, bp(1)=[], end
    while bp($) > m-1, bp($)=[], end
    // breakpoints are 0-based so add one to
    // compare them with signal vector indices (1-based)
    bp = bp + 1;  
    nbp = length(bp);
    // build the least square matrix with hat functions
    // (as a basis for continuous piecewise linear functions)
    A = zeros(m, nbp)
    k1 = 1
    delta_bp = diff(bp)
    for j = 2:nbp-1
      k2 = ceil(bp(j))-1
      ind = (k1:k2)'
      A(ind,j-1) = (bp(j) - ind)/delta_bp(j-1)
      A(ind,j) = (ind - bp(j-1))/delta_bp(j-1)
      k1 = k2+1
    end
    ind = (k1:m)'
    A(ind,nbp-1) = (m - ind)/delta_bp(nbp-1)
    A(ind,nbp) = (ind - bp(nbp-1))/delta_bp(nbp-1)
    // solve the least square pb and retrieve the fitted 
    // piecewise linear func off the signal
    y = x - A*(A\x)
  else
    error("detrend: unknown flag specifier")
  end

  if x_is_vector then
    y = matrix(y,mx,nx)
  end

endfunction      
