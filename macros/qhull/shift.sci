// Copyright (C) 1995-2012 Kurt Hornik
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
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
// Author: AW <Andreas.Weingessel@ci.tuwien.ac.at>
// Created: 14 September 1994
// Adapted-By: jwe
// 
// 2017: This file is coming from octave and adapted to nsp
//       Jean-Philippe Chancelier.

function y = shift (x, b, dim)
  if (nargin ~= 2 && nargin ~= 3)
    error ("Error: shift(x,y [,dim]);\n");
  end
  if (numel (x) < 1)
    error ("shift: X must not be empty\n");
    return;
  elseif ( ~(isscalar (b) && b == fix (b)))
    error ("shift: B must be an integer\n");
    return;
  end
  
  nd = 2; // XXXX  ndims (x);
  sz = size (x);
  
  if (nargin == 3)
    if ( ~(isscalar (dim) && dim == fix (dim)) ||
      ~(1 <= dim && dim <= nd))
      error ("shift: DIM must be an integer and a valid dimension");
    end
  else
    // Find the first non-singleton dimension.
    dim = find (sz > 1);
    if size(dim,'*') == 0 then dim = 1; else dim = dim(1);end
  end
  d = sz(dim);
  
  function z=rem(x,y), z=x - y .* fix (x ./ y);endfunction
  
  idx = repmat ({':'}, nd, 1);
  for i=1:nd; idx{i}=1:size(x,i);end 
  if (b > 0)
    b = rem (b, d);
    idx{dim} = [d-b+1:d, 1:d-b];
  elseif (b < 0)
    b = rem (abs (b), d);
    idx{dim} = [b+1:d, 1:b];
  end
  y = x(idx{:});
endfunction

if %f then 
// test
 a = [1, 2, 3];
 b = [4, 5, 6];
 c = [7, 8, 9];

 r = [a, b, c];
 m = [a; b; c];

 if ~shift(r, 0).equal[r] then pause;end 
 if ~shift(r, 3).equal[[c, a, b]] then pause;end 
 if ~shift(r, -6).equal[[c, a, b]] then pause;end 
 if ~shift(r, -3).equal[[b, c, a]] then pause;end 
 if ~shift(m, 1).equal[[c; a; b]] then pause;end 
 if ~shift(m, -2).equal[[c; a; b]] then pause;end 
end


