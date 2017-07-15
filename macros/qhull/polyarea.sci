// Copyright (C) 1999-2012 David M. Doolin
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
// Author: David M. Doolin <doolin@ce.berkeley.edu>
// Date: 1999-04-14
// Modified-by:
//    2000-01-15 Paul Kienzle <pkienzle@kienzle.powernet.co.uk>
//    * use matlab compatible interface
//    * return absolute value of area so traversal order doesn't matter
//    2005-10-13 Torsten Finke
//    * optimization saving half the sums and multiplies
// 
// 2017: This file is coming from octave and adapted to nsp
//       Jean-Philippe Chancelier.

function a = polyarea (x, y, dim)
  if (nargin ~= 2 && nargin ~= 3)
    error("Error: polyarea(x,y [,dim])\n");
    return;
  end
  if ~size(x).equal[size(y)] 
    error ("polyarea: X and Y must have the same shape\n");
    return;
  end
  if size(x,'*')==0 then a=x;return;end
  if (nargin == 2)
    a = abs (sum (x .* (shift (y, -1) - shift (y, 1)),'r')) / 2;
  else
    a = abs (sum (x .* (shift (y, -1, dim) - shift (y, 1, dim)), dim)) / 2;
  end
endfunction

if %f then 
   x = [1;1;3;3;1];
   y = [1;3;3;1;1];
   if abs(polyarea(x,y)-4) > 10*%eps then pause;end
   if abs(polyarea([x,x],[y,y])-[4,4]) > 10*%eps then pause;end
   if abs(polyarea([x,x],[y,y],1)-[4,4]) > 10*%eps then pause;end
   if abs(polyarea([x,x]',[y,y]',2)-[4;4]) > 10*%eps then pause;end
end

