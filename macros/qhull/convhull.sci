// Copyright (C) 2000-2017 Kai Habel
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
// Author: Kai Habel <kai.habel@gmx.de>
// 
// 2017: This file is coming from octave and adapted to nsp
//       Jean-Philippe Chancelier.

function H=convhull(x,y,options)
  
  if (nargin ~= 2 && nargin ~= 3)
    error("convhull: expects 2 or 3 arguments\n");
    return;
  end
  
  // convhulln expects column vectors
  x = x(:);
  y = y(:);
  
  if (length(x) ~= length(y))
    error("convhull: X and Y must have the same size\n");return;
  end
  
  if nargin == 3 then 
    if type(options,'short')=='s' then 
      options = catenate(options,sep= " ");
    elseif type(options,'short') == 'ce' then 
      options = ce2s(options)
    else
      error("convhull: OPTIONS must be a string matrix or a cell\n");
      return
    end
  end
  // call convhulln with use_2d=%t to obtain a column vector 
  // as result 
  if (nargin == 2)
    H = convhulln([x,y],use_2d=%t);
  else 
    H = convhulln([x,y], options,use_2d=%t);
  end
endfunction

if %f then 
  x = -3:0.05:3;
  y = abs (sin (x));
  k = convhull (x, y);
  rect= [-3.05, 3.05, -0.05, 1.05];
  plot (x(k),y(k),"r-", x,y,"b+");
  axis(rect);
  x = -3:0.5:3;
  y = abs (sin (x));
  if ~convhull (x, y).equal[[1;7;13;12;11;10;4;3;2;1]] then pause;end
end



