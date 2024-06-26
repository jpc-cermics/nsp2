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
// emulates meshgrid with B. Pincon ndgrid function
// Copyright (C) 2016-2022 - Jean-Philippe Chancelier

function [xx,yy,zz]=meshgrid(x,y,z)
  if (nargin==0 || nargin > 3) then
    error("Error: [xx, yy, zz] = meshgrid (x, y, z)\n");
    return
  end

  if (nargin < 2) then y=x;end

  if (nargout < 3) then
    [xx,yy]=ndgrid(x(:),y(:));
  else
    if (nargin < 3) then z=y;end
    [xx,yy]=ndgrid(x(:),y(:),z(:));
  end
endfunction
