// Copyright (C) 1999-2017 Kai Habel
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

function tri=delaunay(varargin)
  if (nargin < 1 || nargin > 4)
    error("delaunayn: should be called with at least one and up to 4 argument\n");
    return;
  end
  
  z = [];
  options = m2s([]);

  if type(varargin($),'short')== 's' then 
    options = catenate(varargin($),sep=" ");
    varargin.remove_last[];
  elseif type(varargin($),'short')== 'ce' then 
    options=ce2s(varargin($));
    options = catenate(options,sep=" ");
    varargin.remove_last[];
  end
  pts = [];
  for i = 1:length(varargin)
    xx=varargin(i);
    if type(xx,'short')<>'m' then 
      error(sprintf("delaunay: argument %d should be a real matrix\n"));
      return
    end
    if or(size(xx)==1) || length(varargin) > 1 then xx=xx(:);end
    if ~isempty(pts) && size(pts,1)<>size(xx,1) then 
      error("delaunay: argument %d has wrong number of rows %d expected\n",size(pts,1));
      return 
    end
    pts.concatr[xx];
  end
  if ~isempty(options) then 
    tri = delaunayn__(pts, options);
  else 
    tri = delaunayn__(pts);
  end
endfunction

if %f then 

   x = rand (1,10);
   y = rand (1,10);
   tri = delaunay (x,y);
   clf();
   triplot (tri, x, y);
   hold('on');
   plot (x, y, "r5*");
   axis ([0,1,0,1]);

   x = [-1, 0, 1, 0];
   y = [0, 1, 0, -1];
   h=delaunay(x,y);
   h = sort(sort(h, type='c', dir='i'), type='lr', dir='i');
   if ~h.equal[[1,2,4;2,3,4]] then pause;end 
   
   x = [-1, 0, 1, 0];
   y = [0, 1, 0, -1];
   h=delaunay([x(:),y(:)]);
   h = sort(sort(h, type='c', dir='i'), type='lr', dir='i');
   if ~h.equal[[1,2,4;2,3,4]] then pause;end 
      
   x = [1 5 2; 5 6 7];
   y = [5 7 8; 1 2 3];
   h=delaunay(x,y);
   h = sort(sort(h, type='c', dir='i'), type='lr', dir='i');
   if ~h.equal[[1,2,4;1,3,4;1,3,5;3,4,6]] then pause;end 
   
   x = [-1, -1, 1, 0, -1]; y = [-1, 1, 1, 0, -1]; z = [0, 0, 0, 1, 1];
   h=delaunay(x,y,z);
   h = sort(sort(h, type='c', dir='i'), type='lr', dir='i');
   if ~h.equal[[1,2,3,4;1,2,4,5]] then pause;end 
   
end
 
