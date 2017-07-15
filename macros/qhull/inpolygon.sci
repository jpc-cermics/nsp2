// Copyright (C) 2006-2012 Frederick (Rick) A Niles
//               and Soren Hauberg
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
// Author: Frederick (Rick) A Niles <niles@rickniles.com>
// Created: 14 November 2006
// Vectorized by Soren Hauberg <soren@hauberg.org>
// The method for determining if a point is in in a polygon is based on
// the algorithm shown on
// http://local.wasp.uwa.edu.au/~pbourke/geometry/insidepoly/ and is
// credited to Randolph Franklin.
//
// 
// 2017: This file is coming from octave and adapted to nsp
//       Jean-Philippe Chancelier.

function [in, on] = inpolygon (x, y, xv, yv)
  if (nargin ~= 4)
    error('Error: [in, on] = inpolygon (x, y, xv, yv)\n");
    return;
  end

  if (~ (isreal (x) && isreal (y) && type(x,'short')=='m' && type(y,'short')=='m' && size(x).equal[size(y)]))
    error ("inpolygon: first two arguments must be real matrices of same size");
    return;
  end
  
  if (~ (isreal (xv) && isreal (yv) && type(xv,'short')=='m' && type(yv,'short')=='m' ...
	 && size(xv).equal[size(yv)]))
    error ("inpolygon: last two arguments must be real vectors of same size");
    return;
  end

  npol = max(size(xv)); // length 
  do_boundary = (nargout >= 2);

  in =bmat_create(size(x,1),size(x,2),%f);// <=> mzeros (size(x), "logical");
  if (do_boundary);  on = in ; end

  j = npol;
  for i = 1 : npol
    delta_xv = xv(j) - xv(i);
    delta_yv = yv(j) - yv(i);
    // distance = [distance from (x,y) to edge] * length(edge)
    distance = delta_xv .* (y - yv(i)) - (x - xv(i)) .* delta_yv;
    //
    // is y between the y-values of edge i,j
    //        AND (x,y) on the left of the edge ?
    idx1 = (((yv(i) <= y & y < yv(j)) | (yv(j) <= y & y < yv(i))) ...
            & 0 < distance.*delta_yv);
    if or(idx1) then 
      // nsp bug June 2012 thus we must check or(idx1)
      in (idx1) = ~in (idx1);
    end
    // Check if (x,y) are actually on the boundary of the polygon.
    if (do_boundary)
       idx2 = (((yv(i) <= y & y <= yv(j)) | (yv(j) <= y & y <= yv(i))) ...
               & ((xv(i) <= x & x <= xv(j)) | (xv(j) <= x & x <= xv(i))) ...
               & (0 == distance | ~m2b(delta_xv)));
       on (idx2) = %t;
    end
    j = i;
  end
endfunction

if %f then 
  xv=[ 0.05840, 0.48375, 0.69356, 1.47478, 1.32158,...
       1.94545, 2.16477, 1.87639, 1.18218, 0.27615,...
       0.05840 ];
  yv=[ 0.60628, 0.04728, 0.50000, 0.50000, 0.02015,...
       0.18161, 0.78850, 1.13589, 1.33781, 1.04650,...
       0.60628 ];
 xa=[0:0.1:2.3];
 ya=[0:0.1:1.4];
 [x,y]=meshgrid(xa,ya);
 [in,on]=inpolygon(x,y,xv,yv);

 inside=in & ~on;
 plot(xv,yv)
 //hold on
 plot(x(inside),y(inside),"+4g")
 plot(x(~in),y(~in),"+4m")
 plot(x(on),y(on),"+4b")
 //hold off
 printf("Green points are inside polygon, magenta are outside,");
 printf("and blue are on boundary.");
end 

if %f then 
  xv=[ 0.05840, 0.48375, 0.69356, 1.47478, 1.32158, ...
       1.94545, 2.16477, 1.87639, 1.18218, 0.27615, ...
       0.05840, 0.73295, 1.28913, 1.74221, 1.16023, ...
       0.73295, 0.05840 ];
  yv=[ 0.60628, 0.04728, 0.50000, 0.50000, 0.02015, ...
       0.18161, 0.78850, 1.13589, 1.33781, 1.04650, ...
       0.60628, 0.82096, 0.67155, 0.96114, 1.14833, ...
       0.82096, 0.60628];
 xa=[0:0.1:2.3];
 ya=[0:0.1:1.4];
 [x,y]=meshgrid(xa,ya);
 [in,on]=inpolygon(x,y,xv,yv);

 inside=in & ~ on;
 plot2d(xv,yv)
 //hold on
 plot2d(x(inside),y(inside),style=-4) // lozenges 
 plot2d(x(~in),y(~in),style=-3) // stars 
 plot2d(x(on),y(on),style=-2); // cross 
 //hold off
 printf("lozenges points are inside polygon, stars are outside,");
 printf("and cross are on boundary.");
end

if %f then 
  [in, on] = inpolygon ([1, 0], [1, 0], [-1, -1, 1, 1], [-1, 1, 1, -1]);
  if ~in.equal[[%f, %t]] then pause; end;
  if ~on.equal[[%t, %f]] then pause; end;
end 
  
