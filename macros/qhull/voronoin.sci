// Copyright (C) 2000-2012 Kai Habel
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
// First Release: 20/08/2000
// 2003-12-14 Rafael Laboissiere <rafael@laboissiere.net>
// Added optional second argument to pass options to the underlying
// qhull command
//
// 
// 2017: This file is coming from octave and adapted to nsp
//       Jean-Philippe Chancelier.

function [C, F] = voronoin (pts, options)

  if (nargin ~= 1 && nargin ~= 2)
    error("voronoin: should be called with one or two arguments\n");
    return;
  end
  [np, dim] = size (pts);
  if (np <= dim)
    error ("voronoin: number of points must be greater than their"+...
	   " dimension\n");
    return;
  end
  caller = "voronoin";
  // take care that in nsp interface we need the transposed version of pts
  if (nargin == 1)
    [C, F] = voronoi_internal (caller, pts');
  else
    [C, F] = voronoi_internal (caller, pts', options);
  end
endfunction

//%% FIXME: Need functional tests
//%% FIXME: Need input validation tests

