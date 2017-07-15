// Copyright (C) 2007-2015 David Bateman
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
// 
// 2017: This file is coming from octave and adapted to nsp
//       Jean-Philippe Chancelier.

function T=delaunayn(pts,varargin)
  if nargin < 1 then 
    error("delaunayn: should be called with at least one argument\n");
    return;
  end
  T = delaunayn__(pts, varargin(:));

  if %f then 
    // Try to remove the zero volume simplices.  The volume of the i-th simplex is
    // given by abs(det(pts(T(i,1:end-1),:)-pts(T(i,2:end),:)))/prod(1:n)
    // (reference http://en.wikipedia.org/wiki/Simplex).  Any simplex with a
    // relative volume less than some arbitrary criteria is rejected.  The
    // criteria we use is the volume of the simplex corresponding to an
    // orthogonal simplex is equal edge length all equal to the edge length of
    // the original simplex.  If the relative volume is 1e3*eps then the simplex
    // is rejected.  Note division of the two volumes means that the factor
    // prod(1:n) is dropped.
    tol = 1e3 * %eps;
    idx = [];
    [nt,n] = size(T);
    // FIXME: Vectorize this for loop or convert delaunayn to .oct function
    // dim= size(pts,2);
    for i = 1: nt
      X = pts(T(i, (1 : $ - 1)), :) - pts(T(i, (2 : $)), :);
      sumsqX= sum(X.*X,2);
      vr = abs(det(X)) / sum(sqrt(sumsqX));
      if %f then 
	// an other way to compute the volume 
	pts1= pts(T(i,:), :);
	[_v,vol]=convhulln(pts1); 
	vr1= (vol *prod(1:dim))/ sum(sqrt(sumsqX));
	if abs(vr1-vr) >= 100*%eps then pause;end 
      end
      if vr < tol then
	idx($ + 1) = i;
	printf("Warning: Removing small simplex\n");
      end
    end
    T(idx, :) = [];
  end
endfunction

//!testif HAVE_QHULL
//! x = [-1, 0; 0, 1; 1, 0; 0, -1; 0, 0];
//! assert (sortrows (sort (delaunayn (x), 2)), [1,2,5;1,4,5;2,3,5;3,4,5]);

// Test 3-D input
//!testif HAVE_QHULL
//! x = [-1, -1, 1, 0, -1]; y = [-1, 1, 1, 0, -1]; z = [0, 0, 0, 1, 1];
//! assert (sortrows (sort (delaunayn ([x(:) y(:) z(:)]), 2)), [1,2,3,4;1,2,4,5])

