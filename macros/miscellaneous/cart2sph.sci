function [theta,phi,r] = cart2sph_m_m(x,y,z,colat=%f)

// Copyright (C) 2007 Bruno Pinçon
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
// purpose
// -------  
//  translate cartesian coordinates to spherical ones
//  theta = azimut angle
//    phi = latitude (or elevation) angle if colat=%f
//        = colatitude angle if colat=%t
//      r = radius, euclidian distance |OM| M(x;y;z), O(0;0;0)
//
//  each input argument should be both a scalar or of same dimensions
//  than others. Moreover all input arguments should be real
  
  if ( ~isreal(x) | ~isreal(y) | ~isreal(z) )
    error(" arguments should be real vectors or matrices")
  end
  rxy = hypot(x,y)
  theta = atan(y,x)
  r = hypot(rxy,z)
  if colat then
    phi = atan(rxy,z)
  else
    phi = atan(z,rxy)
  end
endfunction
