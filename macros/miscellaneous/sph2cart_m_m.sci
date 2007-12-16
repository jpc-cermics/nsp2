function [x,y,z] = sph2cart_m_m(theta,phi,r,colat=%f)

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
//  translate spherical coordinates to cartesian ones
//  theta = azimut angle
//    phi = latitude (or elevation) angle if colat=%f
//        = colatitude angle if colat=%t
//      r = radius, euclidian distance |OM| M(x;y;z), O(0;0;0)
//  each input argument should be both a scalar or of same dimensions
//  than others. Moreover all input arguments should be real
  
  if ( ~isreal(theta) | ~isreal(phi) | ~isreal(r) )
    error(" arguments should be real vectors or matrices")
  end
  
  if colat then
    z = r.*cos(phi)
    rsinphi = r.*sin(phi)
    x = rsinphi.*cos(theta)
    y = rsinphi.*sin(theta)
  else
    z = r.*sin(phi)
    rcosphi = r.*cos(phi)
    x = rcosphi.*cos(theta)
    y = rcosphi.*sin(theta)
  end
endfunction
