function cels=casc(x,z)
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 1987-2016 - F. Delebecque et all (INRIA)
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
  
//cels=casc(x,z)
//Creates cascade realization of filter
//from a matrix of coefficients
//  x    :(4xN)-Matrix where each column is a cascade
//       :element, the first two column entries being
//       :the numerator coefficients and the second two
//       :column entries being the denominator coefficients
//  z    :Character string representing the cascade variable
//  cels :Resulting cascade representation
//
//EXAMPLE:
//  x=[ 1.     2.     3.  ;
//      4.     5.     6.  ;
//      7.     8.     9.  ;
//      10.    11.    12. ]
//
//  cels=casc(x,'z')
//  cels      =
//
//  !             2               2               2  !
//  !   1 + 4z + z      2 + 5z + z      3 + 6z + z   !
//  !  ------------    ------------    ------------  !
//  !              2               2               2 !
//  !   7 + 10z + z     8 + 11z + z     9 + 12z + z  !
// Author: F. D.  date: August 1988
// 
  if nargin == 1 then z='z';end
  // create an empty rational matrix
  cels = p2r(m2p([]));
  for i=1:size(x,'c');
    col=x(:,i);
    nf=[col(1:2);1];
    nd=[col(3:4);1];
    cels(i)= p2r(poly(nf,z,'c'),poly(nd,z,'c'));
  end
endfunction
