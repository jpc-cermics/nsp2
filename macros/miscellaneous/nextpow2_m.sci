function p = nextpow2_m(n)

// Copyright (C) 2008 Bruno Pinçon
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
//  Purpose
//  -------
//    computes for each entry of n, p such that 2^(p-1) < |n| <= 2^p
//
   [m,e] = frexp(abs(n))
   p = m   // infinities and Nans are "passed" with frexp then to p here
   ind = find(m == 0.5)
   e(ind) = e(ind)-1
   ind = find(finite(m))
   p(ind) = e(ind)
endfunction
