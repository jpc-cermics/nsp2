function [m]=find_freq(epsilon,A,n)
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
  
//Search for m such that n=K(1-m1)K(m)/(K(m1)K(1-m))
//with m1=(epsilon*epsilon)/(A*A-1);
//If  m = omegar^2/omegac^2,the parameters
//epsilon,A,omegac,omegar and n are then
//compatible for defining a prototype elliptic filter.
//  epsilon :Passband ripple
//  A       :Stopband attenuation
//  n       :filter order
//  m       :Frequency needed for construction of
//          :elliptic filter
//
//Author F.D.

  m1=(epsilon*epsilon)/(A*A-1);
  chi1=%k(1-m1)/%k(m1);
  m=findm(chi1/n);
  
endfunction
