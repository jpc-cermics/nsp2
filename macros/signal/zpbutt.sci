function [pols,gain]=zpbutt(n,omegac)
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
  
//<pols,gain>=zpbutt(n,omegac)
//Computes the poles of a Butterworth analog
//filter of order n and cutoff frequency omegac
//transfer function H(s) is calculated by
//  H(s) = gain/real(poly(pols,'s'))
//  n      :Filter order
//  omegac :Cut-off frequency in Hertz
//  pols   :Resulting poles of filter
//  gain   :Resulting gain of filter
//
// Author F.D.
// Revised by C. Bunks Oct. 24, 1996  
// 
  angles=ones(1,n)*(%pi/2+%pi/(2*n))+(0:n-1)*%pi/n;
  pols=omegac*exp(%i*angles);
  gain=abs((-omegac)^n);
endfunction
