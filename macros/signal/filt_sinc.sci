function [x]=filt_sinc(n,fl)
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 1987-2016 - C. Bunks  (INRIA)
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
//x=sinc(n,fl)
//Calculate n samples of the function sin(2*pi*fl*t)/(pi*t)
//for t=-n/2:n/2 (i.e. centered around the origin).
//  n  :Number of samples
//  fl :Cut-off freq. of assoc. low-pass filter in Hertz
//  x  :Samples of the sinc function

  no2  = (n-1)/2;
  ino2 = int(no2);
  wl   = fl*2*%pi;
  xn   = sin(wl*(-no2:no2));
  xd   = %pi*(-no2:no2);
  if ino2==no2 then
    xn(no2+1) = 2*fl;
    xd(no2+1) = 1;
  end
  x=xn./xd;
endfunction
