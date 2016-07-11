function [xm,fr]=frmag(num,den,npts)
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 1987-2016 - C. Bunks (INRIA)
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
  
// [xm,fr]=frmag(num[,den],npts)
// Calculates the magnitude of the frequency respones of
// FIR and IIR filters.  
// The filter description can be
//   one or two vectors of coefficients, 
//   one or two polynomials,
//   or a rational polynomial.
// Case 1 (When den is not given):
//   num  :Vector coefficients/Polynomial/Rational
//        :polynomial of filter
// Case 2 (When den is given):
//   num  :Vector coefficients/Polynomial of filter numerator
//   den  :Vector coefficients/Polynomial of filter denominator
// Case 1 and 2:
//   npts :Number of points desired in frequency response
//   xm   :Magnitude of frequency response at the points fr
//   fr   :Points in the frequency domain where
//        :magnitude is evaluated
//
// Author: C. Bunks   1988-2016 
  
  if nargin==2 then
    npts=den;
  end
  fr=(0:.5/(npts-1):.5);
  dfr=exp(2*%i*%pi*fr);
  if nargin==2 then
    select type(num,'short')
     case 'r' then  xm=abs(freq(num.num,num.den,dfr));
     case 'p' then  xm=abs(freq(num,poly(1,'z','c'),dfr));
     case 'm' then  xm=abs(freq(poly(num,'z','c'),poly(1,'z','c'),dfr));
    else
      error('Error: input argument has wrong type");
      return;
    end
  elseif nargin==3 then
    select type(num,'short')
     case 'p' then xm=abs(freq(num,den,dfr));
     case 'm' then xm=abs(freq(poly(num,'z','c'),poly(den,'z','c'),dfr));
    else
      error('Error: input argument has wrong type");
    end
  end
endfunction

