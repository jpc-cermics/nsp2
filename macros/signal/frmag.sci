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
    if type(num,'short')=='h' then
      xm=abs(freq(num.n,num.d,dfr));
    elseif type(num,'short') == 'p' then 
      // rational case xm=abs(freq(num(2),num(3),dfr));
      xm=abs(freq(num,poly(1,'z','c'),dfr));
    elseif type(num,'short')=='m' then
      xz=poly(num,'z','c');
      xm=abs(freq(xz,1,dfr))
    else
      error('Error---Input arguments wrong data type')
    end
  elseif nargin==3 then
    if type(num,'short')=='p' then
      xm=abs(freq(num,den,dfr));
    elseif type(num,'short')=='m' then
      nz=poly(num,'z','c');
      dz=poly(den,'z','c');
      xm=abs(freq(nz,dz,dfr));
    else
      error('Error---Input arguments wrong data type')
    end
  end
endfunction
