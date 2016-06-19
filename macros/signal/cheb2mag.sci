function [h2]=cheb2mag(n,omegar,A,sample)
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
  
//<h2>=cheb2mag(n,omegar,A,sample)
//Square magnitude response of a type 1 Chebyshev filter
//omegar = stopband edge
//sample = vector of frequencies where the square magnitude
//h2 is desired.
//  n       :Filter order
//  omegar  :Cut-off frequency
//  A       :Attenuation in stop band
//  sample  :Vector of frequency where cheb2mag is evaluated
//  h2      :Chebyshev II filter values at sample points
//
//Author F.D.
// 

  [n1,n2]=size(sample);
  un=ones(n1,n2);
  Tn=chepol(n,'x');             //n-th Chebyshev polynomial
  frd=freq(Tn,1,omegar*un./sample);   //frd=Tn(omegar/sample)
  h2=un./(un+(A*A-1)*un./real(frd.*frd))
endfunction
