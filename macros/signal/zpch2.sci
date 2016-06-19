function [zers,pols,gain]=zpch2(n,A,omegar)
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
  
//[zers,pols,gain]=zpch2(n,A,omegar)
//Poles and zeros of a type 2 Chebyshev analog filter
//gain is the gain of the filter
//H(s)=gain*poly(zers,'s')/poly(pols,'s')
//  n      :Filter order
//  A      :Attenuation in stop band (A>1)
//  omegar :Cut-off frequency in Hertz
//  zers   :Resulting filter zeros
//  pols   :Resulting filter poles
//  gain   :Resulting filter gain
//
//Author F.D.
//Revised by C. Bunks Oct. 24, 1996  
// 
  un=ones(1,n);
  v=%pi/(2*n)*(1:2:2*n-1);
  w=exp(%i*v);
  cosine=real(w);
  sine=imag(w);
  n2=int(n/2);
  if n==2*n2 then
    zers=%i*omegar*un./cosine;
  else
    zers=%i*omegar*un(1:n-1)./[cosine(1:n2),cosine(n2+2:n)];
  end
  Gamma=(A+sqrt(A*A-1))**(1/n);
  alpha=-((Gamma-1/Gamma)/2)*sine;
  Beta=((Gamma+1/Gamma)/2)*cosine;
  normal=alpha.*alpha+Beta.*Beta;
  pols=omegar*(alpha-%i*Beta)./normal;
  gain=abs(real(prod(pols)/prod(zers)));
  
endfunction
