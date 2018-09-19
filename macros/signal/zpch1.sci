function [pols,gain]=zpch1(n,epsilon,omegac)
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
  
//Poles of a Type 1 Chebyshev analog filter
//The transfer function is given by :
//H(s)=gain/poly(pols,'s')
//  n       :Filter order
//  epsilon :Ripple in the pass band (0<epsilon<1)
//  omegac  :Cut-off frequency in Hertz
//  pols    :Resulting filter poles
//  gain    :Resulting filter gain
//
//Author F.D.
//Revised by C. Bunks Oct. 24, 1996  
// 
  Gamma=((1+sqrt(1+epsilon**2))/epsilon)^(1/n);
  a=omegac*(Gamma-1/Gamma)/2;
  b=omegac*(Gamma+1/Gamma)/2;
  v=%pi/(2*n):%pi/n:(2*n-1)/(2*n)*%pi;
  sigma=-a*sin(v);
  omega=b*cos(v);
  pols=sigma+%i*omega;
  gain=abs(real(prod(pols)));
  if n==2*int(n/2) then
    gain=gain/sqrt(1+epsilon^2);
  end
endfunction
