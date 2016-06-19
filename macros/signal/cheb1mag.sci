function [h2]=cheb1mag(n,omegac,epsilon,sample)
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
  
//<h2>=cheb1mag(n,omegac,epsilon,sample)
//Square magnitude response of a type 1 Chebyshev filter
//omegac=passband edge
//epsilon such that 1/(1+epsilon**2)=passband ripple
//sample vector of frequencies where the square magnitude
//is desired.
//  n       :Filter order
//  omegac  :Cut-off frequency
//  epsilon :Ripple in pass band
//  sample  :Vector of frequency where cheb1mag is evaluated
//  h2      :Chebyshev I filter values at sample points
//
//Author F.D.
// 

  [n1,n2]=size(sample);
  un=ones(n1,n2);
  Tn=chepol(n,'x');  //n-th Chebyshev polynomial
  fr=freq(Tn,1,sample/omegac);   //fr=Tn(sample/omegac)
  h2=un./(un+epsilon*epsilon*fr.*fr)   //magnitude
endfunction
