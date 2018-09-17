function [ar,sigma2,rc]=lev(r)
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
  
//[ar,sigma2,rc]=lev(r)
//Resolve the Yule-Walker equations:
//
//       |r(0)   r(1)   ... r(N-1)|| a(1) | |sigma2|
//       |r(1)   r(0)   ... r(n-1)|| a(2) | |  0   |
//       |  :      :    ...    :  ||   :  |=|  0   |
//       |  :      :    ...    :  ||   :  | |  0   |
//       |r(N-1) r(N-2) ...  r(0) ||a(N-1)| |  0   |
//
//using Levinson's algorithm.
//  r      :Correlation coefficients
//  ar     :Auto-Regressive model parameters
//  sigma2 :Scale constant
//  rc     :Reflection coefficients
//  Author: C. Bunks  date: 24 August 1988
// revised: 9 April 1991
// 

//get the size of the correlation vector
  
  rsize=max(size(r));
  r=matrix(r,1,rsize);
  
  //initialize levinson's algorithm
  
  ar=-r(2)/r(1);
  rc(1)=ar;
  sigma2=(1-ar*conj(ar))*r(1);
  //iterative solution to yule-walker equations
  
  for k=2:rsize-1,
    ak1(k,1)=-(r(k+1)+ar(1:k-1)'*r(k:-1:2)')/sigma2;
    rc(k)=ak1(k,1);
    ak1(1:k-1,1)=ar(1:k-1)+ak1(k,1)*conj(ar(k-1:-1:1));
    sigma2=(1-ak1(k,1)*conj(ak1(k,1)))*sigma2;
    ar=ak1;
  end
endfunction
