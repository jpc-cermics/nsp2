function [xk]=mfft(x,flag,dim)
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
  
//<xk>=mfft(x,flag,dim)
//macro which calculates the fft for a multi-dimensional signal
// x    :x(i,j,k,...) input signal in the form of a row vector
//      :whose values are arranged so that the i index runs the
//      :quickest, followed by the j index, etc.
// flag :(-1) fft or (1) inverse fft
// dim  :dimension vector which gives the number of
//      :values of x for each of its indices
// xk   :output of multidimensional fft in same format as for x
//
//Example: For a three dimensional vector which has three points
//along its first dimension, two points along its second
//dimension and three points along its third dimension the row
//vector is arranged as follows
//
//      x=<x(1,1,1),x(2,1,1),x(3,1,1),
//         x(1,2,1),x(2,2,1),x(3,2,1),
//               x(1,1,2),x(2,1,2),x(3,1,2),
//               x(1,2,2),x(2,2,2),x(3,2,2),
//                     x(1,1,3),x(2,1,3),x(3,1,3),
//                     x(1,2,3),x(2,2,3),x(3,2,3)>
//
//and the dim vector is as follows
//
//      dim=<3,2,3>
//
// Author: C. Bunks  date: 30 Sept 1988
// 

  xk=x;
  dims=[1 matrix(dim,1,max(size(dim)))];
  for k=1:max(size(dim)),
    xk=fft(xk,flag,dim(k),prod(dims(1:k)));
  end
endfunction
