function [xe]=sskf(y,f,h,q,r,x0)
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
  
//<xe>=sskf(y,f,h,q,r,x0)
//steady-state kalman filter
// y   :data in form [y0,y1,...,yn], yk a column vector
// f   :system matrix dim(NxN)
// h   :observations matrix dim(NxM)
// q   :dynamics noise matrix dim(NxN)
// r   :observations noise matrix dim(MxM)
//
// xe  :estimated state
// 

//get steady-state Kalman gain
  
  // x=ricc(f',h'/r*h,q,'disc') // steady state err cov
  x=riccati(f',h'/r*h,q,'d') // steady state err cov
  k=x*h'/(h*x*h'+r)

  // estimate state
  kfd=(eye_deprecated(f)-k*h)*f;
  [xe]=ltitr(kfd,k,y,x0);
endfunction
