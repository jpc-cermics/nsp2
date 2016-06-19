function [x1,p1,x,p]=kalm(y,x0,p0,f,g,h,q,r)
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
  
//[x1,p1,x,p]=kalm(y,x0,p0,f,g,h,q,r)
//macro which gives the Kalman update and error variance
//Input to the macro is:
// f,g,h  :current system matrices
// q,r    :covariance matrices of dynamics and observation noise
// x0,p0  :state estimate and error variance at t=0 based
//        :on data up to t=-1
// y      :current observation
//
//Output from the macro is:
// x1,p1  :updated estimate and error covariance at t=1
//        :based on data up to t=0
// x,p    :updated estimate and error covariance at t=0
//        :based on data up to t=0
//  Author: C. Bunks  date: 9 August 1988

  k=p0*h'*(h*p0*h'+r)**(-1);
  p=(eye_deprecated(p0)-k*h)*p0;
  p1=f*p*f'+g*q*g';
  x=x0+k*(y-h*x0);
  x1=f*x;
endfunction
