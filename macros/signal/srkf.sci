function [x1,p1]=srkf(y,x0,p0,f,h,q,r)
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
  
//square root kalman filter algorithm
//Input to the macro is:
// f,h    :current system matrices
// q,r    :covariance matrices of dynamics
//        :and observation noise
// x0,p0  :state estimate and error variance
//        :at t=0 based on data up to t=-1
// y      :current observation
//
//Output from the macro is:
// x1,p1  :updated estimate and error covariance
//        :at t=1 based on data up to t=0
//  Author: C. Bunks  date: 9 August 1988
// 

  n=max(size(x0));
  p=max(size(y));
  
  j=[chol(r)',0*r];
  g=[0*q,chol(q)'];
  
  mat=[h*p0,j;f*p0,g];
  [q,tmat]=qr(mat')';
  p1=tmat(p+1:p+n,p+1:p+n);
  k=tmat(p+1:p+n,1:p);
  re=tmat(1:p,1:p);
  
  epsilon=y-h*x0;
  x1=f*x0+k*(re**(-1))*epsilon;
endfunction
