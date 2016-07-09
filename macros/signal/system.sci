function [x1,y]=system_m(x0,f,g,h,q,r)
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
  
//<x1,y>=system(x0,f,g,h,q,r)
//define system macro which generates the next
//observation given the old state
//  x0 :Input state vector
//  f  :System matrix
//  g  :Input matrix
//  h  :Output matrix
//  q  :Input noise covariance matrix
//  r  :Output noise covariance matrix
//  x1 :Output state vector
//  y  :Output observation
//System recursively calculates
//
//     x1=f*x0+g*u
//      y=h*x0+v
//
//where u is distributed N(0,q)
//and v is distribute N(0,r).
// Author: C. Bunks  date: August 1988
// 
  // rand('normal');
  q2=chol(q);
  r2=chol(r);
  u=q2'*rand(x0);
  v=r2'*rand(x0);
  x1=f*x0+g*u;
  y=h*x0+v;
endfunction
