function [hk]=hank(m,n,cov)
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 1987-2016 - G. Le Vey (INRIA)
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
  
//<hk>=hank(m,n,cov)
//this macro builds the hankel matrix of size (m*d,n*d)
//from the covariance sequence of a vector process
//   m  : number of bloc-rows
//   n  : number of bloc-columns
//   cov: sequence of covariances; it must be given as :[R0 R1 R2...Rk]
//   hk : computed hankel matrix
// Author: G. Le Vey  Date: 16 Febr. 1989

  hk=[];
  d=min(size(cov));
  for k=0:m-1,hk=[hk;cov(:,(k*d+1):(k+n)*d)];end
endfunction
