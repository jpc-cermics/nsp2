function [h,f,g]=phc(hk,d,r)
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
  
//[h,f,g]=phc(hk,d,r)
//macro which computes the matrices h, f, g by the principal hankel
//component approximation method, from the hankel matrix built
//from the covariance sequence of a stochastic process.
//   hk  : hankel matrix
//   d   : dimension of the observation
//   r   : desired dimension of the state vector
//       : for the approximated model
//   h,f,g  : relevant matrices of the state-space model
// Author G. Le Vey  Date: 16 Febr. 1989
// 

  [p,q]=size(hk);
  [u,s,v]=svd(hk);
  s=diag(sqrt(s(1:r)));
  obs=u(:,1:r)*s;
  g=s*v(:,1:r)';
  g=g(:,1:d);
  ofl=obs(d+1:p,:);opr=obs(1:p-d,:);
  f=opr\ofl;
  h=obs(1:d,:);
endfunction
