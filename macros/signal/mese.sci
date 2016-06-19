function [sm,fr]=mese(x,npts);
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
  
//<sm,fr]=mese(x [,npts]);
//Calculate the maximum entropy spectral estimate of x
//  x    :Input sampled data sequence
//  npts :Optional parameter giving number of points of fr and sm
//        (default is 256)
//  sm   :Samples of spectral estimate on the frequency grid fr
//  fr   :npts equally spaced frequency samples in [0,.5)
//  Author: C. Bunks  date: 24 August 1988
// revised: 9 April 1991
// 

//default evaluation
  
  if nargout==1 then
    npts=256;
  end
  
  //estimate autocorrelation function of x
  
  Nx=length(x);
  r=convol(x,x(Nx:-1:1))
  r=r(Nx:-1:1)/Nx;
  
  //get solution to the Yule-Walker equations
  
  [ar,sigma2,rc]=lev(r);
  
  //compute spectrum
  
  ak=[1;ar];
  [sf,fr]=frmag(ak,npts);
  sm=sigma2*ones_deprecated(sf)./(sf.*conj(sf));
endfunction
