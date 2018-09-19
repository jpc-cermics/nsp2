function [hs,pols,zers,gain]=analpf(n,fdesign,rp,omega)
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 1987-2016 - C. Bunks  date: 9 Sept 1988 (INRIA)
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
  
//[hs,pols,zers,gain]=analpf(n,fdesign,rp,omega)
//Creates analog low-pass filter with cut-off frequency at omega
//  n       :filter order (pos. integer)
//  fdesign :filter design method
//          :    fdesign=('butt','cheb1','cheb2','ellip')
//  rp      :2-vector of error values for cheb1, cheb2, and
//          :ellip filters where only rp(1) is used for
//          :cheb1 case, only rp(2) is used for cheb2 case, and
//          :rp(1) and rp(2) are both used for ellip case.
//          :          0<rp(1),rp(2)<1
//          :for cheb1 filters:  1-rp(1)<ripple<1 in passband
//          :for cheb2 filters:  0<ripple<rp(2)   in stopband
//          :for ellip filters:  1-rp(1)<ripple<1 in passband
//          :                    0<ripple<rp(2)   in stopband
//  omega   :cut-off frequency of low-pass filter in Hertz
//  hs      :rational polynomial transfer function
//  pols    :poles of transfer function
//  zers    :zeros of transfer function
//  gain    :gain of transfer function
//
//	hs=gain*poly(zers,'s')/poly(pols,'s')

  select fdesign
   case 'butt' then
    [pols,gain]=zpbutt(n,omega);
    zers=[];
    hs=p2r(m2p(gain,var='s'),real(poly(pols,var='s')));
   case 'cheb1' then
    epsilon=sqrt(-1+1/(1-rp(1))**2);
    [pols,gain]=zpch1(n,epsilon,omega);
    zers=[];
    hs=p2r(m2p(gain,var='s'),real(poly(pols,var='s')));
   case 'cheb2' then
    att=1/rp(2);
    [zers,pols,gain]=zpch2(n,att,omega);
    hs=p2r(gain*real(poly(zers,var='s')), real(poly(pols,var='s')));
   case 'ellip' then
    epsilon=sqrt(-1+1/(1-rp(1))**2);
    att=1/rp(2);
    m=find_freq(epsilon,att,n);
    omegar=omega/sqrt(m);
    [zers,pols,gain]=zpell(epsilon,att,omega,omegar);
    hs=p2r(gain*real(poly(zers),var='s'),real(poly(pols,var='s')));
  else
    error('Unknown design type --- program termination'),
  end
endfunction
