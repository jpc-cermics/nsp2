function [hzt]=trans(pd,zd,gd,tr_type,frq)
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
  
// hzt=trans(pd,zd,gd,tr_type,frq)
// macro for transforming standardized low-pass filter into
// one of the following filters:
//     low-pass, high-pass, band-pass, stop-band.
// hz      :input polynomial
// tr_type :type of transformation
// frq     :frequency values
// hzt     :output polynomial
// Author: C. Bunks  date: 9 Sept 1988
//corrections: C. Bunks date 14 Feb. 1998
// 
  if type(pd,'short')=="r" then
    hz=pd;
    tr_type=zd
    frq=gd
    pd=roots(hz.den)
    zd=roots(hz.num)
    gd=coeff(hz.num,hz.num.degree[])/coeff(hz.den,hz.den.degree[])
  end
  z=poly(0,'z');fc=.25;fu=frq(1);fl=frq(2);
  //make filter type using all-pass change of variables
  select tr_type
   case 'lp' then
    alpha=sin(%pi*(fc-fu))/sin(%pi*(fc+fu));
    num=z-alpha;
    den=1-alpha*z;
   case 'hp' then
    alpha=-cos(%pi*(fc-fu))/cos(%pi*(fc+fu));
    num=-(1+alpha*z);
    den=z+alpha;
   case 'bp' then
    k=tan(%pi*fc)/tan(%pi*(fu-fl));
    alpha=cos(%pi*(fu+fl))/cos(%pi*(fu-fl));
    num=-((k+1)-2*alpha*k*z+(k-1)*z^2);
    den=(k+1)*z^2-2*alpha*k*z+(k-1);
   case 'sb' then
    k=tan(%pi*fc)*tan(%pi*(fu-fl));
    alpha=cos(%pi*(fu+fl))/cos(%pi*(fu-fl));
    num=(k+1)-2*alpha*z+(1-k)*z^2;
    den=(k+1)*z^2-2*alpha*z+(1-k);
  else
    error(sprintf('Error: unknown filter type ''%s'' in function trans',tr_type));
  end
  [pt,zt,gt]=bilt(pd,zd,gd,num,den);
  hzt= p2r(gt*real(poly(zt,'z')),real(poly(pt,'z')),var='z');// ,'d');
endfunction
