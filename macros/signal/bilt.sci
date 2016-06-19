function [npl,nzr,ngn]=bilt(pl,zr,gn,num,den)
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
  
//[npl,nzr,ngn]=bilt(pl,zr,gn,num,den)
//macro for calculating the gain poles and zeros
//which result from a bilinear transform or from
//a biquadratic transform.  Used by the macros iir
//and trans
//Note: ***This macro is not intended for general use***
//  pl  :input poles
//  zr  :input zeros
//  gn  :input gain
//  num :numerator of transform
//  den :denominator of transform
// Author: C. Bunks  date: 5 May 1989
//Updated: 15 Sep 1997

// 
  n= num.coeffs{1} // n=coeff(num);
  d= den.coeffs{1} // d=coeff(den);
  order=max(size(n))-1;
  ms=max(size(pl));ns=max(size(zr));
  
  if order==1 then
    n0=n(1);n1=n(2);
    if prod(size(d))==1 then d=[d,0];end
    d0=d(1);d1=d(2);
    if isempty(zr) then      
      ngn=1/prod(n1*ones_deprecated(pl)-d1*pl);
    else
      ngn=prod(n1*ones_deprecated(zr)-d1*zr)/prod(n1*ones_deprecated(pl)-d1*pl);
    end
    if ms<>ns then ngn=real(gn*d1**(ms-ns)*ngn);else ngn=real(gn*ngn);end
    nzr=-(n0*ones_deprecated(zr)-d0*zr)./(n1*ones_deprecated(zr)-d1*zr);
    npl=-(n0*ones_deprecated(pl)-d0*pl)./(n1*ones_deprecated(pl)-d1*pl);
    if ms>ns then
      nzr=[nzr';-(d0/d1)*ones(ms-ns,1)];
    elseif ms<ns then
      npl=[npl';-(d0/d1)*ones(ms-ns,1)];
    end
  elseif order==2 then
    n0=n(1);n1=n(2);n2=n(3);
    d0=d(1);d1=d(2);d2=d(3);
    a=n2*ones_deprecated(zr)-d2*zr;
    b=n1*ones_deprecated(zr)-d1*zr;
    c=n0*ones_deprecated(zr)-d0*zr;
    gz=a;
    z1=-b./(2*a)+sqrt((b./(2*a)).^2-c./a);
    z2=-b./(2*a)-sqrt((b./(2*a)).^2-c./a);
    a=n2*ones_deprecated(pl)-d2*pl;
    b=n1*ones_deprecated(pl)-d1*pl;
    c=n0*ones_deprecated(pl)-d0*pl;
    gp=a;
    p1=-b./(2*a)+sqrt((b./(2*a)).^2-c./a);
    p2=-b./(2*a)-sqrt((b./(2*a)).^2-c./a);
    gw=d2;
    w1=-d1./(2*d2)+sqrt((d1./(2*d2)).^2-d0./d2);
    w2=-d1./(2*d2)-sqrt((d1./(2*d2)).^2-d0./d2);
    ngn=gn*prod(gz)/prod(gp);
    nzr=[z1,z2];
    npl=[p1,p2];
    if ms>ns then
      nzr=[nzr';-(d0/d1)*ones(ms-ns,1)];
    elseif ms<ns then
      npl=[npl';-(d0/d1)*ones(ms-ns,1)];
    end
    ngn=real(ngn*(gw**(ms-ns)));
  else
    error('error bilt --- wrong order transform')
  end
endfunction
