function [hn]=eqfir(nf,bedge,des,wate)
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
  
//<hn>=eqfir(nf,bedge,des,wate)
//Minimax approximation of multi-band, linear phase, FIR filter
//  nf    :Number of output filter points desired
//  bedge :Mx2 matrix giving a pair of edges for each band
//  des   :M-vector giving desired magnitude for each band
//  wate  :M-vector giving relative weight of error in each band
//  hn    :Output of linear-phase FIR filter coefficients
// Author: C. Bunks  date: 18 June 1988
// 

//get number of cosines
  
  nc=int(nf/2);
  if nf-2*nc<>0 then
    flag=0;
    nc=nc+1;
  else
    flag=1;
  end
  
  //make frequency grid, desired function, and weight function
  
  [nb,c2]=size(bedge);
  ngp=nc*16;
  b1=bedge(:,1);
  b2=bedge(:,2);
  sb=sum(b2-b1);
  delf=sb/ngp;
  bp=round((b2-b1)/delf);
  bsum=0;
  for k=1:nb,
    bpk=bp(k);
    et=b2(k)-b1(k);
    fg(bsum+1:bsum+bpk)=b1(k)*ones_deprecated(1:bpk)+(0:bpk-1)*et/(bpk-1);
    ds(bsum+1:bsum+bpk)=des(k)*ones_deprecated(1:bpk);
    wt(bsum+1:bsum+bpk)=wate(k)*ones_deprecated(1:bpk);
    bsum=bsum+bpk;
  end
  
  //adjust values of ds and wt if filter is of even length
  
  if flag==1 then
    fgs=max(size(fg));
    if fg(fgs)>.5-%eps then
      fg=fg(1:fgs-1);
      ds=ds(1:fgs-1);
      wt=wt(1:fgs-1);
    end
    cf=cos(%pi*fg);
    ds=ds./cf;
    wt=wt.*cf;
  end
  
  //call remez
  
  [an]=remezb(nc,fg,ds,wt);
  
  //obtain other half of filter coefficients (by symmetry)
  
  if flag==1 then
    hn(1)=.25*an(nc);
    hn(2:nc-1)=.25*(an(nc:-1:3)+an(nc-1:-1:2));
    hn(nc)=.5*an(1)+.25*an(2);
    hn(nc+1:2*nc)=hn(nc:-1:1);
  else
    hn=an(nc:-1:2)/2;
    hn(nc)=an(1);
    hn(nc+1:2*nc-1)=hn(nc-1:-1:1);
  end
endfunction
