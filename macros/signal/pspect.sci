function [sm,cwp]=pspect(sec_step,sec_leng,wtype,x,y,wpar)
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
  
//<sm,cwp>=pspect(sec_step,sec_leng,wtype,x,y,wpar)
//Cross-spectral estimate between x and y if both are given
//and auto-spectral estimate of x otherwise.
//Spectral estimate obtained using the modified periodogram method
// x        :data if vector, amount of input data if scalar
// y        :data if vector, amount of input data if scalar
// sec_step :offset of each data window
// sec_leng :length of each data window
// wtype    :window type (re,tr,hm,hn,kr,ch)
// wpar     :optional window parameters
//          :         for wtype='kr', wpar>0
//          :         for wtype='ch', 0<wpar(1)<.5, wpar(2)>0
// sm       :power spectral estimate in the interval [0,1]
// cwp      :unspecified Chebyshev window parameter
// Author: C. Bunks  date: 14 Sept 1988
// 
  
  cross=0;
  if sec_step<sec_leng then
    //get number of sections to be used
    xsize=max(size(x));
    if xsize==1 then
      xsize=x;
    end
    nsecs=int((xsize-sec_leng)/sec_step);
    
    //construct window
    
    if nargin==4 then
      w=window(wtype,sec_leng);
    else 
      if nargin==5 then
	if wtype=='kr' then
	  wpar=y;
	  w=window(wtype,sec_leng,wpar);
	else
	  if wtype=='ch' then
	    wpar=y;
	    [w,cwp]=window(wtype,sec_leng,wpar);
	  else
	    cross=1;
	    w=window(wtype,sec_leng);
	  end
	end
      else
	[w,cwp]=window(wtype,sec_leng,wpar);
	cross=1;
      end
    end
    wpower=w*w';
    
    //average periodograms
    
    sm=0*w;
    if max(size(x))==1 then
      ovrlp=sec_leng-sec_step;
      xd=[0*ones(1,sec_step) getx(ovrlp,1)];
      if cross==1 then
	yd=[0*ones(1,sec_step) gety(ovrlp,1)];
      end
      for k=1:nsecs,
	xd(1:ovrlp)=xd(sec_step+1:sec_leng);
	xd(ovrlp+1:sec_leng)=...
	    getx(sec_step,sec_leng+(k-1)*sec_step+1);
	xw=w.*(xd-(sum(xd)/sec_leng)*ones(1,sec_leng));
	fx=fft(xw);
	if cross==1 then
	  yd(1:ovrlp)=yd(sec_step+1:sec_leng);
	  yd(ovrlp+1:sec_leng)=...
	      gety(sec_step,sec_leng+(k-1)*sec_step+1);
	  yw=w.*(yd-(sum(yd)/sec_leng)*ones(1,sec_leng));
	  fy=fft(yw);
	  sm=sm+fx.*conj(fy);
	else
	  sm=sm+real(fx.*conj(fx));
	end
      end
    else
      ind=(1:sec_leng);
      for k=1:nsecs,
	indd=ind+(k-1)*sec_step*ones(1,sec_leng);
	xd=x(indd);
	xe=w.*(xd-(sum(xd)/sec_leng)*ones(1,sec_leng));
	fx=fft(xe);
	if cross==1 then 
	  yd=y(indd);
	  ye=w.*(yd-(sum(yd)/sec_leng)*ones(1,sec_leng));
	  fy=fft(ye);
	  sm=sm+fx.*conj(fy);
	else
	  sm=sm+real(fx.*conj(fx));
	end
      end
    end
    
    sm=sm/(nsecs*wpower);
    
    //input error
  else
    error('section step is bigger then section length');
  end
endfunction
