function x=ffilt(ft,n,fl,fh)
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
  
//x=ffilt(ft,n,fl,fh)
//Get n coefficients of a an FIR low-pass,
//high-pass, band-pass, or stop-band filter
//  ft :Filter type where ft can take the values
//     :       'lp' for low-pass filter
//     :       'hp' for high-pass filter
//     :       'bp' for band-pass filter
//     :       'sb' for stop-band filter
//  n  :Number of filter samples desired
//  fl :Low frequency cut-off
//  fh :High frequency cut-off
//     :For low and high-pass filters one cut-off
//     :frequency must be specified whose value is
//     :given in fl.  For band-pass and stop-band
//     :filters two cut-off frequencies must be
//     :specified for which the lower value is in
//     :fl and the higher value is in fh.
//  x  :Filter coefficients
// Author: C. Bunks  date: 12 March 1988
//        sinc replaced by filt_sync S Steer Jan 2002
// 

//Pre-calculation
  
  no2=(n-1)/2;
  ino2=int(no2);
  
  //Calculate n samples of the sinc function
  
  //Low pass filter
  
  if ft=='lp' then
    [x]=filt_sinc(n,fl)
  end
  
  //High pass filter
  
  if ft=='hp' then
    x=filt_sinc(n,fl)
    x=-x;
    x(no2+1)=1+x(no2+1);
  end
  
  //Band pass filter
  
  if ft=='bp' then
    wc=%pi*(fh+fl);
    fl=(fh-fl)/2;
    x=filt_sinc(n,fl)
    y=2*cos(wc*(-no2:no2));
    x=x.*y;
  end
  
  //Stop band filter
  
  if ft=='sb' then
    wc=%pi*(fh+fl);
    fl=(fh-fl)/2;
    x=filt_sinc(n,fl)
    y=2*cos(wc*(-no2:no2));
    x=-x.*y;
    x(no2+1)=1+x(no2+1);
  end
  
endfunction

function [x]=filt_sinc(n,fl)
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 1987-2016 - C. Bunk (INRIA)
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
  
//x=sinc(n,fl)
//Calculate n samples of the function sin(2*pi*fl*t)/(pi*t)
//for t=-n/2:n/2 (i.e. centered around the origin).
//  n  :Number of samples
//  fl :Cut-off freq. of assoc. low-pass filter in Hertz
//  x  :Samples of the sinc function
// 
// C. Bunks  date: 12 March 1988

  no2=(n-1)/2;
  ino2=int(no2);
  wl=fl*2*%pi;
  xn=sin(wl*(-no2:no2));
  xd=%pi*(-no2:no2);
  if ino2==no2 then xn(no2+1)=2*fl; xd(no2+1)=1;end
  x=xn./xd;
endfunction
