function tab=wigner(x,h,deltat,zp)
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 1987-2016 - F. Delebecque et all (INRIA)
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
  
//macro which computes the 'time-frequency' wigner
//spectrum of a signal.
//
//	tab    : wigner spectrum (lines correspond to the time variable)
//	x      : analysed signal
//	h      : data window
//      deltat : analysis time increment (in samples)
//      zp     : length of FFT's. %pi/zp gives the frequency increment.
//
//   Initializations
//
// 
  l=prod(size(x));
  n=prod(size(h));
  npr=2*n;
  h=h.*conj(h);tab=[];
  //
  //   Analytical signal computation using Hilbert transform
  //
  [y,y1]=convol(hilb(127),x);
  z=x+%i*y;
  //
  //   Wigner distribution computation
  //
  t=n;
  while t<=l-n,
    z1=h.*z(t:t+n-1).*conj(z(t:-1:t-n+1));
    z1(zp)=0;
    w=fft(z1);
    w=2*(2*real(w)-z(t)*z(t)'*ones_deprecated(w));tab=[tab;w];
    t=t+deltat;
  end
  tab=tab(:,1:zp);
endfunction
