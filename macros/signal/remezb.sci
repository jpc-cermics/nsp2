function an=remezb(nc,fg,ds,wt)
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
  
//an=remezb(nc,fg,ds,wt)
//Minimax approximation of a frequency domain
//magnitude response.  The approximation takes
//the form
//
//          h = sum[a(n)cos(wn)]
//
//for n=0,1,...,nc.  An FIR, linear-phase filter
//can be obtained from the the output of the macro
//by using the following Scilab commands
//
//          hn(1:nc-1)=an(nc:-1:2)/2;
//          hn(nc)=an(1);
//          hn(nc+1:2*nc-1)=an(2:nc)/2;
//
//  nc :Number of cosine functions
//  fg :Grid of frequency points in [0,.5)
//  ds :Desired magnitude on grid fg
//  wt :Weighting function on error on grid fg
//  an :Cosine filter coefficients
//  Author: C. Bunks  date: 24 August 1988
// 

//get frequency grid size
  
  ngrid=max(size(fg));
  
  //check for errors in input
  
  f0=fg(1);
  f1=fg(ngrid);
  if f0<0 then
    error('values of fg must be in [0,.5] --- program stopped'),
  end
  if f0>.5 then
    error('values of fg must be in [0,.5] --- program stopped'),
  end
  if f1<0 then
    error('values of fg must be in [0,.5] --- program stopped'),
  end
  if f1>.5 then
    error('values of fg must be in [0,.5] --- program stopped'),
  end
  dsize=max(size(ds));
  wsize=max(size(wt));
  if dsize<>ngrid then
    error('fg and ds vectors are not the same length --- program stopped'),
  end
  if wsize<>ngrid then
    error('fg and wt vectors are not the same length --- program stopped'),
  end
  
  //set up the initial guess for the extremal frequencies
  
  iext=round(1:ngrid/nc:ngrid);
  iext(nc+1)=ngrid;
  iext(nc+2)=ngrid;
  
  //call remez.f
  
  an=remez(iext,ds,fg,wt);
  an=an(1:nc)
endfunction
