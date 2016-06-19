function [y,e1]=convol(h,x,e0)
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
  
//  convol - convolution 
//%CALLING SEQUENCE
//  [y]=convol(h,x)
//  [y,e1]=convol(h,x,e0)     (for use with overlap add method)
//%PARAMETERS
//  x,h       :input sequences (h is a "short" sequence, x a "long" one)
//  e0        : old tail to overlap add (not used in first call)
//  y         : output of convolution
//  e1        : new tail to overlap add (not used in last call)
//%DESCRIPTION
//  calculates the convolution y= h*x of two discrete sequences by 
//  using the fft.  overlap add method can be used.
//%USE OF OVERLAP ADD METHOD
//  For x=[x1,x2,...,xNm1,xN]
//  First call : [y1,e1]=convol(h,x1)
//  Subsequent calls : [yk,ek]=convol(h,xk,ekm1)
//  Final call : [yN]=convol(h,xN,eNm1)
//  Finally y=[y1,y2,...,yNm1,yN]
// 

  n=prod(size(x))
  m=prod(size(h))
  m1=2^(int(log(n+m-1)/log(2))+1)
  x(m1)=0;h(m1)=0
  if norm(imag(x))==0&norm(imag(h))==0 then
    y=real(ifft(fft(matrix(x,1,m1)).*fft(matrix(h,1,m1))))
  else
    y=ifft(fft(matrix(x,1,m1)).*fft(matrix(h,1,m1)))
  end
  if nargout+nargin==5 then
    e0(n)=0;//update carried from left to right
    e1=y(n+1:n+m-1)
    y=y(1:n)+e0

  elseif nargout+nargin==4 then
    if nargin==2 then
      e1=y(n+1:n+m-1)
      y=y(1:n) //initial update
    else
      e0(n+m-1)=0 //final update
      y=y(1:n+m-1)+e0
    end

  else
    y=y(1:n+m-1) //no update
  end
endfunction
