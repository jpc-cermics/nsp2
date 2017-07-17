function [b]=shift(a,nx,ny)
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
  // Adapted from Enrico Segre scicoslab toolbox.
  // Copyright (C) 1998-2017 - Enrico Segre

  //
  // shifts cyclically the elements of a vector of nx positions
  //        or the elements of an array of nx,ny positions
  //       useful e.g. to center fft arrays or to center
  //       periodical images
  //
  lx=size(a,1)
  ly=size(a,2)
  if ly>1 & nargin==2 then ny=0; end
  if ly==1 & nargin==3 then ny=0; end
  if lx==1 & ly>1 & nargin==2 then ny=nx; nx=1; end

  ix=modulo(modulo((0:lx-1)+nx,lx)+lx,lx)+1;
  iy=modulo(modulo((0:ly-1)+ny,ly)+ly,ly)+1;

  b(ix,iy)=a;
endfunction


