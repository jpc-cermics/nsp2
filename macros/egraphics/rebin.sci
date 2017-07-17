function an=rebin(xn,yn,x,y,a,sedge)
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

  // Given the matrix a(x,y), it rebins it on the new grid as an(xn,yn).
  //  the function interp2, which treats the matrix as doubly periodic,
  //  is used.
  if nargin==0 then
    demo_str=['x=linspace(-%pi,%pi,5);y=x;'
	      'z=x''*y;'
	      'xn=linspace(-%pi,%pi,20);yn=xn;'
	      'an=rebin(xn,yn,x,y,z);';
	      'plot3d1(xn,yn,an)'];
    printf("%s\n",demo_str);
    execstr(demo_str);
    return
  end

  if nargin<6 then sedge='per'; end                                                   

  nxn=length(xn);nyn=length(yn);
  xy= [ matrix(ones(nyn,1)*matrix(xn,1,nxn),nxn*nyn,1), ...
	matrix(matrix(yn,nyn,1)*ones(1,nxn),nxn*nyn,1)];
  an=interp2(xy,x,y,a,sedge)';
  an=matrix(an,nyn,nxn)';

endfunction

