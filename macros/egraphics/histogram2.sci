function  [x,y,h]=histogram2(a,b,lx,ux,ly,uy,nx,ny)
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
  // joint histogram in 2d
  //  syntax:   [x,y,h]=histogram2(a,b)
  //            [x,y,h]=histogram2(a,b,nx,ny)
  //            [x,y,h]=histogram2(a,b,lx,ux,ly,uy)
  //            [x,y,h]=histogram2(a,b,lx,ux,ly,uy,nx,ny)
  //

  if nargin==0 then
    printf('[x,y,h]=histogram2(rand(1,1000).^2,rand(1,1000).^3,20,20);\n" +...
	   'plot3d1(x,y,h)\n');
    [x,y,h]=histogram2(rand(1,1000).^2,rand(1,1000).^3,20,20);
    plot3d1(x,y,h);
    return
  end

  if ~(nargin==2 | nargin==4 | nargin==6 ) then 
    printf("%s\n",'histogram2: wrong number of arguments'); return; 
  end
  
  if nargin==6 then nx=50; ny=50; end
  if nargin==4 then  nx=lx; ny=ux; lx=min(a);ux=max(a); 
    ly=min(b); uy=max(b);
  end
  if nargin==2 then 
    lx=min(a),ux=max(a); ly=min(b),uy=max(b); 
    nx=50; ny=50; 
  end;
  h=zeros(nx,ny);

  x=lx+((1:nx)'-0.5)*(ux-lx)/nx;
  dx=(ux-lx)/nx; xp=[x'-0.5*dx,ux+dx];
  y=ly+((1:ny)'-0.5)*(uy-ly)/ny;
  dy=(uy-ly)/ny; yp=[y'-0.5*dy,uy+dy];

  for i=1:nx; 
    for j=1:ny;
      h(i,j)=length( find( a>=xp(i) &  a<xp(i+1) & b>=yp(j) &  b<yp(j+1) ) )
    end; 
  end
  if nargout==1 then x=h; 
  end         // comodita' per calls veloci
  return
endfunction
