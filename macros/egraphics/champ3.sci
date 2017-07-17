function champ3(x,y,z,sx,sy,sz)
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
  
  //argument parsing
  x=matrix(x,1,-1); y=matrix(y,1,-1); z=matrix(z,1,-1); 
  nx=length(x); ny=length(y); nz=length(z); 
  
  if size(sx)<>[nx,ny,nz] &  size(sx)<>[nx,ny,nz]'  then 
    printf("%s\n", "sx size "+strcat(string(size(sx)),'x')+" found, "+ ...
		   strcat(string([nx,ny,nz]),'x')+" expected")
    return
  end
  if size(sy)<>[nx,ny,nz] &  size(sy)<>[nx,ny,nz]' then 
    printf("%s\n", "sy size "+strcat(string(size(sy)),'x')+" found, "+ ...
		   strcat(string([nx,ny,nz]),'x')+" expected")
    return
  end
  if size(sz)<>[nx,ny,nz] & size(sz)<>[nx,ny,nz]'  then 
    printf("%s\n", "sz size "+strcat(string(size(sz)),'x')+" found, "+ ...
		   strcat(string([nx,ny,nz]),'x')+" expected")
    return
  end

  //scale factors
  ds=mean([x(2:$)-x(1:$-1), y(2:$)-y(1:$-1), z(2:$)-z(1:$-1)])
  su=sx.^2+sy.^2+sz.^2;
  sf=ds/max(su(:));
  q=su(:)>0
  clear su;

  //generate the arrows
  zp=   z   .*.ones(size(y)).*.ones(size(x))
  yp=ones(size(z)).*.  y    .*.ones(size(x))
  xp=ones(size(z)) .*. ones(size(y)) .*. x

  xp=xp(q); yp=yp(q); zp=zp(q)
  
  xp1=xp+sf*sx(q)'; yp1=yp+sf*sy(q)'; zp1=zp+sf*sz(q)'; 
  clear q
  
  [xx,yy,zz]=spaghetti([xp;xp1],[yp;yp1],[zp;zp1],ds/15)
  clear xp xp1 yp yp1 zp zp1
  shadesurf2(xx,yy,zz,m=1)
endfunction
