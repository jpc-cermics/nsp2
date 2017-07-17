function zp=interp2(xyp,x,y,z,sedge)
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
  // bilinear interpolation: renders values of z at points xyp
  //     input: values z over the grid x * y, xyp(np,2)
  //     Periodic boundary conditions are enforced, x,y assumed
  //     in increasing order and equispaced
  //     (the period is [x(n)-x(1)]*(n+1)/n  )
  //     for simplicity and for now very little error check

  if nargin == 0 then
    x=linspace(0,10,10);y=x;z=x'*y;
    n=20,
    xp=linspace(0,10,n);yp=xp;
    [xp1,yp1]=meshgrid(xp,yp);
    zp=interp2([xp1(:),yp1(:)],x,y,z);
    zp=matrix(zp,n,-1);
    plot3d1(xp,xp,zp,colormap=jetcolormap(64));
    return;
  end
  
  if nargout<5 then sedge='per'; end

  np=length(xyp)/2; nx=length(x); ny=length(y);

  if size(z,1)<>nx | size(z,2)<>ny then
    printf("%s\n",' ');
    printf("%s\n",'  argument 4 of interp2: size ('+string(size(z,1))+...
	  ','+string(size(z,2))+') found, ('+string(nx)+','+string(ny)+') expected')
    return
  end
  if length(x)<2 then printf("%s\n",'   x too small');end
  if length(y)<2 then printf("%s\n",'   y too small');end


  x0=[x(1)-(x(nx)-x(1))/(nx-1),matrix(x,1,nx)]'; 
  y0=[y(1)-(y(ny)-y(1))/(ny-1),matrix(y,1,ny)]';
  dx=x0(2:$)-x0(1:$-1); dy=y0(2:$)-y0(1:$-1); 
  Lx=x(nx)-x0(1); Ly=y(ny)-y0(1);

  xp=modulo(xyp(:,1)-x0(1),Lx); 
  yp=modulo(xyp(:,2)-y0(1),Ly);
  xp(find(xp<0))=xp(find(xp<0))+Lx;
  yp(find(yp<0))=yp(find(yp<0))+Ly;
  //i1=int(xp/dx)+1; j1=int(yp/dy)+1;
  i1=bsearch(xp+x0(1),x0); j1=bsearch(yp+y0(1),y0);
  xp=xp+x0(1); yp=yp+y0(1);
  nx1=nx+1;

  zzz=matrix([z(nx,ny),z(nx,:); z(:,ny),z],(nx+1)*(ny+1),1);
  xx=zzz((j1-1)*nx1+i1).*(x0(i1+1)-xp)+...
     zzz((j1-1)*nx1+i1+1).*(xp-x0(i1));
  yy=zzz(j1*nx1+i1).*(x0(i1+1)-xp)+zzz(j1*nx1+i1+1).*(xp-x0(i1));
  clear zzz
  zp=(xx.*(y0(j1+1)-yp)+yy.*(yp-y0(j1)))./(dx(i1).*dy(j1));

  if sedge=='zero' then
    pz=xyp(:,1)<x(1) | xyp(:,1)>x(nx) | xyp(:,2)<y(1) | xyp(:,2)>y(ny)
    zp(pz)=0
  end      
endfunction
