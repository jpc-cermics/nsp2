function sp=interp3(xyzp,x,y,z,s,sedge)
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
  // trilinear interpolation: renders values of s at points xyzp
  //     input: values s over the grid x * y * z, xyzp(np,3)
  //     Periodic boundary conditions are enforced, x,y,z assumed
  //     in increasing order (the period is [x(nx)-x(1)]*nx/(nx-1)  )

  if nargout<6 then sedge='per'; end

  np=length(xyzp)/3; nx=length(x); ny=length(y); nz=length(z);

  if size(s,1)<>nx | size(s,2)<>ny | size(s,3)<>nz then
    printf("%s\n",' ');
    printf("%s\n",'  argument 5 of interp3: size ('+string(size(s,1))+...
	  ','+string(size(s,2))+','+string(size(s,3))+') found, ('+ ...
	  string(nx)+','+string(ny)+','+string(nz)+') expected')
    return
  end
  if length(x)<2 then printf("%s\n",'   x too small');end
  if length(y)<2 then printf("%s\n",'   y too small');end
  if length(z)<2 then printf("%s\n",'   z too small');end


  x0=[x(1)-(x(nx)-x(1))/(nx-1),matrix(x,1,nx)]'; 
  y0=[y(1)-(y(ny)-y(1))/(ny-1),matrix(y,1,ny)]';
  z0=[z(1)-(z(nz)-z(1))/(nz-1),matrix(z,1,nz)]';
  dx=x0(2:$)-x0(1:$-1); dy=y0(2:$)-y0(1:$-1); dz=z0(2:$)-z0(1:$-1)
  dx=[dx(2:$);dx(1)]; dy=[dy(2:$);dy(1)]; dz=[dz(2:$);dz(1)];
  Lx=x(nx)-x0(1); Ly=y(ny)-y0(1); Lz=z(nz)-z0(1)

  xp=modulo(xyzp(:,1)-x0(1),Lx); 
  yp=modulo(xyzp(:,2)-y0(1),Ly);
  zp=modulo(xyzp(:,3)-z0(1),Lz);  
  //out-of-edge flag, for later
  pz=xyzp(:,1)<x(1) | xyzp(:,1)>x(nx) |...
     xyzp(:,2)<y(1) | xyzp(:,2)>y(ny) |...
     xyzp(:,3)<z(1) | xyzp(:,3)>z(nz)     ; clear xyzp
  xp(find(xp<0))=xp(find(xp<0))+Lx;
  yp(find(yp<0))=yp(find(yp<0))+Ly;
  zp(find(zp<0))=zp(find(zp<0))+Lz;
  xp=xp+x0(1); yp=yp+y0(1); zp=zp+z0(1);
  i=bsearch(xp',x); j=bsearch(yp',y); k=bsearch(zp',z);

  i1=modulo(i,nx)+1; j1=modulo(j,ny)+1; k1=modulo(k,nz)+1; 
  jm=j-1; jm1=j1-1; km=k-1; km1=k1-1;
  jm(jm<0)=jm(jm<0)+ny; km(km<0)=km(km<0)+nz;

  x2=xp-x0(i+1); x1=x0(i1+1)-xp;
  y2=yp-y0(j+1); y1=y0(j1+1)-yp;
  z2=zp-z0(k+1); z1=z0(k1+1)-zp; 

  clear xp yp zp

  i(i==0)=nx; j(j==0)=ny; k(k==0)=nz; 


  //the computation of sp is slpitted so intermediate arrays can be erased
  //(many arrays = readability and savings in number of operations)
  // (compromise - I could factor two multiplications by .*z1, .*z2, but I
  //   would waste twice as much memory)


  D=(dx(i).*dy(j).*dz(k)); clear j k dx dy dz
  s=s('entries'); //essential for speed
  s111=s(i+  (jm  + km *ny)*nx);
  s211=s(i1+ (jm  + km *ny)*nx);
  sp=(s111.*x1 + s211.*x2).*(y1.*z1);     clear s111 s211
  s121=s(i+  (jm1 + km *ny)*nx);
  s221=s(i1+ (jm1 + km *ny)*nx);          clear km
  sp=sp+(s121.*x1 + s221.*x2).*(y2.*z1);  clear s121 s221 z1

  s112=s(i+  (jm  + km1*ny)*nx);
  s212=s(i1+ (jm  + km1*ny)*nx);          clear jm
  sp=sp+(s112.*x1 + s212.*x2).*(y1.*z2);  clear s112 s212
  s122=s(i+  (jm1 + km1*ny)*nx);
  s222=s(i1+ (jm1 + km1*ny)*nx);          clear i1 jm1 km1
  sp=sp+(s122.*x1 + s222.*x2).*(y2.*z2);  clear s122 s222 x1 y1 x2 y2 z2 

  sp= sp./D

  clear dx dy dz i j k

  //if many points are out-of-edge it might be unefficient first to
  //  compute them and then to zero them
  if sedge=='zero' then
    sp(pz)=0
  end      
endfunction


