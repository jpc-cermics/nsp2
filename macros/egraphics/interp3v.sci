function [spxyz]=interp3v(xyzp,x,y,z,sx,sy,sz,sedge)
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

  //for small np it is not worth to clear intermediate variables: it slows down 
  // too much

  if nargout<8 then sedge='per'; end

  np=length(xyzp)/3; nx=length(x); ny=length(y); nz=length(z);

  //check of the sizes of sx, sy, sz (if they are hypermatrices)
  if type(sx)==17 & type(sy)==17 & type(sz)==17  then
    Mx=sx("dims"); My=sy("dims"); Mz=sz("dims");
    // the following line alone TAKES HALF OF THE ROUTINE TIME. this is why 
    //  flattened is better
    sx=sx('entries'); sy=sy('entries'); sz=sz('entries'); //essential for speed
    if Mx<>[nx,ny,nz] then
      printf("%s\n",' ');
      printf("%s\n",'  argument 5 of interp3v: size ('+string(Mx(1))+...
	    ','+string(Mx(2))+','+string(Mx(3))+') found, ('+...
	    string(nx)+','+string(ny)+','+string(nz)+') expected')
      return
    end
    if Mx<>Mz then
      printf("%s\n",'size of sx <> size of sz')
      return
    end
    if Mx<>My then
      printf("%s\n",'size of sx <> size of sy')
      return
    end
  end
  if nx<2 then printf("%s\n",'   x too small');end
  if ny<2 then printf("%s\n",'   y too small');end
  if nz<2 then printf("%s\n",'   z too small');end

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
     xyzp(:,3)<z(1) | xyzp(:,3)>z(nz)     ; // clear xyzp
  xp(find(xp<0))=xp(find(xp<0))+Lx;
  yp(find(yp<0))=yp(find(yp<0))+Ly;
  zp(find(zp<0))=zp(find(zp<0))+Lz;
  xp=xp+x0(1); yp=yp+y0(1); zp=zp+z0(1);
  //i=bsearch(xp',x); j=bsearch(yp',y); k=bsearch(zp',z);
  //inlined bsearch()
  i= ones(size(xp)); ikh=nx*i;
  for ii=1:ceil(log2(nx))
    ikt = floor((ikh + i)/2 )
    ty= (xp' < x(ikt)); ikh(ty)= ikt(ty) ;  i(~ty)= ikt(~ty);
  end
  i(x(1)>xp | xp>x($))=0   
  //                                      
  j= ones(size(yp)); ikh=ny*j;
  for ii=1:ceil(log2(ny))
    ikt = floor((ikh + j)/2 )
    ty= (yp' < y(ikt)); ikh(ty)= ikt(ty) ;  j(~ty)= ikt(~ty);
  end
  j(y(1)>yp | yp>y($))=0                                         
  //
  k = ones(size(zp)); ikh=nz*k;
  for ii=1:ceil(log2(nz))
    ikt = floor((ikh + k)/2 )
    ty= (zp' < z(ikt)); ikh(ty)= ikt(ty) ;  k(~ty)= ikt(~ty);
  end
  k(z(1)>zp | zp>z($))=0                                         

  i1=i-int(i./nx).*nx+1; j1=j-int(j./ny).*ny+1; k1=k-int(k./nz).*nz+1; 
  jm=j-1; jm1=j1-1; km=k-1; km1=k1-1;
  jm(jm<0)=jm(jm<0)+ny; km(km<0)=km(km<0)+nz;

  x2=xp-x0(i+1); x1=x0(i1+1)-xp;
  y2=yp-y0(j+1); y1=y0(j1+1)-yp;
  z2=zp-z0(k+1); z1=z0(k1+1)-zp; 

  //clear xp yp zp

  i(i==0)=nx; j(j==0)=ny; k(k==0)=nz; 

  //the computation of sp is slpitted so intermediate arrays can be erased
  //(many arrays = readability and savings in number of operations)
  // (compromise - I could factor two multiplications by .*z1, .*z2, but I
  //   would waste twice as much memory)

  D=(dx(i).*dy(j).*dz(k)); // clear j k dx dy dz

  ijk= i+  (jm  + km *ny)*nx
  s111x=sx(ijk); s111y=sy(ijk); s111z=sz(ijk);
  ijk= i1+  (jm  + km *ny)*nx
  s211x=sx(ijk); s211y=sy(ijk); s211z=sz(ijk); yz=(y1.*z1);
  spx=(s111x.*x1 + s211x.*x2).*yz;  
  spy=(s111y.*x1 + s211y.*x2).*yz;  
  spz=(s111z.*x1 + s211z.*x2).*yz;  
  //   clear s111x s211x s111y s211y s111z s211z

  ijk= i+  (jm1  + km *ny)*nx
  s121x=sx(ijk); s121y=sy(ijk); s121z=sz(ijk);yz=(y2.*z1)
  ijk= i1+  (jm1  + km *ny)*nx
  s221x=sx(ijk); s221y=sy(ijk); s221z=sz(ijk); // clear km
  spx=spx+(s121x.*x1 + s221x.*x2).*yz;
  spy=spy+(s121y.*x1 + s221y.*x2).*yz;
  spz=spz+(s121z.*x1 + s221z.*x2).*yz;  
  //clear s121x s221x s121y s221y s121z s221z z1

  ijk= i+  (jm  + km1 *ny)*nx
  s112x=sx(ijk); s112y=sy(ijk); s112z=sz(ijk);yz=(y1.*z2);
  ijk= i1+  (jm  + km1 *ny)*nx
  s212x=sx(ijk); s212y=sy(ijk); s212z=sz(ijk); // clear jm
  spx=spx+(s112x.*x1 + s212x.*x2).*yz; 
  spy=spy+(s112y.*x1 + s212y.*x2).*yz; 
  spz=spz+(s112z.*x1 + s212z.*x2).*yz;  
  //clear s112x s212x s112y s212y s112z s212z

  ijk= i+  (jm1  + km1 *ny)*nx               ;  // clear i
  s212x=sx(ijk); s212y=sy(ijk); s212z=sz(ijk); yz=(y2.*z2);
  ijk= i1+  (jm1  + km1 *ny)*nx
  s222x=sx(ijk); s222y=sy(ijk); s222z=sz(ijk);  // clear i1 jm1 km1
  spx=spx+(s212x.*x1 + s222x.*x2).*yz; 
  spy=spy+(s212y.*x1 + s222y.*x2).*yz;
  spz=spz+(s212z.*x1 + s222z.*x2).*yz;
  //clear s122x s222x s122y s222y s122z s222z x1 y1 x2 y2 z2 

  spx= spx./D; spy= spy./D; spz= spz./D; 

  //if many points are out-of-edge it might be unefficient first to
  //  compute them and then to zero them
  if sedge=='zero' then
    spx(pz)=0; spy(pz)=0; spz(pz)=0
  end      

  spxyz=[spx;spy;spz]
endfunction



