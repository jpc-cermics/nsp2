function [xx,yy,zz,c]=slice3d(x,y,z,h,ii,jj,kk)
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

  //  syntax: [xx,yy,zz,c]=slice3d([x,y,z,] h,ii,jj,kk)
  //
  // generates facelets for the slicing (sort of a la spyglass)
  //  of 3d data in hypermat h
  // Only cartesian plane slices for the moment
  //
  //  x,y,z: vectors, coordinates of the grid
  //
  //  ii, jj, kk : vectors or scalars, 
  //    the slice is cut on cartesian planes at
  //    constant value of the nonzero indices (if more than
  //    one index is non null, more than one slice is produced)
  //  index values can be negative: then the facelets are flipped
  //   (this is for plot3d, which distinguishes the side of the faces);
  //
  // c is a vector of values of h, rescaled between 0 and 1
  //  for use e.g. in plot3d1(xx,yy,list(zz,c*ncolors))
  //  the resulting facelets extend over x,y,z: the center of
  //   each facelet is in (x,y,z).
  //
  // add interpolated shading 
    
  if nargin <= 0 then
    s_demo = ['  n=30;x=linspace(-4.5,4.5,n); y=x;z=x;';
	      '  s=cell(1,n);[X,Y]=ndgrid(x,y);'
	      '  for i=1:n; M= 1 ./((X-2).^2+Y.^2+(z(i)-2)^2);'
	      '    M =M + 0*1 ./((X+2).^2+Y.^2+(z(i)+2)^2);';
	      '    s{i} = -log(M);';
	      '  end'
	      '  for j=1:n;'
	      '    [xx,yy,zz,c]=slice3d(x,y,z,s,[j],[j],[j]);'
	      '    plot3d(xx,yy,zz,ebox=[min(x),max(x),min(y),max(y),min(z),max(z)],colors=c*(64-1)+1,back_color=-2,colormap=hotcolormap(64));'
	      '    xclick();xbasc();';
	      '  end;'
	      '    [xx,yy,zz,c]=slice3d(x,y,z,s,[22],[15],[22]);'
	      '    plot3d(xx,yy,zz,ebox=[min(x),max(x),min(y),max(y),min(z),max(z)],colors=c*(64-1)+1,back_color=-2,colormap=hotcolormap(64));'
	      ''];
    printf("%s\n",s_demo);
    execstr(s_demo);
    return
  end
  
  if nargin==4 then 
    ii=y; jj=z; kk=h; h=x;
  end

  nz=size(h,'*'); nx=size(h{1},1); ny=size(h{1},2);
  
  if nargin==4 then 
    x=1:nx;  y=1:ny;  z=1:nz; 
  end

  x=matrix(x,1,-1); y=matrix(y,1,-1); z=matrix(z,1,-1); 

  x1=[x(1),(x(2:$)+x(1:$-1))/2,x($)];
  y1=[y(1),(y(2:$)+y(1:$-1))/2,y($)];
  z1=[z(1),(z(2:$)+z(1:$-1))/2,z($)];
  // facelets are centered on the gridpoint, except for edge facelets,
  //  which are halved
  //max and min are sought layer by layer, likely because scilab<2.6 didn't
  // support max(hypermat); however, max(uint,uint) is still buggy in 2.6;
  // therefore the double()s
  maxh=max(h{1}); minh=min(h{1}); 
  for l=2:nz
    minh=min(min(h{l}),minh); maxh=max(max(double(h{l})),maxh);
  end
  xx=[];yy=[];zz=[];c=[];

  if or(abs(ii)>nx) then 
    printf("%s\n","yz slices "+strcat(string(ii(abs(ii)>nx)),' ')+...
		  " nonexisting, ignored")
  end 
  if or(abs(jj)>ny) then 
    printf("%s\n","xz slices "+strcat(string(jj(abs(jj)>ny)),' ')+...
		  " nonexisting, ignored")
  end 
  if or(abs(kk)>nz) then 
    printf("%s\n","xy slices "+strcat(string(kk(abs(kk)>nz)),' ')+...
		  " nonexisting, ignored")
  end 
  
  Cx=cell(1,nx);
  for i=1:nx,
    M=[];for k=1:nz do M=[M, h{k}(i,:)'];end 
    Cx{i}= M;
  end
  shade =%t;
  for i=int(ii(find(ii<>0 & abs(ii)<=nx)))
    c0=matrix(Cx{abs(i)}', ny,nz);
    if ~shade then 
      [xx0,yy0,zz0]=genfac3d(x(abs(i))*ones(1,nz+1),y1,z1'*ones(1,ny+1))
      xx=[xx,xx0]; yy=[yy,yy0], zz=[zz,zz0]; c=[c,c0(:)'];
    else
      [xx0,yy0,zz0]=genfac3d(x(abs(i))*ones(1,nz),y,z'*ones(1,ny))
      [xx_0,yy_0,cc0]=genfac3d(x(abs(i))*ones(1,nz),y,c0);
      xx=[xx,xx0]; yy=[yy,yy0], zz=[zz,zz0]; c=[c,cc0];
    end
  end

  Cy=cell(1,ny);
  for i=1:ny,
    M=[];for k=1:nz do M=[M, h{k}(:,i)];end 
    Cy{i}= M;
  end
  
  for j=int(jj(find(jj<>0 & abs(jj)<=ny)))
    c0=matrix(Cy{abs(j)}',nx,nz);
    if ~shade then 
      [xx0,yy0,zz0]=genfac3d(x1,y(abs(j))*ones(1,nz+1),(z1'*ones(1,nx+1))');
      xx=[xx,xx0]; yy=[yy,yy0], zz=[zz,zz0]; c=[c,c0(:)'];
    else
      [xx0,yy0,zz0]=genfac3d(x,y(abs(j))*ones(1,nz),(z'*ones(1,nx))');
      [xx_0,yy_0,cc0]=genfac3d(x,y(abs(j))*ones(1,nz),c0);
      xx=[xx,xx0]; yy=[yy,yy0], zz=[zz,zz0]; c=[c,cc0];
    end
  end
  
  for k=int(kk(find(kk<>0 & abs(kk)<=nz))) 
    c0=matrix(h{abs(k)},nx,ny);
    if ~shade then 
      [xx0,yy0,zz0]=genfac3d(x1,y1,z(abs(k))*ones(nx+1,ny+1))
      xx=[xx,xx0]; yy=[yy,yy0], zz=[zz,zz0]; c=[c,c0(:)'];
    else
      [xx0,yy0,zz0]=genfac3d(x,y,z(abs(k))*ones(nx,ny));
      [xx_0,yy_0,cc0]=genfac3d(x,y,c0);
      xx=[xx,xx0]; yy=[yy,yy0], zz=[zz,zz0]; c=[c,cc0];
    end
  end
  c=(double(c)-minh)/(maxh-minh);
endfunction

