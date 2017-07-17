function shadesurf(x,y,z,varargopt)
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

  // OK (Sep 2015 nsp v1.0)
  // plot of a surface with lighting due to an infinitely
  //    distant light source (the sun), no shading
  //
  // shadesurf(x,y,z,[a=,b=,...])
  //
  //     all optional arguments of plot3d1 can be used 
  //     plus a and b the director cosines of the sun direction
  //   
  
  if nargin == 0 then
    s_demo= ["x=linspace(-2.5,2.5,50);"
	     "z=exp(-(x''*x).^2);"
	     "shadesurf(x,x,z,a=-1,b=1,theta=65,alpha=60,colormap=hotcolormap(128));"];
    printf("%s\n",s_demo);
    execstr(s_demo);
    return
  end
  
  if varargopt.iskey['colormap'] then 
    nc = size(varargopt.colormap,1);
  else 
    F=get_current_figure();
    if ~isempty(F.gc.colormap) then
      F.gc.set[colormap=jetcolormap(64)];
    end
    nc = size(F.gc.colormap,1);
  end
  
  if size(x,1)==1 | size(x,2)==1 then
    // style like plot3d(x,y,z) i.e x(1:nx), y(1:ny), z(1:nx,1:ny)
    [xx,yy,zz]=genfac3d(x,y,z);
    nn1=(zz(1,:)+zz(2,:)-zz(3,:)-zz(4,:))./(xx(4,:)-xx(1,:));
    nn2=(zz(1,:)-zz(2,:)-zz(3,:)+zz(4,:))./(yy(2,:)-yy(1,:));
    nn3=1
  else
    // style like plot3d(xx,yy,zz) i.e three matrices of the same size 
    // describings facelets 
    xx=x; yy=y; zz=z;
    nn1=-((yy(1,:)-yy(2,:)).*(zz(2,:)-zz(3,:))-...
	  (yy(2,:)-yy(3,:)).*(zz(1,:)-zz(2,:)));
    nn2=((xx(1,:)-xx(2,:)).*(zz(2,:)-zz(3,:))-...
	 (xx(2,:)-xx(3,:)).*(zz(1,:)-zz(2,:)));
    nn3=-((xx(1,:)-xx(2,:)).*(yy(2,:)-yy(3,:))-...
	  (xx(2,:)-xx(3,:)).*(yy(1,:)-yy(2,:)));
    if size(xx,1)==3 then
      // so far scilab (up to 2.5) has the bug of the extra line of the
      // 3d triangle
      // xx=[xx;xx(1,:)]; yy=[yy;yy(1,:)]; zz=[zz;zz(1,:)]; 
    end
  end
  
  degfac=find(abs(nn1)+abs(nn2)+abs(nn3)<10*%eps);
  // this can happen for instance if there are coincident vertices;
  // it shouldn't, if xx, yy, zz are constructed properly, but
  // I want shadesurf to get through it anyway
  nn3(degfac)=1

  if varargopt.iskey['a'] then a = varargopt.a;varargopt.remove['a'] else a=0;end 
  if varargopt.iskey['b'] then b = varargopt.b;varargopt.remove['b'] else b=0.2;end 
  
  tag = 3; 
  select tag 
   case 1 then 
    // color proportional to the slope
    cc=sqrt(nn1.^2+nn2.^2+nn3.^2);
   case 2 then 
    // color inversely proportional to the lightened side
    cc=1../max(a*nn1+b*nn2,1);
   case 3 then 
    // color proportional to the sine of the angle (local normal)^(sun)
    // the direction of the sun is [a, b, 1]
    cc=(a*nn1+b*nn2+nn3)./sqrt(nn1.^2+nn2.^2+nn3.^2) ./sqrt(a^2+b^2+1);
  end
  if max(cc)~=min(cc) then
    cc=(cc -min(cc))/(max(cc)-min(cc))*(nc-1)+1;
  else
    // this can happen if the surface is a plane
    cc=ones(size(cc,1),size(cc,2))*nc;
  end
  plot3d1(xx,yy,zz,varargopt(:),colors=cc);
endfunction

