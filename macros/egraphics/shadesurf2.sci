function shadesurf2(x,y,z,varargopt);
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

  //  plot of a surface with lighting due to an infinitely
  //    distant light source (the sun), shading
  //
  // usage: shadesurf2(x,y,z, varargopt) 
  //     varargopt can be options for plot3d1 
  //     and can contain  l=[-1 1 2],shine=3,m=2 
  //     which are options for shadecomp
  
  if nargin ==0 then
    printf("%s\n","Demo of shadesurf2:\n")
    s_demo= ["x=linspace(-2.5,2.5,50);"
	     "z=exp(-(x''*x).^2);"
	     "shadesurf2(x,x,z,colormap=hotcolormap(128));"];
    printf("%s\n",s_demo);
    execstr(s_demo);
    return
  end
  
  if size(x,1)==1 | size(x,2)==1 then
    // style like plot3d(x,y,z): x(1:nx), y(1:ny), z(1:nx,1:ny)
    [xx,yy,zz]=genfac3d(x,y,z);
    nn1=(zz(1,:)+zz(2,:)-zz(3,:)-zz(4,:))./(xx(4,:)-xx(1,:));
    nn2=(zz(1,:)-zz(2,:)-zz(3,:)+zz(4,:))./(yy(2,:)-yy(1,:));
    nn3=1
  else
    // style like plot3d(xx,yy,zz): facelets (thought primarily for
    // triangles)
    xx=x; yy=y; zz=z;
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
  
  keys=['l','shine','m'];
  values=['[-1 1 2]','3','2'];
    
  for i=1:size(keys,'*') do 
    if varargopt.iskey[keys(i)] then 
      execstr(sprintf('%s=varargopt(''%s'')',keys(i),keys(i))); 
      varargopt.remove[keys(i)];
    else
      execstr(sprintf('%s=%s;',keys(i), values(i)));
    end
  end
  
  cc=shadecomp(xx,yy,zz,l,shine,m)*(nc-1)+1;
  plot3d1(xx,yy,zz,varargopt(:),colors=cc);
  // to draw also the external contour of the surface would be
  //  nice, but requires more tracking of the external edges
  
  return
  //an additional animated demo:
  x=linspace(-2.5,2.5,35); 
  a=cos((x'*x)).*((x'*x)+1);
  xbasc();xset("pixmap",1);xset("wwpc");
  for i=-1.5:.15:1.5; shadesurf2(x,x,a,l=[-i,i,2],theta=145); xset("wshow"); end; 
  xset("pixmap",0)
endfunction

