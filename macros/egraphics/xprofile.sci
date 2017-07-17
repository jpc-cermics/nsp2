function xprofile(x,y,a)
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

  // OK a revoir pour les options de contour 
  //
  function profilewin(x,y,rectx,recty,a,i,j)
    subplot(2,2,3); 
    A=get_current_axes(); A.children=list();
    plot2d(x',a(:,j),rect=rectx,line_color=3);
    plot2d(x(i),double(a(i,j)),rect=rectx,mark=3,mark_size=3,mark_color=5);
    xtitle("X profile -- y="+string(y(j)))
    subplot(2,2,2); 
    A=get_current_axes(); A.children=list();
    plot2d(a(i,:),y,rect=recty,style=2);
    xtitle("Y profile -- x="+string(x(i)))
    plot2d(double(a(i,j)),y(j),rect=recty,mark=3,mark_size=3,mark_color=5);
  endfunction
  
  if nargin==0 then
    demostring=[
	"x=linspace(-2,2,50);y=linspace(-2,2,50);"
	"xprofile(x,y,sin(x)''*cos(y));"]
    printf("%s\n",demostring)
    execstr(demostring)
    return
  end
  
  if nargin==1 then
    a=x;  x=1:size(a,1); y=1:size(a,2);
  end
  // discretization of x and y 
  xi=(3*x(1)-x(2))/2; xl=(3*x($)-x($-1))/2
  yi=(3*y(1)-y(2))/2; yl=(3*y($)-y($-1))/2
  xs=[xi,(x(2:$)+x(1:$-1))/2];
  ys=[yi,(y(2:$)+y(1:$-1))/2];
  
  subplot(2,2,1);
  rect=[min(x),min(y),max(x),max(y)];
  xset('colormap', hotcolormap(10));
  contourf(x,y,a,rect=rect,nv=10);
  zr=[min(a),max(a)];
  rectx=[rect(1),zr(1),rect(3),zr(2)];
  recty=[zr(1),rect(2),zr(2),rect(4)];
  xtitle('Click to exit')
  qset=%f;
  xml=xi; yml=yi; rep=[xi,yi,-1]; i=1; j=1; firsttime=%t;
  while rep(3)==-1 then 
    rep=xgetmouse()
    xm=rep(1); ym=rep(2); btn=rep(3);
    i=max([1,find(xm>=xs)]); j=max([1,find(ym>=ys)])
    xml=min(max(xm,xi),xl);  yml=min(max(ym,yi),yl);
    profilewin(x,y,rectx,recty, a,i,j)
  end
endfunction

