function [xx,yy,zz,cc]=spherecast(a,amin,amax,l)
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

  if nargin == 0 then 
    // esempio:
    x=linspace(0,10,30);y=x;
    [x,y]=meshgrid(x,y);
    Z=x+y;
    // a = rebin(linspace(0,10,10),linspace(0,10,10),
    // 1:20,1:20,testmatrix('franck',20));
    a=Z;
    [xx,yy,zz,cc]=spherecast(a,1);
    plot3d1(xx,yy,zz,colors=32*cc,colormap=jetcolormap(32),alpha=50,theta=50,leg='x@y@z',iso=%t);
    return;
  end
  
  if nargin==3 then l=1; end
  if nargin==2 then l=amin;amin=min(a);amax=max(a); end
  if nargin==1 then l=1;amin=min(a);amax=max(a); end

  if amin>=amax then amin=min(a);amax=max(a); end
  a(find(a<amin))=amin;a(find(a>amax))=amax;

  if l==1 then
    vn=size(a,2)+1
    n=size(a,1)+1
    nf=length(a)
  end
  if l==2 then
    vn=size(a,2)+1
    n=size(a,1)
    nf=(vn-1)*(n-1)
  end

  // facelets of the sphere
  u = linspace(-%pi/2,%pi/2,n);
  v = linspace(0,2*%pi,vn);  
  x= cos(u)'*cos(v);
  y= cos(u)'*sin(v);
  z= sin(u)'*ones(size(v));

  in=1:(n-1); inp=2:n; in2=1:(vn-1)
  xx=[matrix(x(in,in2+1),1,nf);
      matrix(x(in,in2),1,nf);
      matrix(x(inp,in2),1,nf);
      matrix(x(inp,in2+1),1,nf)]
  yy=[matrix(y(in,in2+1),1,nf);
      matrix(y(in,in2),1,nf);
      matrix(y(inp,in2),1,nf);
      matrix(y(inp,in2+1),1,nf)]
  zz=[matrix(z(in,in2+1),1,nf);
      matrix(z(in,in2),1,nf);
      matrix(z(inp,in2),1,nf);
      matrix(z(inp,in2+1),1,nf)]

  // color casting

  if l==1 then
    cc=matrix((a-amin)/(amax-amin),1,length(a))
  end
  if l==2 then
    c=(a-amin)/(amax-amin);
    inp=in+1
    in2p=[2:in2($),1]
    cc=[matrix(c(in,in2p),1,nf);
	matrix(c(in,in2),1,nf);
	matrix(c(inp,in2),1,nf);
	matrix(c(inp,in2p),1,nf)]
  end   
endfunction


