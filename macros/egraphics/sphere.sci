function [xx,yy,zz]=sphere(x0,r,n)
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

  // expanded from SCI/surface/surfaces.sci
  if nargin<3 then n=16; end
  if nargin<2 then r=1; end
  if nargin<1 then x0=[0,0,0]; end

  if size(x0)==[3,1] then x0=x0'; end
  if size(x0,2)<>3 then
    printf("%s\n",'wrong size of x0\n'); return
  end
  ns=size(x0,1)
  if length(r)<>ns then r=r(1)*ones(ns,1); end 

  vn=2*n

  // prototype sphere
  u = linspace(-%pi/2,%pi/2,n);
  v = linspace(0,2*%pi,vn);  
  x= cos(u)'*cos(v);
  y= cos(u)'*sin(v);
  z= sin(u)'*ones(size(v));

  in=1:(n-1); inp=2:n; in2=1:(vn-1)
  xxx=[matrix(x(in,in2+1),1,(n-1)*(vn-1));
       matrix(x(in,in2),1,(n-1)*(vn-1));
       matrix(x(inp,in2),1,(n-1)*(vn-1));
       matrix(x(inp,in2+1),1,(n-1)*(vn-1))]
  yyy=[matrix(y(in,in2+1),1,(n-1)*(vn-1));
       matrix(y(in,in2),1,(n-1)*(vn-1));
       matrix(y(inp,in2),1,(n-1)*(vn-1));
       matrix(y(inp,in2+1),1,(n-1)*(vn-1))]
  zzz=[matrix(z(in,in2+1),1,(n-1)*(vn-1));
       matrix(z(in,in2),1,(n-1)*(vn-1));
       matrix(z(inp,in2),1,(n-1)*(vn-1));
       matrix(z(inp,in2+1),1,(n-1)*(vn-1))]
  
  //multiplication of spheres

  nf=size(xxx,2);
  xx=zeros(4,nf*ns); yy=xx; zz=xx
  for i=1:ns
    xx(:,((i-1)*nf+1):i*nf)=xxx*r(i)+x0(i,1)
    yy(:,((i-1)*nf+1):i*nf)=yyy*r(i)+x0(i,2)
    zz(:,((i-1)*nf+1):i*nf)=zzz*r(i)+x0(i,3)
  end
endfunction

