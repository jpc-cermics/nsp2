// A set of surface demo
// Copyright Inria (from scilab) ?


function hole3d1()
// Holes in surfaces using %inf
  function [x,y,z]=sph(alp,tet)
    x=r*cos(alp).*cos(tet)+orig(1)*ones(size(tet));
    y=r*cos(alp).*sin(tet)+orig(2)*ones(size(tet));
    z=r*sin(alp)+orig(3)*ones(size(tet));
  endfunction
  r=1;orig=[0 0 0];
  x=linspace(-%pi/2,%pi/2,40);y=linspace(0,%pi*2,20);
  x(5:8)=%nan*ones(size(5:8));
  x(30:35)=%nan*ones(size(30:35));
  [x1,y1,z1]=eval3dp(sph,x,y);
  plot3d1(x1,y1,z1,colormap=jetcolormap(32))
endfunction

function sphere()
  u = linspace(-%pi/2,%pi/2,40);
  v = linspace(0,2*%pi,20);
  x= cos(u)'*cos(v);
  y= cos(u)'*sin(v);
  z= sin(u)'*ones(size(v));
  [xx,yy,zz]=nf3d(x,y,z);
  plot3d1(xx,yy,zz,colormap=jetcolormap(32));
endfunction

function shell()
  u = linspace(0,2*%pi,40);
  v = linspace(0,2*%pi,20);
  x= (cos(u).*u)'*(1+cos(v)/2);
  y= (u/2)'*sin(v);
  z= (sin(u).*u)'*(1+cos(v)/2);
  [xx,yy,zz]=nf3d(x,y,z);
  plot3d1(xx,yy,zz,colormap=jetcolormap(32));
endfunction

function spiral()
  [r,a]=ndgrid(0:0.1:1,0:%pi/8:6*%pi);
  r=r',a=a';
  z=a/8;
  x=r.*cos(a).*(1-a/20);
  y=r.*sin(a).*(1-a/20);
  z=z-1.5;
  [xx,yy,zz]=nf3d(x,y,z);
  plot3d1(xx,yy,zz,colormap=jetcolormap(32));
endfunction



function torus1  ()
// a deformed torus
  x=linspace(0,2*%pi,40);
  y=linspace(0,2*%pi,20)';
  factor=1.5+cos(y);
  X=factor*cos(x);
  Y=factor*sin(x);
  Z=sin(y)*ones(size(x))+ ones(size(y))*cos(2*x);
  [xx,yy,zz]=nf3d(X,Y,Z);
  plot3d1(xx,yy,zz,colormap=jetcolormap(32));
endfunction

function moebius()
// the Moebius band
  t=linspace(-1,1,20)';
  x=linspace(0,%pi,40);
  factor=2+ t*cos(x);
  X=factor*diag(cos(2*x));
  Y=factor*diag(sin(2*x));
  Z=t*sin(x);
  [xx,yy,zz]=nf3d(X,Y,Z);
  plot3d1(xx,yy,zz,colormap=jetcolormap(32));
endfunction

function tube(nn)
// some tube like bodies.
  x=linspace(0,2*%pi,nn);
  //  atomic modell or so.
  y=0.1+[sin(linspace(0,%pi,15)),1.5*sin(linspace(0,%pi,10)),sin(linspace(0,%pi,15))];
  cosphi=repmat(cos(x),length(y),1);
  sinphi=repmat(sin(x),length(y),1);
  f=repmat(y',1,length(x));
  x1=f.*cosphi;     y1=f.*sinphi;
  z=repmat(linspace(-2,2,prod(size(y)))',prod(size(x)),1);
  [xx,yy,zz]=nf3d(x1,y1,z);
  plot3d1(xx,yy,zz,alpha=35,theta=70,colormap=jetcolormap(32));
endfunction


function cplxroot(n,m,varargin)
//cplxroot(n,m,T,A,leg,flags,ebox)
//CPLXROOT Riemann surface for the n-th root.
//       CPLXROOT(n) renders the Riemann surface for the n-th root.
//       CPLXROOT(), by itself, renders the Riemann surface for the cube root.
//       CPLXROOT(n,m) uses an m-by-m grid.  Default m = 20.
// Use polar coordinates, (r,theta).
// Cover the unit disc n times.

  function cplxmap(z,w,varargopt)
    x = real(z);
    y = imag(z);
    u = real(w);
    v = imag(w);
    M = max(u);
    m = min(u);
    s = ones(size(size(z)));
    //mesh(x,y,m*s,blue*s);
    //hold on
    [X,Y,U]=nf3d(x,y,u);
    [X,Y,V]=nf3d(x,y,v);
    Colors = sum(V,'r');
    Colors = Colors - min(Colors);
    Colors = 32*Colors/max(Colors);
    plot3d1(X,Y,U,varargopt(:),colors=Colors,colormap=jetcolormap(32));
  endfunction

  if nargin  < 1, n = 3; end
  if nargin  < 2, m = 20; end
  r = (0:m)'/m;
  theta = - %pi*(-n*m:n*m)/m;
  z = r * exp(%i*theta);
  s = r.^(1/n) * exp(%i*theta/n);
  cplxmap(z,s,varargin(:))
endfunction

function surfaces()
  xclear(); hole3d1();xclick();
  xclear(); sphere();xclick();
  xclear(); shell();xclick();
  xclear(); spiral();xclick();
  xclear(); rings();xclick();
  xclear(); torus1  ()    ;xclick();
  xclear(); moebius();xclick();
  xclear(); tube(30);xclick();
  xclear();cplxroot(3,10);xclick();
endfunction
