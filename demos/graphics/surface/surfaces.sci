function hole3d()
// Holes in surfaces using %inf 
// Copyright INRIA
  t=linspace(-%pi,%pi,40);z=sin(t)'*cos(t);
  z1=find(abs(z) > 0.5);
  z(z1)=%inf*z1;
  plot3d1(t,t,z);
endfunction

function hole3d1()
// Holes in surfaces using %inf 
  function [x,y,z]=sph(alp,tet)
    x=r*cos(alp).*cos(tet)+orig(1)*ones(size(tet));
    y=r*cos(alp).*sin(tet)+orig(2)*ones(size(tet));
    z=r*sin(alp)+orig(3)*ones(size(tet));
  endfunction
  r=1;orig=[0 0 0];
  x=linspace(-%pi/2,%pi/2,40);y=linspace(0,%pi*2,20); 
  x(5:8)=%inf*ones(size(5:8));  
  x(30:35)=%inf*ones(size(30:35)); 
  [x1,y1,z1]=eval3dp(sph,x,y);
  plot3d1(x1,y1,z1)      
endfunction

function sphere()
  u = linspace(-%pi/2,%pi/2,40);
  v = linspace(0,2*%pi,20);
  x= cos(u)'*cos(v);
  y= cos(u)'*sin(v);
  z= sin(u)'*ones(size(v));
  [xx,yy,zz]=nf3d(x,y,z);
  plot3d1(xx,yy,zz);
endfunction

function shell()
  u = linspace(0,2*%pi,40);
  v = linspace(0,2*%pi,20);
  x= (cos(u).*u)'*(1+cos(v)/2);
  y= (u/2)'*sin(v);
  z= (sin(u).*u)'*(1+cos(v)/2);
  [xx,yy,zz]=nf3d(x,y,z);
  plot3d1(xx,yy,zz);
endfunction

function spiral()
  [r,a]=field(0:0.1:1,0:%pi/8:6*%pi);
  z=a/8;
  x=r.*cos(a).*(1-a/20);
  y=r.*sin(a).*(1-a/20);
  z=z-1.5;
  [xx,yy,zz]=nf3d(x,y,z);
  plot3d1(xx,yy,zz);
endfunction

function rings()
  rr=0.2;
  t=linspace(0,2*%pi,10);
  s=linspace(0,2*%pi,41); n=length(s);
  r=dup(1+cos(t)*rr,n)'; m=length(t);
  x=dup(cos(s),m).*r; y=dup(sin(s),m).*r;
  z=dup(sin(t)*rr,n)';
  // ring1 
  [xx1,yy1,zz1]=nf3d(x,y,z);
  // ring2 
  [xx2,yy2,zz2]=nf3d(x+1.3,-z,y);
  // ring3 
  [xx3,yy3,zz3]=nf3d(x-1.3,-z,y);
  plot3d([xx1,xx2,xx3],[yy1,yy2,yy3],[zz1,zz2,zz3]);
endfunction

function torus()
// some torus type bodies.
  x=linspace(0,2*%pi,40);
  y=linspace(0,2*%pi,20)';
  // a torus with a thick and a thin side.
  factor=1.5+cos(y)*(cos(x)/2+0.6);
  X=factor*diag(cos(x));
  Y=factor*diag(sin(x));
  Z=sin(y)*(cos(x)/2+0.6);
  [xx,yy,zz]=nf3d(X,Y,Z);
  plot3d1(xx,yy,zz);
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
  plot3d1(xx,yy,zz);
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
  plot3d1(xx,yy,zz);
endfunction

function tube(nn)
// some tube like bodies.
  x=linspace(0,2*%pi,nn);
  //  atomic modell or so.
  y=0.1+[sin(linspace(0,%pi,15)),1.5*sin(linspace(0,%pi,10)),sin(linspace(0,%pi,15))];
  cosphi=dup(cos(x),length(y));
  sinphi=dup(sin(x),length(y));
  f=dup(y',length(x));
  x1=f.*cosphi;     y1=f.*sinphi;
  z=dup(linspace(-2,2,prod(size(y)))',prod(size(x)));
  [xx,yy,zz]=nf3d(x1,y1,z);
  plot3d1(xx,yy,zz,alpha=35,theta=70);
endfunction

function bh(nn)
// a black hole
  x=linspace(0,2*%pi,nn);
  t=linspace(0,1,20);
  cosphi=dup(cos(x),length(t));
  sinphi=dup(sin(x),length(t));
  f=dup((t.*t+0.2)',length(x));
  x=f.*cosphi;y=f.*sinphi ;z=dup(t'.*2-1,length(x));
  [xx,yy,zz]=nf3d(x,y,z);
  plot3d1(xx,yy,zz,alpha=70,theta=64);
endfunction

//xclear(); hole3d()
//xclear(); hole3d1()
xclear(); sphere();pause;
xclear(); shell();pause;
xclear(); spiral();pause;
xclear(); rings();pause;
xclear(); torus();pause;
xclear(); torus1  ()    ;pause;
xclear(); moebius();pause;
xclear(); tube(30);pause;
xclear(); bh(30);pause;
