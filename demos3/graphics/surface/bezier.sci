function gammatest (N)
// Bezier curve approximating a circle
// Copyright Enpc
  if nargin <= 0 then N=[10,20,50,100];end
  x=linspace(0,1,100);
  plot2d(cos(2*%pi*x)',sin(2*%pi*x)',style=1,strf="151",rect=[-2,-2,2,2]);
  xtitle('Bezier curve approximating a circle')
  icol=2
  for n=N ; 
    t=sqrt(linspace(0,1,n));
    p=[cos(2*%pi*t);sin(2*%pi*t)];
    y=bezier(p,x);
    plot2d(y(1,:)',y(2,:)',style=icol,strf="000");
    icol=icol+1;
  end;
endfunction

function beziertest()
// a random polygon and a bezier curve 
// Copyright Enpc
  plot2d([],[],strf="011",rect=[-0.2,-0.2,1.2,1.2]);
  xtitle('Bezier Test : random polygon and bezier curve')
  p=rand(2,5);
  t=linspace(0,1,300);
  s=bezier(p,t);
  plot2d(p(1,:)',p(2,:)',style=1,strf="000");
  plot2d(s(1,:)',s(2,:)',style=2,strf="000");
endfunction

function bezier3dtest ()
// Show a Beziercurve of dimension 3
// Copyright Enpc
  p=[-1,-1,-1;0,-1,-1;1,0,0;1,1,0;0,1,1;-1,1,0]';
  bezier3d(p);
endfunction

function beziersurftest()
// Show a Bezier surface
// Copyright Enpc
  x=linspace(-%pi,%pi,5);
  [x,y]=ndgrid(x,x);x=x';y=y';
  z= 3*sin(x).*cos(y);
  [xb,yb,zb]=beziersurface(x,y,z);
  [xx,yy,zz]=nf3d(x,y,z);
  xsetech(wrect=[0,0,1.0,0.5],a3d = %t); 	
  plot3d1(xx,yy,zz,colormap=jetcolormap(32));
  xtitle('A first surface ');
  xsetech(wrect=[0,0.5,1.0,0.5],a3d = %t)
  [xx,yy,zz]=nf3d(xb,yb,zb);
  plot3d1(xx,yy,zz,colormap=jetcolormap(32));
  xtitle('The bezier interpolated surface (n=10)');
endfunction

function c1test()
// Show how two bezier surfaces can be joined.
// Copyright Enpc
// first surface 
  x1=repmat(-0.5:0.25:0.5,5,1);
  y1=repmat([0,0,0,0,1],5,1);
  z1=repmat(2:0.25:3,5,1)';
  [xb1,yb1,zb1]=beziersurface(x1,y1,z1,10);
  // second surface 
  x2=repmat(-0.5:0.25:0.5,5,1);
  y2=[(ones_new(4,5));[0,0,0,0,0]];
  z2=-repmat(-1:0.25:0,5,1)';
  [xb2,yb2,zb2]=beziersurface(x2,y2,z2,10);
  // a surface to link the two previous ones 
  x=zeros_new(5,5); y=x; z=x;

  x(1,:)=x1(1,:); x(2,:)=x(1,:)-(x1(2,:)-x1(1,:));
  x(5,:)=x2(1,:); x(4,:)=x(5,:)-(x2(2,:)-x2(1,:));
  x(3,:)=(x(4,:)+x(2,:))/2;
  y(1,:)=y1(1,:); y(2,:)=y(1,:)-(y1(2,:)-y1(1,:));
  y(5,:)=y2(1,:); y(4,:)=y(5,:)-(y2(2,:)-y2(1,:));
  y(3,:)=(y(4,:)+y(2,:))/2;
  z(1,:)=z1(1,:); z(2,:)=z(1,:)-(z1(2,:)-z1(1,:));
  z(5,:)=z2(1,:); z(4,:)=z(5,:)-(z2(2,:)-z2(1,:));
  z(3,:)=(z(4,:)+z(2,:))/2;
  A=35,T=50,L=" ",EB=[4,2,0]
  [xb,yb,zb]=beziersurface(x,y,z,10);
  xsetech(wrect=[0,0,0.5,0.5],a3d = %t);
  xtitle('how two bezier surfaces can be joined');
  [xx,yy,zz]=nf3d(xb1,yb1,zb1);
  plot3d(xx,yy,zz,alpha=A,theta=T,flag=EB); 
  xsetech(wrect=[0,0.5,0.5,0.5],a3d=%t);
  [xx,yy,zz]=nf3d(xb2,yb2,zb2);
  plot3d(xx,yy,zz,alpha=A,theta=T,flag=EB); 
  xsetech(wrect=[0.5,0.0,0.5,1.0],a3d=%t);
  [n1,p1]=size(xb1);
  [n2,p2]=size(xb);
  [xx,yy,zz]=nf3d([xb1;xb;xb2],[yb1;yb;yb2],[zb1;zb;zb2]);
  plot3d(xx,yy,zz,alpha=A,theta=T,flag=EB); 
endfunction


// library 



function [z] = bezier(p,t)
//comment : Computes a  Bezier curves.
//For a test try:
//beziertest; bezier3dtest; nurbstest; beziersurftest; c1test;
//Uses the following functions:
//bezier, bezier3d, nurbs, beziersurface
//endcomment
//reset();
// Evaluate sum p_i B_{i,n}(t) the easy and direct way.
// p must be a k x n+1 matrix (n+1) points, dimension k.
// Copyright INRIA
  n=size(p,'c')-1;// i=nonzeros(t~=1);
  t1=(1-t); t1z= find(t1==0.0); t1(t1z)= ones(size(t1z));
  T=repmat(t./t1,n,1)';
  T1 = repmat((n-(1:n)+1)./(1:n),size(t,'c'),1);
  b=[((1-t').^n),(T.*T1)];
  b=cumprod(b,'c');
  T3=repmat([ 0*ones_new(1,n),1],size(t1z,'c'),1);
  if (size(t1z,'c')>0); b(t1z,:)= T3;end
  z=p*b';
endfunction

function bezier3d (p)
// Shows a 3D Bezier curve and its polygon
// Copyright INRIA
  t=linspace(0,1,300);
  s=bezier(p,t);
  d=xget("color");
  xset("color",3)
  param3d(p(1,:),p(2,:),p(3,:),alpha=34,theta=45)
  xset("color",4);
  param3d(s(1,:),s(2,:),s(3,:),alpha=34,theta=45,leg="x@y@z",flag=[0,0])
  xset("color",d);
  xtitle('A 3d polygon and its Bezier curve');
endfunction	

function [X,Y,Z]=beziersurface (x,y,z,n)
// Compute a Bezier surface. Return {bx,by,bz}.
// Copyright INRIA
  if nargin <= 3 ; n=20;end 
  t=linspace(0,1,n);
  n=size(x,'r')-1; // i=nonzeros(t~=1);
  t1=(1-t); t1z= find(t1==0.0); t1(t1z)= ones(size(t1z));
  T=repmat(t./t1,n,1)';
  T1 = repmat((n-(1:n)+1)./(1:n),size(t,'c'),1);
  b1=[((1-t').^n),(T.*T1)];
  b1=cumprod(b1,'c');
  if (size(t1z,'c')>0); 
    T3=repmat([ 0*ones_new(1,n),1],size(t1z,'c'),1);
    b1(t1z,:)= T3;
  end
  n=size(x,'c')-1; // i=nonzeros(t~=1);
  t1=(1-t); t1z= find(t1==0.0); t1(t1z)= ones(size(t1z));
  T=repmat(t./t1,n,1)';
  T1 = repmat((n-(1:n)+1)./(1:n),size(t,'c'),1);
  b2=[((1-t').^n),(T.*T1)];
  b2=cumprod(b2,'c');
  if (size(t1z,'c')>0) then
    T3=repmat([ 0*ones_new(1,n),1],size(t1z,'c'),1);
    b2(t1z,:)= T3;
  end;
  X=b1*x*b2';Y=b1*y*b2';Z=b1*z*b2';
endfunction











