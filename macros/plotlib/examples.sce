// A faire : enlever un iso =%t ? 

t=linspace(0,50*%pi,2000);
x=t.*sin(t)/50;
y=sin(t).*cos(t);
z= t.^2 ./(50*%pi);
param3d(x',3*y',z'/max(z),alpha = 45,theta = 60,flag = [3,1],style ...
	= 5,ebox=[-3,3,-3,3,-3,3]);

xclick(); xclear();

t=linspace(0,50*%pi,2000);
x=t.*sin(t)/50;
y=sin(t/2).*cos(t);
z= t.^2 ./(50*%pi);
param3d(x',3*y',2*z'/max(z),alpha = 45,theta = 60,flag = [3,1],style ...
	= 5,ebox=[-3,3,-3,3,-3,3]);

xclick(); xclear();


t=linspace(0,50*%pi,2000);
x=t.*sin(t)/50;
y=sin(t/2).*cos(t);
z= t.^2 ./(50*%pi);
xdel(); 
xset('colormap',jetcolormap(64));
param3d(x',3*y',2*z'/max(z),alpha = 45, box_style='scilab', theta = 60,  ...
	line_color=64*t/max(t),line_thickness=3, ebox=[-3,3,-3,3,-1,3]);

xclick(); xclear();

function z=peaks(x,y)
   z = 3*(1-x)^2*exp(-x^2 - (y+1)^2) ...
       - 10*(x/5 - x^3 - y^5)*exp(-x^2-y^2) ...
       - 1/3*exp(-(x+1)^2 - y^2)
endfunction

// un example pour pcolor 
n=10;
y=linspace(0,2*%pi,n);
Y=y(:).*.ones(1,n);
X=Y;
for i=1:n
  X(i,:)=linspace(sin(y(i)),2.2+cos(y(i)),n)
end
xclear();
colormap(colormap=jetcolormap(32));
pcolor(X,Y,X+Y);

xclick(); xclear();

// une surface + une courbe 

t=linspace(-0.5,%pi-0.5,10);
[U,T]=meshgrid(t,t);
X=U.*sin(T);Y=sin(U).*cos(T);Z= T;
[mx,nx]=minmax(X);[my,ny]=minmax(Y);[mz,nz]=minmax(Z);
X= (X-mx)/(nx-mx);Y= (Y-my)/(ny-my);Z= (Z-mz)/(nz-mz);
//xset('thickness',1);
mesh(X,Y,Z);
hold('on');
p=linspace(0,1,1000);
U=p.*sin(10*%pi*p);T=p.*cos(10*%pi*p);
[mu,nu]=minmax(U);[mt,nt]=minmax(T);
U= (U-mu)/(nu-mu);T= (T-mt)/(nt-mt);
U=-0.4 + (%pi-0.2)*U;T=-0.4 + (%pi-0.2)*T;
X=U.*sin(T);Y=sin(U).*cos(T);Z= T;
X= (X-mx)/(nx-mx);Y= (Y-my)/(ny-my);Z= (Z-mz)/(nz-mz);
plot3(X,Y,Z,'r3',axis='off');
hold('off');

xclick(); xclear();

// un examples de triangles 

[V,F] = regular_polyhedron('dodecahedron');
[np,nf]=size(F);
[m,n]=size(V);
for i=1:nf
  Pts=V(:,F(1:3,i));
  V3=cross(Pts(:,1)-Pts(:,2),Pts(:,3)-Pts(:,1));
  C=sum(V(:,F(:,i)),'c')/5;
  V(:,n+i)=C - i*V3/norm(V3);
  F1(1,i)=n+i;
end
Fr=[F(1:2,:),F(2:3,:),F(3:4,:),F(4:5,:),F([5,1],:);
    F1,F1,F1,F1,F1];
trisurf(Fr',V(1,:)',V(2,:)',V(3,:)',axis=[-10,10,-10,10,-10,2]);

xclick(); xclear();

// animation 

for anim=1:400 
  r= ones(1,nf)*(1+sin(5*%pi*anim/100))*2;
  for i=1:nf
    Pts=V(:,F(1:3,i));
    V3=cross(Pts(:,1)-Pts(:,2),Pts(:,3)-Pts(:,1));
    C=sum(V(:,F(:,i)),'c')/5;
    V(:,n+i)=C - r(i)*V3/norm(V3);
    F1(1,i)=n+i;
  end
  A=get_current_objs3d(create=%f);
  if type(A,'short')=='objs3d' then A.children=list();end 
  Fr=[F(1:2,:),F(2:3,:),F(3:4,:),F(4:5,:),F([5,1],:);
    F1,F1,F1,F1,F1];
  trisurf(Fr',V(1,:)',V(2,:)',V(3,:)',axis={4*[-1,1,-1,1,-1,1],'equal'},shading='flat');
  xpause(10000,events = %t);
end

xclick(); xclear();

function surf_demo()
// Attention normalement X et Y sont inversés en Matlab 
// example with parametric case 
  x=linspace(-1,1,4);
  y=linspace(-1,1,7);
  function z=f(x,y); z=cos(%pi*x.*y); endfunction; 
  surf(x,y,f);

  xclick();xdel();
  
  // example with vector, vector, matrix
  x=-1:0.1:1;
  y=x;
  function z=f(x,y); z=cos(%pi*x.*y); endfunction;
  [X,Y]=meshgrid(x,y);
  Z=f(X,Y);
  surf(x,y,Z);
  xclick();xdel();
  
  // example with  vector, vector, f -> parametric case 
  u=linspace(0,2*%pi,50);
  v=linspace(0,%pi,25);v=v($:-1:1);
  function [x,y,z]=sphere(u,v)
    x=sin(v).*cos(u); y=sin(v).*sin(u);z=cos(v);
  endfunction 
  surf(u,v,sphere);
  xclick();xdel();
  
  // example with 3 matrices 
  
  x=linspace(0,2*%pi,10);
  y=linspace(0,%pi,5);
  function [x,y,z]=f(u,v)
    x=u;
    y=v;
    z=sin(v)+cos(u);
  endfunction;
  [X,Y]=meshgrid(x,y);
  [X,Y,Z]=f(X,Y);
  surf(X,Y,Z);
  xclick();xdel();
  //[X,Y,Z]=generate3dPolygons(X,Y,Z,4,%t);
  //plot3d1(X,Y,Z,colormap=jetcolormap(32));

endfunction


function z=peaks(x,y)
   z = 3*(1-x)^2*exp(-x^2 - (y+1)^2) ...
       - 10*(x/5 - x^3 - y^5)*exp(-x^2-y^2) ...
       - 1/3*exp(-(x+1)^2 - y^2)
endfunction

// exemple 

t=linspace(0,2*%pi+0.01,40);
Pts=[cos(t);sin(2*t);sin(3*t)];

theta=45; phi=90;
Mt=[cos(theta),-sin(theta),0;
    sin(theta), cos(theta),0;
   0,0,1];
Mp=[cos(phi),0,-sin(phi);
    0,1,0;
    sin(phi),0, cos(phi)];
Pts1=Mp*Mt*Pts;

[xx,yy,zz]=tubelet(Pts(1,:)',Pts(2,:)',Pts(3,:)',0.2,5);
// [xx,yy,zz]=ribbon(Pts(1,:)',Pts(2,:)',Pts(3,:)',0.2,5);
F=get_current_figure();
A=get_current_objs3d();
A.children=list();
plot3d1(xx,yy,zz,ebox=[-1,1,-1,1,-1,1]*2,colormap=jetcolormap(32))
A=get_current_objs3d();
S=A.children(1);
n=300;
phi= %pi/100;
cosp = cos(phi);
sinp = sin(phi);
Mp=[cosp,0,-sinp;
    0,1,0;
    sinp,0, cosp];
for i=1:n
  Pts1=Mp*S.Mcoord';
  S.Mcoord=Pts1';
  A.invalidate[]
  xpause(10000,events = %t);
end


