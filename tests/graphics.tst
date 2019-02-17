// -*- Mode: nsp -*-

// xinit(cairo=%t);

if ~exists('tpause') then tpause=1.e6;end
count=1;
if ~exists('ps') then ps =%f;end

//---------------------------------------------------------
xclear();x=0:0.1:2*%pi;plot2d(x,sin(x),style=[2],mode="std");
if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();
xclear();x=0:0.1:2*%pi;plot2d(x,sin(x),style=[2],mode="stairs");
if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();
xclear();x=0:0.1:2*%pi;plot2d(x,sin(x),style=[2],mode="stem");
if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();
xclear();x=0:0.1:2*%pi;plot2d(x,sin(x),style=[2],mode="arrow");
if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();
xclear();x=0:0.1:2*%pi;plot2d(x,sin(x),style=[2],mode="fill");
if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();
xclear();x=0:0.1:2*%pi;plot2d(x,sin(x),style=[2],mode="stairs_fill");
if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();
//---------------------------------------------------------
champ(-5:5,-5:5,rand(11,11),rand(11,11));
if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
champ1(-5:5,-5:5,rand(11,11),rand(11,11),arfact=2,rect=[-10,-10,10,10]);

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
x=linspace(-%pi,%pi,20);
y=x;
gray=xget('lastpattern')+3;
xsegs([x;x],[min(y)*ones(size(x));max(y)*ones(size(x))],style=gray);
xsegs([min(x)*ones(size(y));max(x)*ones(size(y))],[y;y],style=gray);
champ1(x,y,sin(x'*y),cos(x'*y));
[X,Y]=ndgrid(x,y);
plot2d(X(:),Y(:),line_color=-2,mark=13,mark_size=3,mark_color=1);

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear()

//---------------------------------------------------------
xset('colormap',hotcolormap(64));
A=1:64;plot2d([0,10],[0,10],style=0);Matplot1(A,[1,1,9,9]);
cmap=xget('colormap');
[n,m]=size(cmap)
// number of colors in then current colormap
n1= xget('lastpattern');
n1 == n

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear()

//---------------------------------------------------------
p1=linspace(0,2*%pi,10);
p2=linspace(0,2*%pi,10);
function [x,y,z]=scp(p1,p2)
  x=p1.*sin(p1).*cos(p2);
  y=p1.*cos(p1).*cos(p2);
  z=p1.*sin(p2);
endfunction
[x,y,z]=eval3dp(scp,p1,p2);
plot3d1(x,y,z);

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear()

//---------------------------------------------------------
x=-5:5;y=x;
function [z]=f(x,y); z= x.*y; endfunction;
z=eval3d(f,x,y);
plot3d(x,y,z);

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
n = 352*poly(-5,'s'); d= poly([0,0,2000,200,25,1],'s','coeffs');
evans(n,d,100);
P=3.0548543 - 8.8491842*%i;    //P=selected point
k=-1/real(horner(n,P){1}/horner(d,P){1});
roots(d+k*n)     //contains P as particular root

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
s=poly(0,'s');n=1+s;
d=real(poly([-1 -2 -%i %i],'s'));
evans(n,d,100);

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
n=real(poly([0.1-%i 0.1+%i,-10],'s'));
d=real(poly([-1 -2 -%i %i],'s'));
evans(n,d,80);

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear()

//---------------------------------------------------------
N=20;
n=1:N;
x=cos(n*2*%pi/N);
y=sin(n*2*%pi/N);
noeul=[(1:(N))',x',y',0*ones(N,1)];
noeul=[noeul;(N+1),0,0,0];
trianl=[(1:N)',[(2:N)';1],(N+1)*ones(N,1)];
triangles=[(1:N)',trianl,zeros(N,1)];
rect=[-1.2,-1.2,1.2,1.2];
z=(1:N+1)';
if new_graphics() then z(1:4)=%nan;end
xset('colormap',jetcolormap(32));
fec(noeul(:,2),noeul(:,3),triangles,z,strf='030',axesflag=2,rect=rect,mesh=%t);

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
// define a mini triangulation (4 vertices, 2 triangles)
x = [0 1 0 -1];
y = [0 0 1  1];
T = [1 1 2 3 1;
     2 3 4 1 1];
// values of then function at each vertices of the
// triangulation;
z = [0 1 0 -1];
xclear();
subplot(1,2,1)
xset("colormap",jetcolormap(64))
fec(x,y,T,z,strf="040",mesh=%t)
xtitle("fec example (with then mesh)")
subplot(1,2,2)
fec(x,y,T,z,strf="040")  // rmq: mesh=%f by default
xtitle("fec example (without then mesh)")

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
xclear();
xset("colormap",jetcolormap(64))
fec(x,y,T,z,strf="040", zminmax=[-0.5 0.5], mesh=%t)
xtitle("fec example : using zminmax argument")

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
xclear();
xset("colormap",jetcolormap(64))
subplot(2,2,1)
fec(x,y,T,z,strf="040", zminmax=[-0.5 0.5], colout=[0 0], mesh=%t)
xtitle("fec example : using zminmax and colout =[0 0]")
subplot(2,2,2)
fec(x,y,T,z,strf="040", zminmax=[-0.5 0.5], colout=[67 67], mesh=%t)
xtitle("fec example : using zminmax and colout =[67 67]")
subplot(2,2,3)
fec(x,y,T,z,strf="040", zminmax=[-0.5 0.5], colout=[-1 0], mesh=%t)
xtitle("fec example : using zminmax and colout =[-1 0]")
subplot(2,2,4)
fec(x,y,T,z,strf="040", zminmax=[-0.5 0.5], colout=[0 -1], mesh=%t)
xtitle("fec example : using zminmax and colout =[0 -1]")
xselect()

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
xset("colormap",[hotcolormap(64);greencolormap(64)])
subplot(1,2,1)
fec(x,y,T,z,strf="040", colminmax=[1 64], mesh=%t)
xtitle("fec using then hot colormap")
subplot(1,2,2)
fec(x,y,T,z,strf="040", colminmax=[65 128], mesh=%t)
xtitle("fec using then greencolormap")
xselect()

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear()

//---------------------------------------------------------
t=[0:0.3:2*%pi]'; z=sin(t)*cos(t');
[xx,yy,zz]=genfac3d(t,t,z);
plot3d(xx,yy,zz)

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear()

//---------------------------------------------------------
x=-10:10; y=-10:10;m =rand(21,21);
grayplot(x,y,m,rect=[-20,-20,20,20])

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
x=-10:10; y=-10:10;m = 64*rand(21,21);
xset('colormap',jetcolormap(32));
grayplot(x,y,m,rect=[-20,-20,20,20] ,remap=%f);

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
function z=f(x,y); z = 30*sin(x)*cos(y);endfunction
t=-%pi:0.1:%pi;
xset('colormap',jetcolormap(32));
grayplot(t,t,f,remap=%t,shade=%t,zminmax=[6,24],colminmax=[6,24],colout=[36,35])

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear()

//---------------------------------------------------------
u = linspace(-%pi/2,%pi/2,40);
v = linspace(0,2*%pi,20);
x = cos(u)'*cos(v);
y = cos(u)'*sin(v);
z = sin(u)'*ones(size(v));
[xx,yy,zz]=nf3d(x,y,z); plot3d(xx,yy,zz)

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear()

//---------------------------------------------------------
t=-%pi:0.3:%pi;plot3d(t,t,sin(t)'*cos(t));
if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
t=-%pi:0.3:%pi;plot3d1(t,t,sin(t)'*cos(t));

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
t=-%pi:0.3:%pi;plot3d(t,t,sin(t)'*cos(t),mesh_only=%t);

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
t=-%pi:0.3:%pi;plot3d(t,t,sin(t)'*cos(t),mesh=%f);

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
function z=f(x,y,args) z=sin(args(1)*x)*cos(args(2)*y);
endfunction;
// then optional argument arg is used to pass extra arguments to function f.
t=-%pi:0.3:%pi;plot3d(t,t,f,args=list(1,2));

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
t=-%pi:0.3:%pi;
z=sin(t)'*cos(t);
[xx,yy,zz]=genfac3d(t,t,z);
plot3d(xx,yy,zz,colors=linspace(1,40,400),colormap=hotcolormap(40));
// remode shade i.e one color per face

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
plot3d(xx,yy,zz,colors=linspace(1,40,400),colormap=hotcolormap(40),shade=%f);

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
t=-%pi:0.3:%pi;
z=sin(t)'*cos(t);
[xx,yy,zz]=genfac3d(t,t,z);
z=sin(2*t)'*cos(t);
[m,n]=size(xx);
[xx1,yy1,zz1]=genfac3d(t,t,z);
plot3d([xx,xx1],[yy,yy1],[zz,zz1],colors=[20*ones(1,n),30*ones(1,n)],colormap=hotcolormap(40));

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear()

//---------------------------------------------------------
z=sin(t)'*cos(t);
[xx,yy,zz]=genfac3d(t,t,z);
[m,n]=size(xx);
plot3d(xx,yy,zz,colors=[20*ones(1,n)],colormap=hotcolormap(40));
z=sin(2*t)'*cos(t);
[xx,yy,zz]=genfac3d(t,t,z);
plot3d(xx,yy,zz,colors=[30*ones(1,n)],colormap=hotcolormap(40));

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear()

//---------------------------------------------------------
t= 0:.01:2*%pi;
polarplot(sin(3*t),cos(2*t),color=10)
// add a second curve
theta= 2*%pi*linspace(0,1,50); rho=linspace(0,1,50);
xpoly(rho.*cos(theta),rho.*sin(theta),color=7,thickness=3);

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear()

//---------------------------------------------------------
x=-10:10; y=-10:10;m =rand(21,21);
xset("colormap",hotcolormap(64))
Sgrayplot(x,y,m, colorbar=%t);

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
n = 30;
nt = 100;
x = linspace(0,2*%pi,n);
y = linspace(0,%pi,n/2);
z = sin(x')*sin(y);
t = linspace(0,4*%pi,nt);
xset("colormap",jetcolormap(64));
Sgrayplot(x,y,cos(t(1))*z, colorbar=%t);
F=get_current_figure[];
fecd=F.children(1).children(1);
for i=1:nt
  fecd.func = cos(t(i))*z;
  fecd.invalidate[];
end
if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
function z=surf1(x,y), z=x*y, endfunction
function z=surf2(x,y), z=x^2-y^2, endfunction
function z=surf3(x,y), z=x^3+y^2, endfunction
function z=surf4(x,y), z=x^2+y^2, endfunction
xset("colormap",[jetcolormap(64);hotcolormap(64)])
x = linspace(-1,1,60);
y = linspace(-1,1,60);
subplot(2,2,1)
Sfgrayplot(x,y,surf1,axesflag=0,colorbar=%t,colminmax=[1,64]);
xtitle("f(x,y) = x*y")
subplot(2,2,2)
Sfgrayplot(x,y,surf2,axesflag=0,colorbar=%t,colminmax=[65,128])
xtitle("f(x,y) = x^2-y^2")
subplot(2,2,3)
Sfgrayplot(x,y,surf3,axesflag=0,colorbar=%t,colminmax=[65,128])
xtitle("f(x,y) = x^3+y^2")
subplot(2,2,4)
Sfgrayplot(x,y,surf4,axesflag=0,colorbar=%t,colminmax=[1,64])
xtitle("f(x,y) = x^2+y^2")

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear()

//---------------------------------------------------------
xsetech(frect=[-1,-1,1,1]);
arcs=[-1.0 0.0 0.5; // upper left x
      1.0 0.0 0.5; // upper left y
      0.5 1.0 0.5; // width
      0.5 0.5 1.0; // height
      0.0 0.0 0.0; // angle 1
      180*64 360*64 90*64]; // angle 2
xarcs(arcs,color=[1,2,3],thickness=[3,3,3],background=[-2,5,6])

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear()

//---------------------------------------------------------
n=100;x=rand(1,n);y=rand(1,n);w=0.1*rand(1,n);h=0.1*rand(1,n);
arcs=[x;y;w;h;0*ones(1,n);360*64*ones(1,n)];
xset('colormap',jetcolormap(32));
xarcs(arcs,color=rand(1,n)*32,background=rand(1,n)*32);

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear()

//---------------------------------------------------------
function plot2d_arcs(x,y,s,varargopt)
  plot2d(x,y,varargopt(:));
  z=ones(1,size(x,'*'));
  arcs=[x(:)'-s/2;y(:)'+s/2;s*z;s*z;0*z;360*64*z];
  v.background= z* varargopt.find['line_color',def=-1];
  v.color=z;
  v.thickness=z;
  xarcs(arcs,v(:));
endfunction

x=1:20;y=x.*sin(x);plot2d_arcs(x,y/3,0.3,line_color=6);

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear()

//---------------------------------------------------------
xsetech(frect=[-2,-2,2,2],iso=%t,axesflag=0);
xarc(-1,1,2,2,0,90*64,background=7,color=3);
xarc(-1.5,1.5,3,3,0,360*64)

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear()

//---------------------------------------------------------
x=2*%pi*linspace(0,1,30);x($)=[];
x1=[sin(x);(1.5+x).*sin(x)];
y1=[cos(x);(1.5+x).*cos(x)];
xsetech(frect=[-8,-8,6,10],fixed=%t,clip=%t,iso=%t);
xset('colormap',jetcolormap(32));
xarrows(x1,y1,arsize=0.5,style=1:length(x));

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear()

//---------------------------------------------------------
xsetech(frect=[-1,-1,1,1]);
arcs=[-1.0 0.0 0.5; // upper left x
      1.0 0.0 0.5; // upper left y
      0.5 1.0 0.5; // width
      0.5 0.5 1.0; // height
      0.0 0.0 0.0; // angle 1
      180*64 360*64 90*64]; // angle 2
xfarcs(arcs,color=[1,2,3],thickness=[3,3,3],background=[-2,5,6])

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear()

//---------------------------------------------------------
xsetech(frect=[-2,-2,2,2],iso=%t,axesflag=0);
xfarc(-1,1,2,2,0,90*64,background=7,color=3);
xarc(-1.5,1.5,3,3,0,360*64)

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear()

//---------------------------------------------------------
xsetech(frect=[-10,-10,210,40]);
x1=[0,10,20,30,20,10,0]';
y1=[15,30,30,15,0,0,15]';
xpols=[x1 x1 x1 x1]; xpols=xpols+[0,60,120,180].*.ones(size(x1));
ypols=[y1 y1 y1 y1];
xfpolys(xpols,ypols,[-1,0,10,13])

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
xsetech(frect=[-10,-10,210,40]);
x1=[0,10,20,30,20,10,0]';
y1=[15,30,30,15,0,0,15]';
xpols=[x1 x1 x1 x1]; xpols=xpols+[0,60,120,180].*.ones(size(x1));
ypols=[y1 y1 y1 y1];
xfpolys(xpols,ypols,color=[-2,-1,5,6], fill_color=[4,-2,10,13],thickness=3);

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
function plot2d_polys(x,y,n,s,varargopt)
  plot2d(x,y,varargopt(:));
  z=ones(1,size(x,'*'));
  p=[x(:)';y(:)'];
  qx=p;qy=p;
  for i=1:n
    qx(i,:)=p(1,:)+s/2*cos(2*%pi*i/n);
    qy(i,:)=p(2,:)+s/2*sin(2*%pi*i/n);
  end
  v.fill_color= z* varargopt.find['line_color',def=-1];
  v.color=z;
  xfpolys(qx,qy,v(:));
endfunction
x=1:20;y=x.*sin(x);plot2d_polys(x,y/3,5,1.0,line_color=5)

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear()

//---------------------------------------------------------
x=sin(2*%pi*(0:4)/5);
y=cos(2*%pi*(0:4)/5);
xsetech(frect=[-2,-2,2,2])
xfpoly(x,y,color=6,fill_color=4,thickness=3);

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear()

//---------------------------------------------------------
xsetech(frect=[-2,-2,2,2]);
xfrect(-1,1,2,2,color=6,thickness=4,stroke_color=3);

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear()

//---------------------------------------------------------
xsetech(frect=[-2,-2,2,2]);
xfrect(-1,1,2,2,color=-1,thickness=4,stroke_color=3);
// we change then default color
xset('color',10);
F=get_current_figure();
F.invalidate[];// force redrawing;

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear()

//---------------------------------------------------------
xsetech(frect=[-100,-100,500,600]);
x=0:100:200;
xnumb(x,500*ones(size(x)),[10,20,35],1)

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear()

//---------------------------------------------------------
N=9;z=linspace(0,1,N)';
x=0.05*cos(2*%pi*z);
y=0.05*sin(2*%pi*z);
xsetech(frect=2*[-1,-1,1,1]);
xset('colormap',jetcolormap(32));
// three polylines
A=xpolys([x,x+0.2,x],[y,y,y+0.2],color=[1,1,1],close=%t,thickness=[1:3]);
// animate then set of polylines
for i=1:32
  A.invalidate[];
  for j=1:length(A.children),
    A.children(j).color=i;
    A.children(j).rotate[[cos(%pi/10),sin(%pi/10)]];
    A.children(j).translate[[0.05*j,0]];
    A.children(j).scale[[1.05,1.05]];
  end
  A.invalidate[];
  xpause(100000,%t)
end

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear()

//---------------------------------------------------------
x=sin(2*%pi*(0:4)/5);
y=cos(2*%pi*(0:4)/5);
xsetech(frect=[-2,-2,2,2])
xpoly(x,y,mark=4,mark_size=20,mark_color=6,close=%t,color=-2);

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
x=sin(2*%pi*(0:4)/5);
y=cos(2*%pi*(0:4)/5);
xsetech(frect=[-2,-2,2,2])
xpoly(x,y,close=%f,color=3,thickness=4);

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
x=sin(2*%pi*(0:4)/5);
y=cos(2*%pi*(0:4)/5);
xsetech(frect=[-2,-2,2,2])
xpoly(x,y,mark=4,mark_size=20,mark_color=6,close=%t,color=2,thickness=0);

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear()

//---------------------------------------------------------
plot2d([-100,500],[-50,50],style=[-1,-1],strf="022")
cols=[-34,-33,-32,-20:5:20,32,33,34];
x=400*(0:14)/14; step=20;
rects=[x;10*ones(size(x));step*ones(size(x));30*ones(size(x))];
xrects(rects,cols)
xnumb(x,15*ones(size(x)),cols)

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear()

//---------------------------------------------------------
plot2d([-100,500],[-50,50],style=[-1,-1],strf="022")
x=400*(0:14)/14; step=20;
rects=[x;10*ones(size(x));step*ones(size(x));30*ones(size(x))];
background=[4*ones(1,5),1:5,6*ones(1,5)];
color=[6*ones(1,5),ones(1,5),4*ones(1,5)];
thickness=[4*ones(1,5),ones(1,5),4*ones(1,5)];
xrects(rects,background=background,color=color,thickness=thickness);

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear()

//---------------------------------------------------------
plot2d([-2,2],[-2,2],style=[-1,-1],strf="022");
xrect(-1,1,2,2,color=6,thickness=4,background=3);

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear()

//---------------------------------------------------------
plot2d([-2,2],[-2,2],style=[-1,-1],strf="022");
xrect(-1,1,2,2,color=-1,thickness=4,background=3);
// we change then default color
xset('color',10);
F=get_current_figure();
F.invalidate[];// force redrawing;

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear()

//---------------------------------------------------------
x=2*%pi*(0:9)/10;
xv=[sin(x);9*sin(x)];
yv=[cos(x);9*cos(x)];
xsetech(frect=[-10,-10,10,10],fixed=%t);
xsegs(xv,yv,style=1:10);

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear()

//---------------------------------------------------------
xsetech(wrect=[0,0,1.0,0.5],frect=[-5,-3,5,3])
plot2d([1:10]',[1:10]',style=1,strf="001")
xsetech(wrect=[0,0.5,1.0,0.5],axesflag=4)
plot2d([1:10]',[1:10]',strf='014',rect=[-6,-6,6,6]);

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
xsetech(wrect=[0,0,0.5,0.5],a3d=%t); plot3d()
xsetech(wrect=[0.5,0,0.5,0.5]); plot2d()
xsetech(wrect=[0.5,0.5,0.5,0.5]); grayplot()
xsetech(wrect=[0,0.5,0.5,0.5]); histplot()

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
xsetech(wrect=[0,0,1.0,0.5],arect=1/8*ones(1,4));
x=1:0.1:10;plot2d(x',sin(x)');
xsetech(wrect=[0,0.5,1.0,0.5],arect=[2/8,2/8,1/16,1/4]);
x=1:0.1:10;plot2d(x',sin(x)');

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear()

//---------------------------------------------------------
xsetech(frect=[0,0,2,2],iso=%t);
xstring(1,1,["Nsp";"Graphics"])

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
xsetech(frect=[0,0,2,2],iso=%t);
xset('font size',3);
for A=[0:30:330];
  x=1+0.5*cos(-%pi*A/180);y=1+0.5*sin(-%pi*A/180);
  xstring(x,y,'Nsp',A,0,color=A/10);
  xsegs([1;x],[1;y]);
end

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
xsetech(frect=[-1.0,-1.0,1.0,1.0],iso=%t);
w=0.2,h=0.2;
for A=[0:30:330];
  x=0.5*cos(-%pi*A/180);y=0.5*sin(-%pi*A/180);
  // (x,y) is upper-left point for xrect
  R=xrect(-w/2,h/2,w,h);
  R.rotate[[cos(-%pi*A/180),sin(-%pi*A/180)]];
  R.translate[[x,y]];
  // (x,y) is lower-left point for xstring
  xstring(x-w/2,y-h/2,'Nsp',fill=%t,w=w,h=h,angle=A,color=A/10);
  xsegs([0;x],[0;y]);
end

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear()

//---------------------------------------------------------
x=(1:10)';plot2d(x,x);
xtitle(['Titre';'Principal'],'x legend ','y legend');

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
contour2d(1:10,1:10,rand(10,10),5,rect=[0,0,11,11],style=1:5)
// changing then format of then printing of then levels
xset("fpf","%.2f");
F=get_current_figure();
F.invalidate[];

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
xset('colormap',jetcolormap(11));
x=linspace(0,2*%pi,20);y=linspace(1,10,50);
levels=-10:2:10;
xset('fpf',' ');
contour2d(x,y,sin(x)'*y,levels,style=1:11)
for i=1:11, plot2d(0,0,line_color=i,leg=string(levels(i)),leg_pos='urm');end
xtitle("level curves of function sin(x)*y");

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
xset('colormap',jetcolormap(11));
x=linspace(0,2*%pi,20);y=linspace(1,10,50);
levels=-10:2:10;
contourf(x,y,sin(x)'*y,nv=levels);
xtitle("level curves of function sin(x)*y");

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
[xc,yc]=contour2di(1:10,1:10,rand(10,10),5);
k=1;n=yc(k);c=1;
while k+yc(k) < size(xc,'*')
  n=yc(k);
  plot2d(xc(k+(1:n)),yc(k+(1:n)),line_color=c)
  c=c+1;
  k=k+n+1;
end

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
xset('colormap',jetcolormap(100));
Matplot(testmatrix('magic',10))

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
Matplot((1:xget("lastpattern")))

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
N=32;
C=[jetcolormap(N);graycolormap(N);hotcolormap(N)];
xset('colormap',C);
M=[1:N;N+(1:N);2*N+(1:N)];
Matplot1(M,[1,1,32,3],rect=[-10,1,32,3],axesflag=2);
xstring(-8,1.4,'hotcolormap');
xstring(-8,2.0,'graycolormap');
xstring(-8,2.6,'jetcolormap');

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
N=32;C=[jetcolormap(N)]; xset('colormap',C);
Matplot(1:32,remap=%t,zminmax=[10,20],colminmax=[20,32]);

if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();

//---------------------------------------------------------
function y=f(t,x)
  y=[0.8*x(1)*(1 - 0.5*x(2));
     0.2*x(2)*(x(1)-3)];
endfunction
x=linspace(0,6,20);y=x;
[X,Y]=ndgrid(x,y);
m=size(x,'*');n=size(y,'*');
fx=zeros(m,n);fy=fx;
for i=1:size(X,'*');
  yv=f(0,[X(i);Y(i)]);
  fx(i)=yv(1); fy(i)=yv(2);
end
n=size(x,'*');
fx.redim[n,-1];fy.redim[n,-1];
xset('colormap',jetcolormap(64));
champ1(x,y,fx,fy);

t = linspace(0,10,100);
xo = ode([4;3],0,t,f);
plot2d(xo(1,:),xo(2,:),line_color=1,line_thickness=2)
if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();
//---------------------------------------------------------

function hinton(A,varargopt)
  xsetech(axesflag=2);
  xset('colormap',graycolormap(8));
  [m,n]=size(A);
  x=1:m;y=1:n;  C=-sign(A);
  [X,Y]=ndgrid(x,y);
  X.redim[1,-1]; Y.redim[1,-1];
  w=  abs(A(:))'/max(abs(A));
  rects=[Y-w./2;X(m*n:-1:1)+w./2;w;w];
  xfrect([0,m+1,n+1,m+1],color=11);
  xrects(rects,color=4*(1+C(:)'),background=4*(1+C(:))');
endfunction

hinton(randn(20,20));
if ps then xexport(0,'file'+string(count)+'.eps');count=count+1;end
xpause(tpause,%t);xclear();
//---------------------------------------------------------
