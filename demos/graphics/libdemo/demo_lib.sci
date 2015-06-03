//---------------------------------------------------
// Copyright (C) 2004-2015 Jean-Philippe Chancelier Cermics/Enpc
// jpc@cermics.enpc.fr
// NSP  graphic demo
//
// Some functions have different copyright (from scilab)
// see below.
//---------------------------------------------------


function demo_lib()
// A set of function for graphic demos
endfunction

function demo_2d_1()
  t=linspace(0,6*%pi,100);
  plot2d(t,sin(t),style=2);
  xtitle("One curve","Time","sin(t)");
  xgrid();
endfunction

function demo_2d_2()
  t=linspace(0,6*%pi,100);
  st=["stem","stairs","fill","stairs_fill"];
  opts=hash(5,leg_pos="ul",line_color=2);
  for i=1:4
    subplot(2,2,i);
    opts.mode=st(i);
    opts.leg=sprintf("mode=''%s''",st(i));
    plot2d(t,t.*sin(t),opts(:));
  end
endfunction

function demo_2d_3()
  x=1:10:10000;y=log(x)/log(10);
  plot2d(x,y,style=5,logflag="ln",leg="log10(x)",leg_pos="ul");
  plot2d(x,sin(y),style=2,leg="sin(log10(x))",leg_pos="ul");
  xtitle("log(x) and sin(log10(x)) in log scale","x","y");
  xgrid();
endfunction

function demo_2d_4()
  n=32-1;t=(0:n)./n;
  u=sin(80*%pi*t)+sin(100*%pi*t);
  plot2d([],randn(100,1),mode="stem");
  xtitle("stem plot","t","f(t)");
endfunction

function demo_2d_5()
  function y=f(x) ; y=sin(1/x); endfunction
  plot2d(linspace(1.e-6,1,1000),f,style=2,leg="sin(1/x)",leg_pos="dr")
  xtitle("plot2d(x,f,...)");
  xgrid()
endfunction

function demo_2d_6()
  histplot();
endfunction

function demo_2d_7()
  t=linspace(0,6*%pi,30)';// column vector
  opts=hash(5,line_color=[2,5],mark=[10,11],mark_color=[2,5],mark_size=[4,4]);
  plot2d(t,[sin(t),t.*cos(t)],opts(:),leg='sin@cos',leg_pos="urm");
  xtitle("Curves with marks","","");
  opts=hash(5,line_color=[-2],mark=[9],mark_color=[6],mark_size=[4]);
  plot2d(t,exp(t/5),opts(:),leg='exp',leg_pos="urm");
  xgrid();
endfunction

function demo_2d_8()
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
  xtitle("Hinton Diagram","","");
  hinton(randn(20,20));
endfunction

function demo_2d_9()
  function plot2d_arcs(x,y,s,varargopt)
    plot2d(x,y,varargopt(:));
    z=ones(1,size(x,'*'));
    arcs=[x(:)'-s/2;y(:)'+s/2;s*z;s*z;0*z;360*64*z];
    v.background= z* varargopt.find['line_color',def=-1];
    v.color=z;
    v.thickness=z;
    xarcs(arcs,v(:));
  endfunction

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

  xtitle("Mixing plo2d and xarcs or xpolys");
  // iso mod to have circles
  x=1:20;y=x.*sin(x);plot2d_arcs(x,y/3,1.0,iso=%t,line_color=6);
  x=1:20;y=sin(x);plot2d_polys(x,y/3,5,0.7,line_color=5);
  xgrid();
endfunction

function demo_2d_10()
  xtitle("vector field and trajectories");
  function y=f(t,x)
    y=[0.8*x(1)*(1 - 0.5*x(2));
       0.2*x(2)*(x(1)-3)];
  endfunction
  x=linspace(0,6,20);y=linspace(0,4,20);
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
  red=xget('lastpattern')+7;
  plot2d(xo(1,:),xo(2,:),mode='arrow',line_color=red,line_thickness=2)
  xo = ode([4;3.5],0,t,f);
  blue=xget('lastpattern')+4;
  plot2d(xo(1,:),xo(2,:),line_color=blue,line_thickness=2)
endfunction

function demo_2d_11()
// scatter plot
  xsetech(wrect=[0,0,1,1/5],frect=[0,0,1,1],axesflag=0)
  xstringb(0,0.0,'Nuages de points',1,1);

  h = (1-1/5);
  xsetech(wrect=[0,1/5,1/2,h]);
  N=1000;
  x=randn(N,1);y=randn(N,1);
  plot2d(x,y,mark=0,line_color=-2,axesflag=5);

  xsetech(wrect=[1/2,1/5,1/2,h],clip=%t);
  x=randn(N,1);y=randn(N,1);
  plot2d(x,y,line_color=-2,mark=8,mark_color=5,mark_size=2,axesflag=5);
  x=randn(N,1)+2;y=randn(N,1);
  plot2d(x,y,line_color=-2,mark=9,mark_color=9,mark_size=2,axesflag=5);
endfunction

// histogram

function demo_2d_12()
  function partie(f,xmin,xmax,couleur)
    x=linspace(xmin,xmax,100);
    y=f(x);
    x=[x,xmax,xmin];y=[y,0,0];
    xfpoly(x',y',couleur);
  endfunction
  histplot([-6:0.4:6],randn(1,2000),rect=[-6,0,6,0.5])
  function [y]=f(x) ; y=exp(-x.*x/2)/sqrt(2*%pi);endfunction
  x=-6:0.1:6;x=x';
  y=f(x);
  plot2d(x,y,style=2,rect=[-6,0,6,0.5])
  titre= 'macro histplot : Histogram plot';
  xtitle(titre,'Classes','N(C)/Nmax');
  xleft=-1;
  partie(f,min(x),xleft,15);
  rect=xstringl(0,0,'A')
  xpoly(xleft*[1;1],[0;0.3]);xstring(xleft-rect(3),0.3,'A');
  xright=2;
  partie(f,2,max(x),12);
  xpoly(xright*[1;1],[0;0.3]);xstring(xright,0.3,'B');
endfunction

function demo_2d_13()
  n=5;p=4;
  val=10*rand(n,p);
  nomsh='nh'+ string(1:p);
  nomsv='nv '+ string(1:n)';

  function mat_show(nomsh,nomsv,val)
    x=1:p+1+1;
    y=1:n+1+1;
    xsetech(frect=[min(x),min(y),max(x),max(y)],axesflag=0,clip=%f);
    xp=[x;x]; yp=[min(y)*ones(size(x));max(y)*ones(size(x))];
    xpolys(xp,yp);
    yp=[y;y]; xp=[min(x)*ones(size(y));max(x)*ones(size(y))];
    xpolys(xp,yp);
    S= [ '', nomsh; [ nomsv, string(val)]];
    for i=1:n+1
      for j=1:p+1
	xstringb(x(j)+0.1,y(n+1-i+1)+0.1,S(i,j),0.8,0.8,'fill');
      end
    end
  endfunction
  mat_show(nomsh,nomsv,val);
endfunction

function demo_3d_1()
  xsetech(wrect=[0,0,0.5,0.5],a3d=%t);
  t=0:0.1:5*%pi;param3d(sin(t)',cos(t)',t'*0.1,flag=[2,1],style=2);
  xtitle("param3d : parametric curves in R3"," "," ");
  //
  xsetech(wrect=[0.5,0,0.5,0.5],a3d=%t);
  t=-50*%pi:0.1:50*%pi;
  x=t.*sin(t);y=t.*cos(t);z=t.*abs(t)./(50*%pi);
  param3d(x',y',z',alpha=45,theta=60,flag=[2,1],style=4);
  title=["param3d : parametric curves in R3 style=4"];
  //
  xsetech(wrect=[0,0.5,0.5,0.5],a3d=%t);
  t=-50*%pi:0.5:50*%pi;
  x=t.*sin(t);y=t.*cos(t);z=t.*abs(t)./(50*%pi);
  param3d(x',y',z',alpha=45,theta=60,flag=[2,1],style=-4);
  F=get_current_figure[];A=F(1);C=A(1);C.color=5;
  x=max(t)*sin(t);y=max(t)*cos(t);z=max(t)*ones(size(t));
  param3d(x',y',z',alpha=45,theta=60,flag=[2,1],style=2);
  x=min(t)*sin(t);y=min(t)*cos(t);z=min(t)*ones(size(t));
  param3d(x',y',z',alpha=45,theta=60,flag=[2,1],style=2,alpha=63,theta=60);
  title=["param3d : parametric curves in R3 style=-4"];
endfunction

function demo_3d_2()
  xsetech(wrect=[0,0,0.5,1.0],a3d=%t);
  t=(-1:0.1:1)*%pi;plot3d(t,t,sin(t)'*cos(t),alpha=80,theta=70);
  title=["plot3d : z=sin(x)*cos(y)"];
  xtitle(title," "," ");
  xsetech(wrect=[0.5,0.0,0.5,1.0],a3d=%t);
  cmap=hotcolormap(45);
  t=(-1:0.1:1)*%pi;plot3d1(t,t,sin(t)'*cos(t),...
			   alpha=80,theta=70,colormap=cmap);
  title=["plot3d1 : z=sin(x)*cos(y)"];
  xtitle(title," "," ");
endfunction

function demo_3d_3()
  function z=f(x,y); z= sin(exp(2*(x+0.2)))*cos(y);endfunction
  xset('colormap',hotcolormap(45));
  t=(-1:0.1:1);plot3d1(t,t,f,theta=-65,alpha=75)
  title=["plot3d1(.,.,f,..) with a function f"];
  xtitle(title," "," ");
endfunction

function demo_3d_4()
  t=linspace(-%pi,%pi,20);
  xsetech(wrect=[0,0,0.5,0.5],a3d=%t)
  xtitle("graycolormap"," ", " ");
  colormap= graycolormap(45);
  plot3d1(t,t,sin(t)'*cos(t),colormap=colormap);
  xsetech(wrect=[0.5,0,0.5,0.5],a3d=%t)
  xtitle("hotcolormap"," "," ");
  colormap=hotcolormap(45);
  plot3d1(t,t,sin(t)'*cos(t),colormap=colormap);
  xsetech(wrect=[0,0.5,0.5,0.5],a3d=%t)
  xtitle("jetcolormap"," "," ");
  colormap=jetcolormap(45);
  plot3d1(t,t,sin(t)'*cos(t),colormap=colormap);
  xsetech(wrect=[0.5,0.5,0.5,0.5],a3d=%t)
  xtitle("greencolormap"," "," ");
  colormap=greencolormap(34);
  plot3d1(t,t,sin(t)'*cos(t),colormap=colormap);
endfunction

function demo_3d_5()
//xset('colormap',hotcolormap(40));
  xsetech(wrect=[0,0,0.5,0.5],a3d=%t)
  xtitle("Interpolated shading colors=[1,5,3]","","");
  // One facet with interpolated shading using colors Id
  plot3d([0,0,1]',[0,1,0]',[3,1,2]',colors=[1,5,3]',flag=[1,1,3]);
  xsetech(wrect=[0.5,0,0.5,0.5],a3d=%t)
  xtitle("Interpolated shading colors=[1,10,3]","","");
  // The number of sub-polygons depends on the distance in Id
  // between colors
  plot3d([0,0,1]',[0,1,0]',[3,1,2]',colors=[1,10,3]',flag=[1,1,3])
  xsetech(wrect=[0,0.5,0.5,0.5],a3d=%t)
  xtitle("mesh only","","");
  // colors are set to zero : only draw polygons
  plot3d([0,0,1]',[0,1,0]',[3,1,2]',colors=[1,10,3]',mesh_only=%t,flag=[1,1,3])
  xsetech(wrect=[0.5,0.5,0.5,0.5],a3d=%t)
  xtitle("no mesh","","");
  // colors are negative: only painting no contour drawing
  plot3d([0,0,1]',[0,1,0]',[3,1,2]',colors=[1,10,3]',mesh=%f,flag=[1,1,3])
endfunction

function demo_3d_6()
  t=[(0:0.2:2)*%pi]'; z=sin(t)*cos(t');
  xset('colormap',hotcolormap(40));
  // remapping zvalues to colors
  zzc = 39*(z-min(z))/(max(z)- min(z))+1;
  [xx,yy,zzcolors]=genfac3d(t,t,zzc);
  [xx,yy,zz]=genfac3d(t,t,z);
  xsetech(wrect=[0,0,0.5,1.0],a3d=%t)
  xtitle("plot3d1"," "," ");
  plot3d1(t,t,z,alpha=45,theta=60);
  xsetech(wrect=[0.5,0,0.5,1],a3d=%t)
  xtitle("plot3d1 with facets"," "," ");
  plot3d(xx,yy,zz,colors=zzcolors,alpha=45,theta=60);
endfunction

function demo_3d_7()
// a demo by Quentin Quadrat.
  demo_tree(1,5);
endfunction

function demo_3d_8()
  u = %pi/2*(-1:0.2:1);
  v = %pi/2*(-1:0.2:1);
  n = size(u,'*');
  x= cos(u)'*exp(cos(v));
  y= cos(u)'*sin(v);
  z= sin(u)'*ones(size(v));
  col=ones(size(u))'*cos(v);
  col=(n-1)*(col-min(col))/(max(col)-min(col))+1;
  xset('colormap',hotcolormap(n));
  [xx,yy,zz]=nf3d(x,y,z);
  [xx,yy,zzcol]=nf3d(x,y,col);
  xx=[xx,-xx];yy=[yy,-yy];zz=[zz,zz];zzcol=[zzcol,zzcol];
  plot3d(xx,yy,zz,colors=zzcol,alpha=55,theta=110,flag=[3,2,0]);
endfunction


function demo_3d_9()
  S=[1,-1,-1; 1,1,-1; -1,1,-1; -1,-1,-1;
     1,1,1;-1,1,1; 1,-1,1; -1,-1,1];

  function F=faces(i)
    F=[S(4,i),S(3,i),S(2,i),S(1,i)
       S(2,i),S(3,i),S(6,i),S(5,i)
       S(4,i),S(8,i),S(6,i),S(3,i)
       S(1,i),S(7,i),S(8,i),S(4,i)
       S(1,i),S(2,i),S(5,i),S(7,i)
       S(5,i),S(6,i),S(8,i),S(7,i)];
    // reverse the orientation
    F=F(:,4:-1:1);
  endfunction

  Fx=faces(1); Fy=faces(2); Fz=faces(3);

  Fx(2:$-1,:) = Fx(2:$-1,:)*1.2;
  Fy(2:$-1,:) = Fy(2:$-1,:)*1.2;
  Fz(2:$-1,:) = Fz(2:$-1,:)*1.2;
  Fz(1,:) = Fz(1,:)-0.4;
  Fz(6,:) = Fz(6,:)+0.4;

  colors=[1,2,3,5,6,9];
  plot3d(Fx',Fy',Fz',colors=colors);
  //colshade=[colors;colors+1;colors+2;colors+1];
  //plot3d(Fx',Fy',Fz',colors=colshade);
endfunction


function demo_3d_10()
// following a parametric 3d curve
  T=6;
  t=linspace(0,T,40);
  ptc=[sin(t);cos(t);t];

  xpol=[];ypol=[];zpol=[];
  for i=1:(size(ptc,'c')-1)
    pt=ptc(:,i);
    ptn=ptc(:,i+1);
    u= ptn-pt;
    u= u / sqrt(sum(u.*u))
    // find an orthogonal basis (u,v,w)
    I=find(u==0.0);
    if ~isempty(I) then
      v=0*u;v(I(1))=1;
    else
      v=[u(2);-u(1);0];
      v= v / sqrt(sum(v.*v))
    end
    w=[u(2)*v(3)-u(3)*v(2);
       -(u(1)*v(3)-u(3)*v(1))
       u(1)*v(2)-u(2)*v(1)];
    // the tube around vector [pt,ptn]
    n=10;
    alpha=2*%pi*(0:n)/n;
    r=0.3;
    pts=r*v*cos(alpha)+r*w*sin(alpha);
    if i==1 then
      ptg=pts+pt*ones(size(alpha));
    else
      ptg=ptd;
    end
    ptd=pts+ptn*ones(size(alpha));
    xpol=[xpol,[ptg(1,1:$-1);ptd(1,1:$-1);ptd(1,2:$);ptg(1,2:$)]];
    ypol=[ypol,[ptg(2,1:$-1);ptd(2,1:$-1);ptd(2,2:$);ptg(2,2:$)]];
    zpol=[zpol,[ptg(3,1:$-1);ptd(3,1:$-1);ptd(3,2:$);ptg(3,2:$)]];
  end
  plot3d1(xpol,ypol,zpol);
endfunction

function demo_3d_11()
  t=4*%pi*(0:20)/20;
  ptc=[t.*sin(t);t.*cos(t);0*ones(size(t))];
  xpol=[];ypol=[];zpol=[];
  for i=1:(size(ptc,'c')-1)
    pt=ptc(:,i);
    ptn=ptc(:,i+1);
    u= ptn-pt;
    u= u / sqrt(sum(u.*u))
    // trouver un vecteur ds le plan orthogonal
    I=find(u==0.0);
    if ~isempty(I) then
      v=0*u;v(I(1))=1;
    else
      v=[u(2);-u(1);0];
      v= v / sqrt(sum(v.*v))
    end
    w=[u(2)*v(3)-u(3)*v(2);
       -(u(1)*v(3)-u(3)*v(1))
       u(1)*v(2)-u(2)*v(1)];

    n=10;
    alpha=2*%pi*(0:n)/n;
    r=t(i);

    pts=r*v*cos(alpha)+r*w*sin(alpha);
    if i==1 then
      ptg=pts+pt*ones(size(alpha));
    else
      ptg=ptd;
    end
    ptd=pts+ptn*ones(size(alpha));

    xpol=[xpol,[ptg(1,1:$-1);ptd(1,1:$-1);ptd(1,2:$);ptg(1,2:$)]];
    ypol=[ypol,[ptg(2,1:$-1);ptd(2,1:$-1);ptd(2,2:$);ptg(2,2:$)]];
    zpol=[zpol,[ptg(3,1:$-1);ptd(3,1:$-1);ptd(3,2:$);ptg(3,2:$)]];
  end
  plot3d1(xpol,ypol,zpol);
endfunction

function demo_3d_12()
  t=%pi*(-10:10)./10;
  box=[-%pi,%pi,-%pi,%pi,-5,1];
  z=sin(t)'*cos(t);
  xset('colormap',jetcolormap(64));
  contour(t,t,z,4,alpha=35,theta=45,flag=[1,1,0],ebox=box,zlevel=-5);
  plot3d1(t,t,z,alpha=35,theta=45,flag=[2,1,1],ebox=box);
  title=["plot3d and contour "];
  xtitle(title," "," ");
endfunction

// demo_3d_16: Copyright CECILL (from scilab).

function demo_3d_13()
  subplot(2,2,1,a3d=%t);
  function hole3d()
  // Holes in surfaces using %inf
    t=linspace(-%pi,%pi,40);z=sin(t)'*cos(t);
    z1=find(abs(z) > 0.5);
    z(z1)=%inf*z1;
    plot3d1(t,t,z,alpha=78,theta=34,colormap=jetcolormap(32));
  endfunction

  hole3d();

  subplot(2,2,2,a3d=%t);
  function rings()
    rr=0.2;
    t=linspace(0,2*%pi,10);
    s=linspace(0,2*%pi,41); n=length(s);
    r=repmat(1+cos(t)*rr,n,1)'; m=length(t);
    x=repmat(cos(s),m,1).*r; y=repmat(sin(s),m,1).*r;
    z=repmat(sin(t)*rr,n,1)';
    // ring1
    [xx1,yy1,zz1]=nf3d(x,y,z);
    // ring2
    [xx2,yy2,zz2]=nf3d(x+1.3,-z,y);
    // ring3
    [xx3,yy3,zz3]=nf3d(x-1.3,-z,y);
    [m,n]=size(xx1);
    colors=[10*ones(1,n),20*ones(1,n),30*ones(1,n)];
    plot3d([xx1,xx2,xx3],[yy1,yy2,yy3],[zz1,zz2,zz3],colors=colors,colormap=jetcolormap(32));
  endfunction
  rings();
  subplot(2,2,3,a3d=%t);
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
    plot3d1(xx,yy,zz,colormap=jetcolormap(32));
  endfunction

  torus();
  subplot(2,2,4,a3d=%t);

  function bh(nn)
  // a black hole
    x=linspace(0,2*%pi,nn);
    t=linspace(0,1,20);
    cosphi=repmat(cos(x),length(t),1);
    sinphi=repmat(sin(x),length(t),1);
    f=repmat((t.*t+0.2)',1,length(x));
    x=f.*cosphi;y=f.*sinphi ;z=repmat(t'.*2-1,length(x),1);
    [xx,yy,zz]=nf3d(x,y,z);
    plot3d1(xx,yy,zz,alpha=70,theta=64,colormap=jetcolormap(32));
  endfunction

  bh(30);
endfunction


function demo_3d_14()
  function [z]=surf(x,y); z=sin(x)*cos(y);endfunction;
  t=%pi*(-10:10)./10;
  plot3d(t,t,surf,alpha=35,theta=45,flag=[-6,2,3]);
  xinfo("Adding 2d graphics on 3d graphic");
  x=%pi/2*sin(t);y=%pi/2*cos(t);
  z=sin(x).*cos(y);
  [x1,y1]=geom3d(x,y,z);
  xpoly(x1,y1,type="lines");
  [x1,y1]=geom3d([0,0],[%pi/2,0],[5,0]);
  xsegs(x1,y1);
  xstring(x1(1),y1(1)," The point (0,0,0)");
  title=["plot3d and use of xgeom "];
  xtitle(title," "," ");
endfunction

function demo_3d_18()
// this one should be used with opengl
  t=%pi*(-10:10)./10;
  rect=[-%pi,%pi,-%pi,%pi,-1,1];
  z=sin(t)'*cos(t);
  plot3d(t,t,z,alpha=35,theta=45,flag=[-3,1,3],ebox=rect);
  contour(t,t,z+0.01,10,alpha=35,theta=45,flag=[0,1,0],ebox=rect);
  title=["plot3d and contour "];
  xtitle(title," "," ");
endfunction

function demo_prim_1()
  function [v]=transl(x,t); v=x+t*ones(size(x)); endfunction
  xsetech(frect=[-100,-100,500,600]);
  // xrects
  x=0:40:240;
  boxes=[x;10*ones(size(x));30*ones(size(x));30*ones(size(x))];
  xstring(-50,-5,'xrects');
  xrects(boxes);
  // xrects
  boxes=[x;45*ones(size(x));30*ones(size(x));30*ones(size(x))];
  pats=[0,4,8,12,15,xget("white"),0];
  xstring(-50,20,'xrects');
  xrects(boxes,pats);
  // xarcs
  boxes=[x;90*ones(size(x));30*ones(size(x));30*ones(size(x))];
  arcs=[boxes; 0*ones(size(x));64*180*ones(size(x))];
  pats=[0,4,8,12,15,xget("white"),0];
  xstring(-50,75,'xarcs');
  xarcs(arcs,pats);
  // xarcs
  boxes=[x;135*ones(size(x));30*ones(size(x));30*ones(size(x))];
  arcs=[boxes; 0*ones(size(x));64*360*ones(size(x))];
  xarcs(arcs);
  // xfpolys
  x1=[0,10,20,30,20,10,0];
  y1=[15,30,30,15,0,0,15];y1=160*ones(size(y1))+y1;
  xpols=[x1;transl(x1,40);transl(x1,80);transl(x1,120);
	 transl(x1,160);transl(x1,200);transl(x1,240)];
  ypols=[y1;y1;y1;y1;y1;y1;y1];
  xfpolys(xpols',ypols');
  // xfpolys
  ypols=transl(ypols,60);
  pats=[0,4,8,12,15,xget("white"),0];
  xfpolys(xpols',ypols',pats);
  //
  ypols=transl(ypols,120);
  xpolys(xpols',ypols',1:7);
  //
  ypols=transl(ypols,180);
  xpolys(xpols',ypols',-(1:7));
  //
  xsegs([x;x+30*ones(size(x))],[(360+40)*ones(size(x));(360+70)*ones(size(x))]);
  xinfo("[I.5] xsegs(x,y)");
  //
  xarrows([x;x+30*ones(size(x))],[(360+70)*ones(size(x));(360+100)*ones(size(x))]);
  xinfo(["[I.6] xarrows(x,y)"]);
  //
  x=0:100:200;
  xnumb(x,500*ones(size(x)),[10,20,35],1);
  xnumb(x,550*ones(size(x)),[10,20,35],0);
  xinfo(["[[II.3] xnumb()"]);
endfunction

function demo_prim_2()
  function [v]=transl(x,t); v=x+t*ones(size(x)); endfunction
  xsetech(frect=[-100,-100,500,600]);
  xrect(20,120,60,60)
  xfrect(100,120,60,60)
  xarc(20,200,50,70,0,64*(225))
  xfarc(100,200,50,70,0,64*225)
  x=0:1:%pi;
  [n1,n2]=size(x);
  x1=50*sin(x)+40*ones(size(x));
  y1=50*cos(x)+90*ones(size(x));
  y1=transl(y1,200);
  xpoly(x1,y1,mark=6);
  xset("mark",4,6);
  x1=transl(x1,80);
  xpoly(x1,y1,mark=4);
  x1=transl(x1,160);
  xpoly(x1,y1,close=%t);
  x1=transl(x1,240);
  xpoly(x1,y1,close=%f);
  x1=transl(x1,320);
  xfpoly(x1,y1,1);
  xset("font",2,2);
  xstring(20,400,"Character string",0,0);;
  xstring(200,400,"Character string",0,1);;
  xstring(400,400,"Character string ",45,0);
  rect=xstringl(20,400,"Character string");
  xrect(rect(1),rect(2),rect(3),rect(4));
  xrect(150,460,100,150);
  // to be done with new graphics
  if %t then
    xset('clipping',150,460,100,150);
    x=0:0.2:2*%pi;
    x1=[sin(x);10*sin(x)];
    y1=[cos(x);10*cos(x)];
    y1=transl(y1,20);
    xsegs(10*x1+200*ones(size(x1)),10*y1+200*ones(size(y1)));
    xset("clipgrf");
  end
  xset("font",2,0);
  xstring(0,130,"Scilab");
  xset("font",2,1);
  xstring(0,150,"Scilab");
  xset("font",2,2);
  xstring(0,170,"Scilab");
  xset("font",2,3);
  xstring(0,200,"Scilab");
  xset("font",2,4);
  xstring(0,230,"Scilab");
  xset("font",2,5);
  xstring(0,270,"Scilab");
  xset("font",3,5);
  xstring(0,310,"Scilab");
  xset("font",4,5);
  xstring(0,350,"Scilab");
  xinfo(["[IV.5] Setting font style and size"]);
endfunction

function demo_prim_3()
  xsetech(wrect=[0,0,0.5,0.5],frect=[-5,-5,5,5]);
  n=4;
  rect=[-n,-n,n,n];
  x=[-1 1 1 -1 -1]';y=[-1 -1 1 1 -1]';
  xx=x*(n-1);yy=y*(n-1);
  for k=2:n;  xx=[xx,x*(n-k)]; yy=[yy,y*(n-k)];end;
  c=0:(n-1);
  xfpolys(xx,yy,c);
  //
  xsetech(wrect=[0,0.5,0.5,0.5],frect=[0,0,100,100]);
  x=[0 25 25  0 0]';y=[0 0 25 25 0]';d=25*[1 1 1 1 1]';
  xx=[];yy=[];i=0;
  for k=1:4; for l=1:4 ; i=i+1; xx=[xx,(l-1)*d+x];yy=[yy,y+(k-1)*d]; end;end
  xfpolys(xx,yy,(1:16));
  //
  xsetech(wrect=[0.5,0,0.5,0.5],frect=[0,0,30,10]);
  x=[1 3 3 1 1]';
  y=[0 0 1 1 0]';
  d=4*[1 1 1 1 1]';
  xx=[x,x+d,x+2*d,x+3*d,x+4*d,x+5*d];
  yy=[y,5*y,2*y,10*y,8*y,6*y];
  xfpolys(xx,yy,2*[1 2 3 4 5 6]);
  //
  xsetech(wrect=[0.5,0.5,0.5,0.5],frect=[-1.5,-1.5 ,1.5,1.5]);
  alls=[-1,1,2,2,0,64*90;
	-1,1,2,2,64*90,64*(30);
	-1,1,2,2,64*(120),64*(70);
	-1,1,2,2,64*(190),64*(360-190)];
  xfarcs(alls',[1,3,5,7]);
endfunction

function demo_prim_4()
  xset('font size',3);
  x=0:0.1:2*%pi;plot2d([x]',[sin(x)]',leg='sin(x)',strf='132')
  for A=[0:30:330];xstring(4.5+0.5*cos(-%pi*A/180),0.5+0.5*sin(-%pi*A/180),'Nsp',A,0);end
  for A=[0:30:330];xsegs([4.5;4.5+0.5*cos(-%pi*A/180)],[0.5;0.5+0.5*sin(-%pi*A/180)]);end
  xtitle('nsp plot2d','Time','Sin(t)')
endfunction

// realtime clock demo

function demo_anim_1()
  xsetech(frect=[-1,-1,1,1]*1.5,fixed=%t,clip=%t,iso=%t,axesflag=0);
  xset('thickness',2);
  xarc(-1,1,2,2,0,360*64,color=3,thickness=2);
  x1=[0;cos(90*%pi/180)];
  y1=[0;sin(90*%pi/180)];
  A=xarrows(x1,y1,arsize=0.2,style=1);
  S=xstring(-0.5,1.2," ",fill=%t,w=1,h=0.25);
  P=xpoly([],[],color=5);
  F=get_current_figure[];
  p=5;
  realtimeinit(1/p);//sets time unit to 1/p
  realtime(0);//sets current date to 0
  N=60;
  for k=linspace(1,N,5*N);
    realtime(k);
    A.x(2)= cos((90-(p*k*360/60))*%pi/180)
    A.y(2)= sin((90-(p*k*360/60))*%pi/180)
    S.text=sprintf("T=%5.2f s.",k/p);
    rk=k/N;
    P.x=[P.x,rk*A.x(2)];
    P.y=[P.y,rk*A.y(2)];
    F.invalidate[];
    F.process_updates[];
    xpause(0,%t); // gtk events are managed here
  end
endfunction

// a Cycloid

function demo_anim_2()
// the main circle
  centeri=[0;1];r=1;
  C=[centeri(1)-r;centeri(2)+r;2*r;2*r;0;360*64];
  xsetech(frect=[-1,-0.5,8,2.5],iso=%t);
  Circ=xarc(C);
  // a point attached to the main circle
  thetai=-%pi/2;
  center1=centeri + [r*cos(thetai);r*sin(thetai)];
  r1=r/15;
  C1=[center1(1)-r1;center1(2)+r1;2*r1;2*r1;0;360*64];
  Circ1=xfarc(C1,color=5);
  // keep track of the attached point
  P=xpoly([],[],color=2);
  if ~exists('anim') then anim=%t;end
  Npt=100;
  // successive positions of the x value of the center
  xcenters=linspace(0,2*%pi*r,Npt);

  for i=1:size(xcenters,'*');
    center = centeri + [xcenters(i);0];
    // the new position of the red circle;
    theta  = thetai - (xcenters(i)/r);
    C=[center(1)-r;center(2)+r;2*r;2*r;0;360*64];
    center1= center + [r*cos(theta);r*sin(theta)];
    C1=[center1(1)-r1;center1(2)+r1;2*r1;2*r1;0;360*64];
    P.x=[P.x,center1(1)];
    P.y=[P.y,center1(2)];
    P.invalidate[]; Circ.invalidate[]; Circ1.invalidate[];
    Circ.x=C(1);Circ.y=C(2);Circ.w=C(3);Circ.h=C(4);
    Circ1.x=C1(1);Circ1.y=C1(2);Circ1.w=C1(3);Circ1.h=C1(4);
    Circ.invalidate[]; Circ1.invalidate[];
    xpause(100000,%t);
  end
endfunction

function demo_anim_3()
// HYPOTROCHOÏDE
  xsetech(frect=[-6,-6,6,6],iso=%t,axesflag=2);
  // the bigest circle
  R=5; xarc([-R;+R;2*R;2*R;0;360*64])
  // the inside circle
  centeri=[0;1];r=1;
  c=[centeri(1)-r;centeri(2)+r;2*r;2*r;0;360*64];
  C0=xarc(c,color=1);
  // points attached to the main circle
  thetai=-%pi/2;
  r1=r/10;
  rn=[r,r/2,2*r,-2*r];
  n=size(rn,'*');
  C=cell(1,n);P=cell(1,n);
  for k=1:n
    center1=centeri + rn(k)*[cos(thetai);sin(thetai)];
    c=[center1(1)-r1;center1(2)+r1;2*r1;2*r1;0;360*64];
    C{k}=xfarc(c,color=k+1,background=k+1);
    P{k}=xpoly([],[],color=k+1);
  end
  Pc1=xpoly([],[],color=0);
  Pc2=xpoly([],[],color=0);
  Npt=200;
  L=linspace(0,10*%pi*r,Npt);

  for i=1:size(L,'*');
    beta= L(i)/R;
    center = [(R-r)*cos(thetai+beta);(R-r)*sin(thetai+beta)];
    // the new position of the red circle;
    theta  = thetai - (L(i)/r);
    c=[center(1)-r;center(2)+r;2*r;2*r;0;360*64];
    C0.invalidate[];
    C0.x=c(1);C0.y=c(2);C0.w=c(3);C0.h=c(4);
    C0.invalidate[];
    Pc1.invalidate[];Pc2.invalidate[];
    Pc1.x = center(1)+2*r*[cos(theta),cos(theta+%pi)];
    Pc1.y = center(2)+2*r*[sin(theta),sin(theta+%pi)];
    Pc2.x = center(1)+r*[cos(theta+%pi/2),cos(theta+3*%pi/2)];
    Pc2.y = center(2)+r*[sin(theta+%pi/2),sin(theta+3*%pi/2)];
    Pc1.invalidate[];Pc2.invalidate[];
    //
    for k=1:n
      center1= center + rn(k)*[cos(theta);sin(theta)];
      P{k}.x=[P{k}.x,center1(1)];P{k}.y=[P{k}.y,center1(2)];
      c=[center1(1)-r1;center1(2)+r1;2*r1;2*r1;0;360*64];
      C{k}.invalidate[];
      C{k}.x=c(1);C{k}.y=c(2);C{k}.w=c(3);C{k}.h=c(4);
      C{k}.invalidate[];
    end
    xpause(100000,%t);
  end
endfunction

// hypothrocoid variation

function demo_anim_4()
//
  xsetech(frect=[-6,-6,6,6],iso=%t,axesflag=2);
  // the main circle
  centeri=[0;1];r=1;
  c=[centeri(1)-r;centeri(2)+r;2*r;2*r;0;360*64];
  C0=xarc(c,color=1);
  // points attached to the main circle
  thetai=-%pi/2;
  r1=r/10;
  center1=centeri + 2*r*[cos(thetai);sin(thetai)];
  c=[center1(1)-r1;center1(2)+r1;2*r1;2*r1;0;360*64];
  C=xfarc(c,color=1,background=1);
  P=xpoly([],[],color=5); // keep track of the created curve
  Pc1=xpoly([],[],color=0); // diameters of the running circle
  Pc2=xpoly([],[],color=0);
  Npt=200;
  // the bigest circle
  R=5; xarc([-R;+R;2*R;2*R;0;360*64])
  L=linspace(0,10*%pi*r,Npt);

  for i=1:size(L,'*');
    ri=2*r*sin(L(i)).^2;
    beta= L(i)/R;
    center = [(R-r)*cos(thetai+beta);(R-r)*sin(thetai+beta)];
    // the new position of the red circle;
    theta  = thetai - (L(i)/r);
    c=[center(1)-r;center(2)+r;2*r;2*r;0;360*64];
    C0.invalidate[];
    C0.x=c(1);C0.y=c(2);C0.w=c(3);C0.h=c(4);
    C0.invalidate[];
    Pc1.invalidate[];Pc2.invalidate[];
    Pc1.x = center(1)+ri*[cos(theta),cos(theta+%pi)];
    Pc1.y = center(2)+ri*[sin(theta),sin(theta+%pi)];
    Pc2.x = center(1)+r*[cos(theta+%pi/2),cos(theta+3*%pi/2)];
    Pc2.y = center(2)+r*[sin(theta+%pi/2),sin(theta+3*%pi/2)];
    Pc1.invalidate[];Pc2.invalidate[];
    center1= center + ri*[cos(theta);sin(theta)];
    P.x=[P.x,center1(1)];P.y=[P.y,center1(2)];
    c=[center1(1)-r1;center1(2)+r1;2*r1;2*r1;0;360*64];
    C.invalidate[];
    C.x=c(1);C.y=c(2);C.w=c(3);C.h=c(4);
    C.invalidate[];
    xpause(100000,%t);
  end
endfunction

function demo_anim_5()
  a=ones_new(60,60);
  plot2d([0,10],[0,10],style = 0);
  F=get_current_figure();
  i=-60;
  b=3*tril(a,i)+2*triu(a,i+1);
  M= Matplot1(b,[4,4,9,9]);
  M1= Matplot1(b,[1,1,3,3]);
  xset('wshow'); // force expose_events
  for i=-60:60 do
    b=3*tril(a,i)+2*triu(a,i+1);
    M.data = b;
    M.invalidate[];
    M.translate[-[0,0.02]];
    b=3*triu(a,i)+2*tril(a,i+1);
    M1.data = b;
    M1.translate[+[0,0.02]];
    M1.invalidate[];
    xpause(10000,events=%t);
  end
endfunction

function demo_anim_6()
  t=%pi*(-1:0.1:1);
  I=20:-1:1;
  ebox=[min(t),max(t),min(t),max(t),-1,1];
  i=1;
  xset('colormap',jetcolormap(32));
  plot3d1(t,t,sin((I(i)/10)*t)'*cos((I(i)/10)*t),alpha=80,theta=62,flag=[2,1,0],ebox=ebox)
  F=get_current_figure();
  A=F(1);
  for i=1:size(I,'*')
    F.children.remove_first[];
    plot3d1(t,t,sin((I(i)/10)*t)'*cos((I(i)/10)*t),alpha=80,theta=62,flag=[2,1,0],ebox=ebox)
    A.invalidate[]; // signal that Axis should be redrawn.
    // F.draw_now[]; // will activate a process_updates
    xpause(100000,%t)// slow down animation
  end
endfunction


function demo_anim_7()
  t=%pi*(-5:5)/5;
  xset('colormap',jetcolormap(32));
  P=plot3d1(t,t,sin(t)'*cos(t),alpha = 35,theta = 45);
  st=5;
  F=get_current_figure();
  A=F(1);
  P=A(1);
  P.shade = %t;
  for i=35 : st : 80 do
    A.theta = 2*i;
    A.alpha = i;
    A.invalidate[]; // signal that Axis should be redrawn.
    xpause(100000,%t)// slow down animation
  end
endfunction

function demo_anim_8()
  x=%pi*(-1:0.1:1);y=x;
  M=x'*y;
  i=1;
  // xset('thickness',2.0);
  xset('colormap',jetcolormap(64));
  C=champ1(x,y,sin((i/10)+x'*y),cos((i/10)+x'*y));
  F=get_current_figure();
  for i=1:100
    C.fx = sin((i/10)+M).*M;
    C.fy = cos((i/10)+M).*M;
    C.invalidate[];
    // F.process_updates[];
    xpause(10000,events=%t);
  end
endfunction

function demo_anim_9()
  np=10;
  t=(0:0.1:np)'*%pi;
  i=1;
  param3d((t/(np*%pi)*%pi).*sin(t),(t/(np*%pi)*%pi).*cos(t),...
	  i*t/(np*%pi),alpha=35,theta=45,flag=[2,4]);
  F=get_current_figure();
  A=F(1);
  P=A(1);
  P.Mcolor= 2;
  for i=1:1:30
    P.Mcoord(:,3) = i*t/(np*%pi)
    P.Mcolor= 2;
    P.invalidate[];
    A.invalidate[];
    // F.draw_now[]; // will activate a process_updates
    xpause(100000,%t)// slow down animation
  end
endfunction

function demo_anim_10()
  t=-%pi:0.3:%pi;
  i=35;
  contour(t,t,sin(t)'*cos(t),10,alpha=i,theta=45,flag=[0,2,4])
  F=get_current_figure();
  A=F(1);
  A.box_style = 1;
  for i=35:80
    A.theta = 45;  A.alpha = i;
    A.invalidate[]; // signal that Axis should be redrawn.
    F.draw_now[]; // will activate a process_updates
    xpause(10000,%t)// slow down animation
  end
  for i=45:80,
    A.theta = i;  A.alpha = 80;
    A.invalidate[]; // signal that Axis should be redrawn.
    F.draw_now[]; // will activate a process_updates
    xpause(10000,%t)// slow down animation
  end
endfunction

function demo_anim_11()
// Demo of Hinton diagrams.
// Initial idea from matplotlib

  xsetech(axesflag=2);
  xset('colormap',graycolormap(8));
  n=20;
  xfrect([0,n+1,n+1,n+1],color=11);
  x=1:n; y=1:n;
  [X,Y]=ndgrid(x,y);
  X.redim[1,-1];Y.redim[1,-1];

  F=get_current_figure[];
  Axe=F(1);
  B=randn(n,n);
  A= tril(ones(n,n))+triu(-ones(n,n));
  N=100;
  alpha=linspace(0,1,N);
  for i=[1:N,N:-1:1]
    M=alpha(i)*A+(1-alpha(i))*B;
    C=-sign(M);
    w= abs(M(:))'/max(abs(M));
    rects=[Y-w./2;X(n*n:-1:1)+w./2;w;w];
    if length(Axe.children) >= 2 then Axe.children.remove_last[];end
    xrects(rects,color=4*(1+C(:)'),background=4*(1+C(:))');
    Axe(1).invalidate[];
    xpause(10000,%t);
  end
endfunction

function demo_anim_12()
  xtitle("vector field and trajectories");
  function y=f(t,x)
    y=[0.8*x(1)*(1 - 0.5*x(2));
       0.2*x(2)*(x(1)-3)];
  endfunction
  x=linspace(0,6,20);y=linspace(0,4,20);
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
  xsetech(frect=[0,0,6,4],fixed=%t);
  champ1(x,y,fx,fy);

  t = linspace(0,20,100);
  N=6;
  x=cell(1,N);P=cell(1,N);
  for i=1:N
    x{i} = ode([4;2+i/3],0,t,f);
    P{i} = xarc([x{i}(1),x{i}(2),0.1,0.1,0,360*64],background=5);
  end
  for j=1:size(t,'*')
    for i=1:N
      P{i}.invalidate[];P{i}.x = x{i}(1,j); P{i}.y = x{i}(2,j);P{i}.invalidate[];
      xpause(10000,%t);
    end
  end
endfunction

function demo_anim_13()

  function [ydot]=lorenz(t,y)
    a=[-10,10,0;
       28,-1,-y(1);
       0,y(1),-8/3];
    ydot=a*y;
  endfunction

  function [j]=Jlorenz(t,y)
    x=y(1);yy=y(2);z=y(3);
    j=[-10,10,0;28-z,-1,-x;-yy,x,-8/3];
  endfunction

  N=10000;
  d=100;
  t=linspace(0,30,N);
  y=ode(1.e-8*[1;1;1],0,t,lorenz);

  C=jetcolormap(128);
  xset('colormap',C($:-1:1,:));
  lpat=xget("lastpattern");
  y1=y(1,:);y1.redim[d,-1];y1($+1,:)=[y1(1,2:$),y1($,$)]
  y2=y(2,:);y2.redim[d,-1];y2($+1,:)=[y2(1,2:$),y2($,$)]
  y3=y(3,:);y3.redim[d,-1];y3($+1,:)=[y3(1,2:$),y3($,$)]
  style=modulo(1:(N/d),lpat-1);
  for i=1:(N/d)
    param3d1(y1(:,i),y2(:,i),y3(:,i),style=style(i),alpha=30, ...
	     theta=67,ebox=[-20,20,-20,20,0,50],flag=[3,1]);
    xpause(100000,%t);
  end
endfunction

function demo_anim_14()

  t=linspace(-%pi,%pi,40);
  m=1-abs(sin(t)'*cos(t));
  [xc,yc]=contour2di(t,t,m,128);
  k=1;n=yc(k);level=xc(k);c=1;
  F=get_current_figure[];
  xsetech(wrect=[0,0,0.5,1],a3d=%t);
  plot3d1(t,t,m,alpha=70,theta=40);

  xsetech(wrect=[0.5,0,0.5,1],frect=%pi*[-1,-1,1,1]);
  xset('colormap',jetcolormap(128));
  t=linspace(-%pi,%pi,40);
  m=1-abs(sin(t)'*cos(t));
  while %t
    A=F(1);A.children = list();
    // make a loop, to plot level curves at level level
    while %t
      n=yc(k);
      plot2d(xc(k+(1:n)),yc(k+(1:n)),line_color=c,line_thickness=2,rect=%pi*[-1,-1,1,1]);
      k=k+n+1;
      if k > size(yc,'*') then break;end
      nlevel=xc(k);
      if abs(nlevel-level) >= 1.e-4 then break;end
    end
    xtitle(sprintf("level=%f",level),"","");
    if k > size(yc,'*') then break;end;
    xpause(100000,%t);
    // start a new level
    level=nlevel;
    c=c+1;
  end
endfunction

function demo_contour_1()
// xset('colormap',hotcolormap(20))
  xset('colormap',jetcolormap(20))
  t=-%pi:0.1:%pi;m=exp(sin(t))'*exp(cos(t));
  xsetech(wrect=[0,0,0.5,0.5])
  xtitle('grayplot default');
  grayplot(t,t,m);
  xsetech(wrect=[0,0.5,0.5,0.5])
  xtitle('grayplot shade=%t, zminmax and colminmax');
  grayplot(t,t,m,shade=%t,colminmax=[5,15],zminmax=[0.3,6.0]);
  xsetech(wrect=[0.5,0,0.5,0.5])
  xtitle('grayplot shade=%t, zminmax and colout=[0,0]');
  grayplot(t,t,m,shade=%t,zminmax=[0.3,6.0],colout=[0,0]);
  xsetech(wrect=[0.5,0.5,0.5,0.5])
  xtitle('grayplot [1,5;5,1] remap=%f, shade=%t');
  grayplot(1:2,1:2,[1,5;5,1],remap=%f,shade=%t);
endfunction

function demo_contour_2()
  xset('colormap',hotcolormap(20))
  N=4;
  // build the nodes
  i=linspace(0,2*%pi,N+1); i($)=[];
  xn=[cos(i)';0];
  yn=[sin(i)';0];
  Nnodes=N+1;
  // values at nodes
  val=ones_new(Nnodes,1);
  val($)=0;
  // build the triangles
  Ntriang=N;
  triangles_nodes=[1:N;[(1:N-1)+1,1];(N+1)*ones_new(1,N)];
  triangles=[(1:N);triangles_nodes;zeros_new(1,N)]';
  rect=[-1.2,-1.2,1.2,1.2];
  xsetech(wrect=[0,0,0.5,0.5])
  xtitle('fec default and mesh=%t');
  fec(xn,yn,triangles,val,strf="032",rect=rect,mesh=%t);
  xsetech(wrect=[0,0.5,0.5,0.5])
  xtitle('fec  zminmax and mesh=%t');
  fec(xn,yn,triangles,val,strf="032",rect=rect,zminmax=[0.4,0.8],mesh=%t);
  xsetech(wrect=[0.5,0,0.5,0.5])
  xtitle('fec  zminmax,colout=[0,0] and mesh=%t');
  fec(xn,yn,triangles,val,strf="032",rect=rect,
  zminmax=[0.4,0.8],colout=[0,0],mesh=%t);
  xsetech(wrect=[0.5,0.5,0.5,0.5])
  xtitle('fec zminmax,colout=[1,1] and mesh=%t');
  fec(xn,yn,triangles,val,strf="032",
  rect=rect,zminmax=[0.4,0.8],colout=[1,1],mesh=%t);
endfunction

function demo_contour_3()
  xset("colormap",hotcolormap(40));
  t=-2:0.1:2;m=sinh(t)'*cosh(t);
  grayplot(t,t,m,shade=%t);
  contour(t,t,m,11);
endfunction

function demo_contour_4()
  t=-2:0.1:2;m=sinh(t)'*cosh(t);
  contour(t,t,m,[-4:0.5:4]);
endfunction

function demo_contour_5()
  function z=f(x,y)
    z =  exp(-x.^2 - (y+2).^2) - 0.5*exp(-x.^2-y.^2) +
    0.3* exp(-(x+2).^2 - y.^2) - 2* exp(-(x+2).^2 - (y+2).^2)
  endfunction

  x=-4:0.1:3;y=x;
  z=eval3d(f,x,y);
  m=30;
  xset('colormap',hotcolormap(m));
  xset('fpf',' ');
  contourf(x,y,z,nv=m,style=0*ones_new(1,m))
  //xset('fpf','');
  //contourf(x,y,z,nv=m);
endfunction

function demo_contour_6()
  function z=f(x,y)
    z =  exp(-x.^2 - (y+2).^2) - 0.5*exp(-x.^2-y.^2) +
    0.3* exp(-(x+2).^2 - y.^2) - 2* exp(-(x+2).^2 - (y+2).^2)
  endfunction
  x=-4:0.1:3;y=x;
  z=eval3d(f,x,y);
  m=30;
  xset('colormap',hotcolormap(m));
  xset('fpf','');
  contourf(x,y,z,nv=m);
endfunction

function demo_prim_new_rects()
  xsetech(frect=[-100,-100,500,600]);// ,fixed=%t);
  F=get_current_figure();
  F.draw_latter[];
  // xrects
  x=0:40:240;
  boxes=[x;10*ones(size(x));30*ones(size(x));30*ones(size(x))];
  // only draw with current color
  xrects(boxes);
  // xrects(rect,colors)
  boxes=[x;45*ones(size(x));30*ones(size(x));30*ones(size(x))];
  pats=[0,4,8,12,15,xget("white"),0];
  xrects(boxes,pats);
  //
  boxes=[x;80*ones(size(x));30*ones(size(x));30*ones(size(x))];
  xrects(boxes,color=1:7,thickness=5*ones(1,7),background=8:8+6);
  F.draw_now[];
endfunction

function demo_prim_new_arcs()
  xsetech(frect=[-100,-100,500,600]);// ,fixed=%t);
  F=get_current_figure();
  F.draw_latter[];
  // xrects
  x=0:40:200;
  // xarcs
  boxes=[x;90*ones(size(x));30*ones(size(x));30*ones(size(x))];
  arcs=[boxes; 0*ones(size(x));64*linspace(0,360,6)];
  pats=[4,9,12,15,xget("white"),0];
  xfarcs(arcs,pats);
  // xarcs
  boxes=[x;130*ones(size(x));30*ones(size(x));30*ones(size(x))];
  arcs=[boxes; 0*ones(size(x));64*linspace(0,360,6)];
  xarcs(arcs);
  boxes=[x;170*ones(size(x));30*ones(size(x));30*ones(size(x))];
  arcs=[boxes; 0*ones(size(x));64*linspace(0,360,6)];
  xarcs(arcs,color=1:6,thickness=4*ones(1,6),background=8:8+5);
  F.draw_now[];
endfunction

function demo_prim_new_xpolys()
  function [v]=transl(x,t); v=x+t*ones(size(x)); endfunction
  xsetech(frect=[-100,-100,500,600]);
  F=get_current_figure();
  F.draw_latter[];
  // xfpolys
  x1=[0,10,20,30,20,10,0];
  y1=[15,30,30,15,0,0,15];y1=160*ones(size(y1))+y1;

  xpols=[x1;transl(x1,40);transl(x1,80);transl(x1,120);
	 transl(x1,160);transl(x1,200);transl(x1,240)];
  ypols=[y1;y1;y1;y1;y1;y1;y1];
  xfpolys(xpols',ypols');
  // xfpolys
  ypols=transl(ypols,60);
  pats=[0,4,8,12,15,xget("white"),0];
  xfpolys(xpols',ypols',pats);
  //
  ypols=transl(ypols,120);
  xpolys(xpols',ypols',1:7);
  //
  ypols=transl(ypols,180);
  xpolys(xpols',ypols',-(1:7));
  F.draw_now[];
endfunction
