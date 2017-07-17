//---------------------------------------------------
// Copyright (C) 2004 Jean-Philippe Chancelier Cermics/Enpc 
// jpc@cermics.enpc.fr 
// NSP  graphic demo 
//--------------------------------------------------- 

exec('NSP/demos3/graphics/main_graphic.sci');

// AXIS  Sets ot gets the axis limits of a 2D or 3D graph
//    --- To get the axis limits, use the function axis :

function mot_demo_1()
  t=linspace(0,2*%pi,50);
  plot(cos(t),sin(2*t));
endfunction 

function mot_demo_2()
  x = linspace(0,2*%pi,64);
  plot(x,cos(x),x,cos(2*x),legend={'The sine','The cosine'});
endfunction

function mot_demo_3()
  subplot(2,2,1);
  function y=f(x); y=x.^2; endfunction;
  x=-1:0.1:1;
  plot(x,f);
  subplot(2,2,2);
  function [x,y]=circle(t);x=cos(t);y=sin(t);endfunction ;
  function [x,y]=lissajous(t);x=cos(t);y=sin(2*t);endfunction ;
  t=linspace(0,2*%pi,63);
  plot(t,circle,t,lissajous,axis='equal');
  subplot(2,2,3);
  t=linspace(0,2*%pi,63); 
  plot(t,cos(t),'r',t,sin(t))        // red line for the cos 
  subplot(2,2,4);
  plot(t,cos(t),'ro',t,sin(t),'y^-') // red dots for the cos and yellow 
  //triangles + solid line for the sin 
  if %f then 
    plot(t,[cos(t);sin(t)])            // color cycling is like in matlab 
    plot(cos(t),sin(t),'axis','equal')   // example of property/value setting 
    plot(cos(t),sin(t),'axis',[0 1 0 1]) //another example the axis vector
    //is 
    plot(t,cos(t),'3r') // red line, set the linewidth to 3. 
  end
endfunction

function mot_demo_4()
//  BAR   Plot vertical bar graph
  subplot(2,2,1);
  bar(rand(5,3)) // ,'legend','intake','exhaust','collapse')
  subplot(2,2,2);
  colormap('blue');
  bar(rand(5,3),'stacked')// ,'legend','intake','exhaust','collapse')
  subplot(2,2,3);
  colormap('jet');
  x = -2.9:0.2:2.9;
  bar(x,exp(-x.*x/2)/sqrt(2*%pi),'r')
endfunction 

function mot_demo_5()
  //  pcolor 
  xset('colormap',jetcolormap(32));
  r=linspace(0,1,25);
  theta=linspace(%pi/4,3*%pi/2,25);
  function [x,y,c]=camenbert(r,t) 
    x=r.*cos(t);y=r.*sin(t);c=t;
  endfunction;
  [R,T]=meshgrid(r,theta);
  pcolor(R.*cos(T),R.*sin(T),T,colorbar='on')
endfunction 

function mot_demo_6()
  n=5;
  angle=2*%pi*(0:n)'/(n+1);
  x= sin(angle); x=[x,x+2,x+3,x+4];
  y= cos(angle); y=[y,y,y,y];
  zi=[ones(n+1,1)];
  z=[zi,0*zi,0.5*zi,0.25*zi];
  
  subplot(2,2,1);
  // rgb 
  fill3(x,y,z,[0.0 0.1,0.45,0.78]);
  title('fill3: one rgb color')

  subplot(2,2,2);
  // flat 
  C=rand(z);
  fill3(x,y,z,C(1,:));
  title('fill3: flat mode ')
  
  subplot(2,2,3);
  // interp 
  fill3(x,y,z,C);
  title('fill3: shaded mode ')
  
  subplot(2,2,4)
  fill3(x,y,z,rand(z),axis={'off',[0,4,-2,2,0,4],'vis3d'},view=[60,35]);
  title('fill3: multiple options ')
endfunction


function mot_demo_7()
// LIGHT property for surfaces with lighting
  function z=f(x,y); z=x.^2-y.^2 ;endfunction;
  x=linspace(-1,1,10);y=x;
  surfl(x,y,f,shading='flat',light=[0 0 1],colormap='jet')
endfunction

function mot_demo_8()
//  MESH   3-D mesh surface.
  subplot(2,2,1,a3d=%t)
  x=linspace(-1,1,20); y=linspace(-2,2,40);
  Z=cos(y'*x);
  mesh(x,y,Z);
  subplot(2,2,2,a3d=%t)
  mesh(x,y,Z,axis='off',facecolor=[1 0 1],edgecolor=[0 0 1]);
  subplot(2,2,3,a3d=%t)
  x=linspace(-1,1,20); y=linspace(-2,2,40);
  function z=f(x,y);z=(x.^2-y.^2)'; endfunction 
  mesh(x,y,f)
  subplot(2,2,4,a3d=%t)
  u=linspace(0,2*%pi,50);
  v=linspace(0,%pi,25);
  function [x,y,z]=sphere(u,v)
    x=sin(v).*cos(u); y=sin(v).*sin(u);z=cos(v);
  endfunction 
  mesh(u,v,sphere,view=[30 30]);
endfunction

function mot_demo_9()
// PCOLOR  Pseudocolor plot.
  xset('colormap',jetcolormap(64));
  subplot(2,2,1);
  x=linspace(-1,1,20); y=linspace(-2,2,40);
  Z=cos(x'*y);
  pcolor(x,y,Z);
  subplot(2,2,2);
  r=linspace(0,1,10);
  theta=%pi/4+linspace(0,3*%pi/2,10);
  [R,T]=meshgrid(r,theta);
  [m,n]=size(T);
  pcolor(R.*cos(T),R.*sin(T),randn(m,n),shading='interp',colorbar='on',axis='equal');
  subplot(2,2,3);
  x=linspace(-1,1,20); y=linspace(-2,2,40);
  function z=f(x,y);z=x.^2-y.^2; endfunction 
  Z=feval(x,y,f);
  pcolor(x,y,Z,shading='flat')
  subplot(2,2,4);
  n=10;
  r=linspace(0,1,n);
  theta=%pi/4+linspace(0,3*%pi/2,n);
  [R,T]=meshgrid(r,theta);
  Z= T.*R;
  pcolor(R.*cos(T),R.*sin(T),Z,...
	 facecolor='interp',colorbar='on',axis='equal');
  return
endfunction 

function mot_demo_10()
// PLOT3  Plot lines and points in 3-D space.
  subplot(2,2,1,a3d=%t);
  t = linspace(0,2*%pi,100);
  plot3(sin(t),cos(t),sin(2*t),'r2+',axis={'off'});
  subplot(2,2,2,a3d=%t);
  plot3(sin(t),cos(t),sin(3*t),'b3',view=[45,10]);
  subplot(2,2,3,a3d=%t);
  plot3(sin(t),cos(t),t,'g3',view=[1 1 1]);
  // plot3(sin(t),cos(t),t,'axis',[xmin xmax ymin ymax zmin zmax]);
endfunction


function mot_demo_11()
// SURF   3-D color surface.
  subplot(2,2,1,a3d=%t);
  x=linspace(-1,1,20); y=linspace(-2,2,40);
  Z=cos(y'*x);
  surf(x,y,Z);
  subplot(2,2,2,a3d=%t);
  surf(x,y,Z,axis='off',shading='interp');

  subplot(2,2,3,a3d=%t);
  x=linspace(-1,1,20); y=linspace(-2,2,40);
  function z=f(x,y); z=(x.^2-y.^2); endfunction;
  surf(x,y,f,shading='flat');
  
  subplot(2,2,4,a3d=%t);
  u=linspace(0,2*%pi,50);
  v=linspace(0,%pi,25);
  function [x,y,z]=sphere(u,v)
    x=sin(v).*cos(u);y=sin(v).*sin(u); z= cos(v);
  endfunction;
  function c=color(u,v);c=cos(u).*sin(v);endfunction;
  surf(u,v,sphere,color,shading='flat');
endfunction 

function mot_demo_12()
// SURFL   3-D surface with lighting
  subplot(2,2,1,a3d=%t);    
  x=linspace(-1,1,20); y=linspace(-2,2,40);
  Z=cos(y'*x);
  surfl(x,y,Z);

  subplot(2,2,2,a3d=%t);    
  surfl(x,y,Z,axis='off',shading='interp');

  subplot(2,2,3,a3d=%t);    
  x=linspace(-1,1,20); y=linspace(-2,2,40);
  function z=f(x,y);z=x.^2-y.^2; endfunction 
  surfl(x,y,f,shading='flat')

  subplot(2,2,4,a3d=%t);    
  u=linspace(0,2*%pi,50);
  v=linspace(0,%pi,25);
  function [x,y,z]=sphere(u,v)
    x=sin(v).*cos(u);
    y=sin(v).*sin(u);
    z=cos(v);
  endfunction
  surfl(u,v,sphere,shading='flat', light=[1 0 0]);
endfunction 

function mot_demo_13()
// TRIMESH Triangular surface mesh 
  trimesh();
endfunction

function mot_demo_14()
// TRIPCOLOR  2D Triangular pseudocolor plot.
  tripcolor();
endfunction

function mot_demo_15()
// TRIPLOT  2D plot of a triangulation
  triplot();
endfunction 

function mot_demo_16()
//  TRISURF Triangular surface plot.
  subplot(2,2,1,a3d=%t)
  trisurf();
  // TRISURFL Triangular surface plot with with lighting 
  subplot(2,2,2,a3d=%t)
  colormap('gray')
  trisurfl();
  subplot(2,2,3,a3d=%t)
  // VIEW   property for 3D graphs
  u=linspace(0,2*%pi,50);
  v=linspace(0,%pi,25);
  function [x,y,z]=sphere(u,v)
    x=sin(v).*cos(u);
    y=sin(v).*sin(u);
    z=cos(v);
  endfunction
  mesh(u,v,sphere,view=[30 30]);
endfunction

// utility function to build a list for demos 

function L=build_demo_list(str,n)
  L = list() 
  for i=1:n
    name=sprintf("%s_%d",str,i); 
    execstr(sprintf("info_d=""%s_%d_info"";",str,i)); 
    L(i) = list(info_d,"not-used",name);
  end 
endfunction

// organize the previous list for graphic demo widget 
graphic_test_2d = build_demo_list("mot_demo",16);

graphic_demos_all=list(list("mottelet", "", "", graphic_test_2d  ));
opengl=%f; 
graphics_demo_in_gtk(graphic_demos_all,opengl);

