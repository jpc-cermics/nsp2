// More stuffs 
//-------------

function demo_surf_1()
  t=%pi*(-10:10)./10;
  box=[-%pi,%pi,-%pi,%pi,-5,1];
  z=sin(t)'*cos(t);
  contour(t,t,z,4,alpha=35,theta=45,flag=[1,1,0],ebox=box,zlevel=-5);
  plot3d(t,t,z,alpha=35,theta=45,flag=[2,1,3],ebox=box);
  title=["plot3d and contour "];
  xtitle(title," "," ");
endfunction

function demo_surf_2()
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

function demo_surf_3()
  t=%pi*(-10:10)./10;
  rect=[-%pi,%pi,-%pi,%pi,-1,1];
  z=sin(t)'*cos(t);
  plot3d(t,t,z,alpha=35,theta=45,flag=[-3,1,3],ebox=rect);
  contour(t,t,z,10,alpha=35,theta=45,flag=[0,1,0],ebox=rect);
  title=["plot3d and contour "];
  xtitle(title," "," ");
endfunction

function demo_surf_4()
  t=%pi*(-10:10)./10;
  rect=[-%pi,%pi,-%pi,%pi,-1,1];
  plot3d(t,t,z,alpha=35,theta=45,flag=[-20,1,3],ebox=rect);
  z=sin(t)'*cos(t);
  xset("alufunction",0);
  contour(t,t,z,10,alpha=35,theta=45,flag=[0,1,0],ebox=rect);
  title=["plot3d and contour X11 only"];
  xtitle(title," "," ");
  xset("default");
endfunction

