// graphic_demo_list : a list of string matrices 
// giving graphics demos examples 

// FIXME : this is a temporary load 
// of all the graphics macros 
NSP=getenv('SCI');

if %t then 
  A=glob(NSP+'/macros/xdess/*.sci');
  for i=1:size(A,1),exec(A(i,1));end;
  A=glob(NSP+'/macros/scicos/00util.sci');
  exec(A)
end 

exec(NSP+'/demos/graphics/main_graphic.sci');

// demo for 2d plots 
// -----------------------

function demo_2d_1()
  t=(0:0.1:6)*%pi;
  xset("font size",2);
  plot2d(t,sin(t),style=2);
  xtitle("One curve given by 2 row or column vectors","Time","sin(t)");
  xgrid(5);
endfunction

function demo_2d_2()
  xset("font size",2);
  plot2d([],(1:10:10000),logflag="nl",leg="log(t)");
  xtitle("plot2d1 log scale","t","y log scale");
  xgrid(3);
endfunction
     
function demo_2d_3()
  n=32-1;t=(0:n)./n;
  xset("font size",2);
  u=sin(80*%pi*t)+sin(100*%pi*t);
  plot2d3([],rand(100,1,"n"));
  xtitle("plot2d3 (vertical bars plot)","t","f(t)");
endfunction

function demo_2d_4()
  v=(1:20)+(1:20).*rand(1,20,"n");
  xset("font size",2);
  plot2d1([],v);
  plot2d1([],(1:20),style=[2],strf="100",leg="estimated");
  xtitle("plot2d1. Two curves drawn, the first one set the scale"," "," ");
endfunction

function demo_2d_5()
  fplot2d();
  xtitle("fplot2d : f given by external ","x ","f(x) ");
endfunction

function demo_2d_6()
  histplot();
endfunction

// organize the previous list 
// for graphic demo widget 

graphic_test_2d = list() 
for i=1:6
  graphic_test_2d(i) = list(sprintf("test%d",i), "not-used",sprintf("demo_2d_%d",i));
end 

// a sublist for 3d plots 
// -----------------------

function demo_3d_1()
  param3d();
  xtitle("param3d : parametric curves in R3"," "," ");
endfunction

function demo_3d_2()
  t=-50*%pi:0.1:50*%pi;
  x=t.*sin(t);y=t.*cos(t);z=t.*abs(t)./(50*%pi);
  param3d(x,y,z,alpha=45,theta=60);
  title=["param3d : parametric curves in R3 (t.sin(t),t.cos(t),t.|t|/50.%pi)"];
  xtitle(title," "," ");
endfunction

function demo_3d_3()
  t=(-1:0.1:1)*%pi;plot3d(t,t,sin(t)'*cos(t),alpha=80,theta=70);
  title=["plot3d : z=sin(x)*cos(y)"];
  xtitle(title," "," ");
endfunction

function demo_3d_4()
  xset('colormap',hotcolormap(45));
  t=(-1:0.1:1)*%pi;plot3d1(t,t,sin(t)'*cos(t),alpha=80,theta=70);
  title=["plot3d1 : z=sin(x)*cos(y)"];
  xtitle(title," "," ");
endfunction

function demo_3d_5()
  function z=f(x,y); z= sin(exp(2*(x+0.2)))*cos(y);endfunction
  xset('colormap',hotcolormap(45));
  t=(-1:0.1:1);plot3d1(t,t,f)
  title=["plot3d1(.,.,f,..) with a function f"];
  xtitle(title," "," ");
endfunction

function demo_3d_6()
  xsetech(wrect=[0,0,0.5,0.5])
  xset('colormap',graycolormap(45));
  plot3d1()
  xsetech(wrect=[0.5,0,0.5,0.5])
  xset('colormap',hotcolormap(45));
  plot3d1()
  xsetech(wrect=[0,0.5,0.5,0.5])
  xset('default_colormap');
  plot3d1()
  xsetech(wrect=[0.5,0.5,0.5,0.5])
  xset('colormap',greencolormap(34));
  plot3d1()
endfunction

function demo_3d_7()
  xsetech(wrect=[0,0,0.5,0.5])
  // One facet with interpolated shading using colors Id 
  plot3d([0,0,1]',[0,1,0]',[3,1,2]',colors=[1,2,3]',flag=[1,1,3])
  xsetech(wrect=[0.5,0,0.5,0.5])
  // The number of sub-polygons depends on the distance in Id
  // between colors
  plot3d([0,0,1]',[0,1,0]',[3,1,2]',colors=[1,6,12]',flag=[1,1,3])
  xsetech(wrect=[0,0.5,0.5,0.5])
  // colors are set to zéro : only draw polygons 
  plot3d([0,0,1]',[0,1,0]',[3,1,2]',colors=0*[1,2,3]',flag=[1,1,3])
  xsetech(wrect=[0.5,0.5,0.5,0.5])
  // colors are negative: only painting no contour drawing
  plot3d([0,0,1]',[0,1,0]',[3,1,2]',colors=-[1,6,12]',flag=[1,1,3])
endfunction

function demo_3d_8()
// using genfac3d to compute facets and node colors 
// from a standard description 
  t=[-%pi/2,0,%pi/2]';
  z=sin(t)*cos(t');
  [xx,yy,zz]=genfac3d(t,t,z); 
  col=[1,2,1;2,3,2;1,2,1]
  [xx,yy,zzcol]=genfac3d(t,t,col); 
  xsetech(wrect=[0,0,0.5,1])
  // with generated facets 
  plot3d(xx,yy,zz,colors=zzcol,);
  xsetech(wrect=[0.5,0,0.5,1])
  // without facets 
  plot3d1(t,t,z);
endfunction

function demo_3d_9()
  t=[(0:0.2:2)*%pi]'; z=sin(t)*cos(t');
  xset('colormap',hotcolormap(40));
  // remapping zvalues to colors 
  zzc = 39*(z-min(z))/(max(z)- min(z))+1;
  [xx,yy,zzcolors]=genfac3d(t,t,zzc);
  [xx,yy,zz]=genfac3d(t,t,z);
  xsetech(wrect=[0,0,0.5,1.0])
  plot3d1(t,t,z,alpha=45,theta=60);
  xsetech(wrect=[0.5,0,0.5,1])
  plot3d(xx,yy,zz,colors=zzcolors,alpha=45,theta=60);
endfunction

function demo_3d_10()
  // a demo by Quentin Quadrat.
  exec(NSP+'/demos/graphics/NouvArbreQ.sci');
endfunction

// organize the previous list 
// for graphic demo widget 

graphic_test_3d = list() 
for i=1:10
  graphic_test_3d(i) = list(sprintf("test%d",i), "not-used",sprintf("demo_3d_%d",i));
end 

// organize the previous list 
// for 

graphic_demos_all = list( list("2D curves", "", "", graphic_test_2d  ), 
                          list("3D curves and surfaces",  "", "", graphic_test_3d ));
			  			  
// small test 
  
graphics_demo_in_gtk(graphic_demos_all)
  

