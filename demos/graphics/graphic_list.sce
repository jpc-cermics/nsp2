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
  // colors are set to z�ro : only draw polygons 
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

// ----------------------------

function demo_prim_1()
  function [v]=transl(x,t); v=x+t*ones(x); endfunction 
  xsetech(frect=[-100,-100,500,600]);
  xset('clipgrf')
  // xrects 
  x=0:40:240;
  boxes=[x;10*ones(x);30*ones(x);30*ones(x)];
  xstring(-50,-5,'xrects');
  xrects(boxes);
  // xrects
  boxes=[x;45*ones(x);30*ones(x);30*ones(x)];
  pats=[0,4,8,12,15,xget("white"),0];
  xstring(-50,20,'xrects');
  xrects(boxes,pats);
  // xarcs 
  boxes=[x;90*ones(x);30*ones(x);30*ones(x)];
  arcs=[boxes; 0*ones(x);64*180*ones(x)];
  pats=[0,4,8,12,15,xget("white"),0];
  xstring(-50,75,'xarcs');
  xarcs(arcs,pats);
  // xarcs 
  boxes=[x;135*ones(x);30*ones(x);30*ones(x)];
  arcs=[boxes; 0*ones(x);64*360*ones(x)];
  xarcs(arcs);
  // xfpolys
  x1=[0,10,20,30,20,10,0];
  y1=[15,30,30,15,0,0,15];y1=160*ones(y1)+y1;
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
  xsegs([x;x+30*ones(x)],[(360+40)*ones(x);(360+70)*ones(x)]);
  xinfo("[I.5] xsegs(x,y)");
  // 
  xarrows([x;x+30*ones(x)],[(360+70)*ones(x);(360+100)*ones(x)]);
  xinfo(["[I.6] xarrows(x,y)"]);
  // 
  x=0:100:200;
  xnumb(x,500*ones(x),[10,20,35],1);
  xnumb(x,550*ones(x),[10,20,35],0);
  xinfo(["[[II.3] xnumb()"]);
  xset('clipoff')
endfunction

function demo_prim_2()
  function [v]=transl(x,t); v=x+t*ones(x); endfunction 
  xsetech(frect=[-100,-100,500,600]);
  xset('clipgrf')
  xrect(20,120,60,60)
  xfrect(100,120,60,60)
  xarc(20,200,50,70,0,64*(225))
  xfarc(100,200,50,70,0,64*225)
  x=0:1:%pi;
  [n1,n2]=size(x);
  x1=50*sin(x)+40*ones(x);
  y1=50*cos(x)+90*ones(x);
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
  xset('clipping',150,460,100,150);
  x=0:0.2:2*%pi;
  x1=[sin(x);10*sin(x)];
  y1=[cos(x);10*cos(x)];
  y1=transl(y1,20);
  xsegs(10*x1+200*ones(x1),10*y1+200*ones(y1));
  xset("clipgrf");
  
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


// organize the previous list 
// for graphic demo widget 

graphic_test_prim = list() 
for i=1:3
  graphic_test_prim(i) = list(sprintf("test%d",i), "not-used",sprintf("demo_prim_%d",i));
end 



// organize the previous list 
// for 

graphic_demos_all = list( list("primitives", "", "", graphic_test_prim  ), 
                          list("2D curves", "", "", graphic_test_2d  ), 
                          list("3D curves and surfaces",  "", "", graphic_test_3d ));
			  			  
// small test 
  
graphics_demo_in_gtk(graphic_demos_all)
  

