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

// A set of demos 
//-------------------------

// utility function to build a list for demos 

function L=build_demo_list(str,n)
  L = list() 
  for i=1:n
    name=sprintf("%s_%d",str,i); 
    execstr(sprintf("info_d=%s_%d_info;",str,i)); 
    L(i) = list(info_d,"not-used",name);
  end 
endfunction

// demo for 2d plots 
// -----------------------

demo_2d_1_info='plot2d';

function demo_2d_1()
  t=(0:0.1:6)*%pi;
  xset("font size",2);
  plot2d(t,sin(t),style=2);
  xtitle("One curve given by 2 row or column vectors","Time","sin(t)");
  xgrid(5);
endfunction

demo_2d_2_info='plot2d log scale';

function demo_2d_2()
  xset("font size",2);
  plot2d([],(1:10:10000),logflag="nl",leg="log(t)",leg_pos="ur");
  xtitle("plot2d1 log scale","t","y log scale");
  xgrid(3);
endfunction

demo_2d_3_info='plot2d3';

function demo_2d_3()
  n=32-1;t=(0:n)./n;
  xset("font size",2);
  u=sin(80*%pi*t)+sin(100*%pi*t);
  plot2d3([],rand(100,1,"n"));
  xtitle("plot2d3 (vertical bars plot)","t","f(t)");
endfunction

demo_2d_4_info='plot2d superpose';

function demo_2d_4()
  v=(1:20)+(1:20).*rand(1,20,"n");
  xset("font size",2);
  plot2d([],v);
  plot2d([],(1:20),style=[2],strf="100",leg="estimated");
  xtitle("plot2d1. Two curves drawn, the first one set the scale"," "," ");
endfunction

demo_2d_5_info='plot2d with function';

function demo_2d_5()
  function y=f(x) ; y=sin(1/x); endfunction
  plot2d(linspace(1.e-6,1,1000),f,leg="sin(1/x)",leg_pos="dr")
  xtitle("plot2d(x,f,...)");
endfunction

demo_2d_6_info='histplot';

function demo_2d_6()
  histplot();
endfunction

// organize the previous list for graphic demo widget 

graphic_test_2d = build_demo_list("demo_2d",6);

// a sublist for 3d plots 
// -----------------------

demo_3d_1_info="param3d";

function demo_3d_1()
  param3d();
  xtitle("param3d : parametric curves in R3"," "," ");
endfunction

demo_3d_2_info="param3d";

function demo_3d_2() 
  t=-50*%pi:0.1:50*%pi;
  x=t.*sin(t);y=t.*cos(t);z=t.*abs(t)./(50*%pi);
  param3d(x,y,z,alpha=45,theta=60);
  title=["param3d : parametric curves in R3 (t.sin(t),t.cos(t),t.|t|/50.%pi)"];
  xtitle(title," "," ");
endfunction

demo_3d_3_info="plot3d";

function demo_3d_3() 
  t=(-1:0.1:1)*%pi;plot3d(t,t,sin(t)'*cos(t),alpha=80,theta=70);
  title=["plot3d : z=sin(x)*cos(y)"];
  xtitle(title," "," ");
endfunction

demo_3d_4_info="plot3d1";

function demo_3d_4() 
  xset('colormap',hotcolormap(45));
  t=(-1:0.1:1)*%pi;plot3d1(t,t,sin(t)'*cos(t),alpha=80,theta=70);
  title=["plot3d1 : z=sin(x)*cos(y)"];
  xtitle(title," "," ");
endfunction

demo_3d_5_info="plot3d with function";

function demo_3d_5()
  function z=f(x,y); z= sin(exp(2*(x+0.2)))*cos(y);endfunction
  xset('colormap',hotcolormap(45));
  t=(-1:0.1:1);plot3d1(t,t,f)
  title=["plot3d1(.,.,f,..) with a function f"];
  xtitle(title," "," ");
endfunction

demo_3d_6_info="multiple colormaps";

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


demo_3d_7_info="interpolated shading";

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

demo_3d_8_info="genfact and interp. shad.";

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

demo_3d_9_info="genfact and interp. shad.";

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

demo_3d_10_info="Tree";

function demo_3d_10() 
  // a demo by Quentin Quadrat.
  exec(NSP+'/demos/graphics/NouvArbreQ.sci');
endfunction

demo_3d_11_info="parametric surface nf3d";

function demo_3d_11_old() 
// parametric 3d surface 
// nf3d 
  u = %pi*(-1:0.2:1);
  v = %pi*(-1:0.2:1);
  n = size(u,'*');
  x= cos(u)'*exp(cos(v));
  y= cos(u)'*sin(v);
  z= sin(u)'*ones(v);
  col=ones(u)'*cos(v);
  col=(n-1)*(col-min(col))/(max(col)-min(col))+1;
  xset('colormap',hotcolormap(n));
  [xx,yy,zzcol]=nf3d(x,y,col);
  [xx,yy,zz]=nf3d(x,y,z);
  plot3d(xx,yy,zz,colors=zzcol,alpha=55,theta=110)
endfunction

function demo_3d_11() 
  u = %pi*(-1:0.2:1)/2;
  v = %pi/2*(-1:0.2:1);
  n = size(u,'*');
  x= cos(u)'*exp(cos(v));
  y= cos(u)'*sin(v);
  z= sin(u)'*ones(v);
  col=ones(u)'*cos(v); 
  col=(n-1)*(col-min(col))/(max(col)-min(col))+1; 
  xset('colormap',hotcolormap(n));
  [xx,yy,zz]=nf3d(x,y,z);
  [xx,yy,zzcol]=nf3d(x,y,col); 
  xx=[xx,-xx];yy=[yy,-yy];zz=[zz,zz];zzcol=[zzcol,zzcol]; 
  plot3d(xx,yy,zz,colors=zzcol,alpha=55,theta=110,flag=[0,2,0]); 
endfunction

demo_3d_12_info="Cube";

function demo_3d_12() 
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
  
demo_3d_13_info="Tube";

function demo_3d_13() 
// following a parametric 3d curve 
  T=6;
  t=0:0.2:T;
  ptc=[sin(t);cos(t);t];

  for i=1:(size(ptc,'c')-1)
    pt=ptc(:,i);
    ptn=ptc(:,i+1);
    u= ptn-pt;
    u= u / sqrt(sum(u.*u))
    // find an orthogonal basis (u,v,w)
    I=find(u==0.0);
    if I<>[] then 
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
      ptg=pts+pt*ones(alpha);
    else
      ptg=ptd;
    end
    ptd=pts+ptn*ones(alpha);
    xpol=[ptg(1,1:$-1);ptd(1,1:$-1);ptd(1,2:$);ptg(1,2:$)];
    ypol=[ptg(2,1:$-1);ptd(2,1:$-1);ptd(2,2:$);ptg(2,2:$)];
    zpol=[ptg(3,1:$-1);ptd(3,1:$-1);ptd(3,2:$);ptg(3,2:$)];
    plot3d1(xpol,ypol,zpol);
  end 
endfunction

demo_3d_14_info="Shell";

function demo_3d_14() 
  t=4*%pi*(0:20)/20;
  ptc=[t.*sin(t);t.*cos(t);0*ones(t)];
  for i=1:(size(ptc,'c')-1)
    pt=ptc(:,i);
    ptn=ptc(:,i+1);
    u= ptn-pt;
    u= u / sqrt(sum(u.*u))
    // trouver un vecteur ds le plan orthogonal
    I=find(u==0.0);
    if I<>[] then 
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
      ptg=pts+pt*ones(alpha);
    else
      ptg=ptd;
    end
    ptd=pts+ptn*ones(alpha);

    xpol=[ptg(1,1:$-1);ptd(1,1:$-1);ptd(1,2:$);ptg(1,2:$)];
    ypol=[ptg(2,1:$-1);ptd(2,1:$-1);ptd(2,2:$);ptg(2,2:$)];
    zpol=[ptg(3,1:$-1);ptd(3,1:$-1);ptd(3,2:$);ptg(3,2:$)];
    plot3d1(xpol,ypol,zpol);
  end 
endfunction

// organize the previous list 
// for graphic demo widget 

graphic_test_3d = build_demo_list("demo_3d",14);

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


//-------------------------------
// animations 
//-------------------------------

function demo_anim_1()
  t=%pi*(-5:5)/5;
  plot3d1(t,t,sin(t)'*cos(t),alpha=35,theta=45);
  st=1;
  for i=35:st:80, // loop on theta angle
    xclear();
    plot3d1(t,t,sin(t)'*cos(t),alpha=i,theta=45,flag=[1,2,4])
    xset("wshow");
  end
  for i=45:st:80, //loop on alpha angle
    xclear()
    plot3d1(t,t,sin(t)'*cos(t),alpha=80,theta=i,flag=[1,2,4])
    xset("wshow");
  end
endfunction

function demo_anim_2()
  np=10;
  t=(0:0.1:np)*%pi;
  for i=1:1:30
    xclear();
    param3d((t/(np*%pi)*%pi).*sin(t),(t/(np*%pi)*%pi).*cos(t),...
	    i*t/(np*%pi),alpha=35,theta=45,flag=[2,4]);
    xset("wshow");
  end
endfunction

function demo_anim_3()
  t=-%pi:0.3:%pi;
  for i=35:80,
    xclear();
    contour(t,t,sin(t)'*cos(t),10,alpha=i,theta=45,flag=[1,2,4])
    xset("wshow");
  end
  for i=45:80,
    xclear();
    contour(t,t,sin(t)'*cos(t),10,alpha=80,theta=i,flag=[1,2,4])
    xset("wshow");
  end
endfunction

function demo_anim_4()
  t=%pi*(-1:0.1:1);
  I=20:-1:1;
  ebox=[min(t),max(t),min(t),max(t),-1,1];
  //realtimeinit(0.1)
  for i=1:size(I,'*')
    //realtime(i)
    xclear();
    plot3d1(t,t,sin((I(i)/10)*t)'*cos((I(i)/10)*t),alpha=35,theta=45,flag=[2,1,0],ebox=ebox)
    xset("wshow");
  end
endfunction 

function demo_anim_5()
  t=%pi*(-5:5)/5;
  plot3d1(t,t,sin(t)'*cos(t),alpha=35,theta=45);
  alpha=35 + (0:2:60)
  theta=45 + (0:2:60)
  w=xget('window')
  for i=1:size(alpha,'*')
    xclear(w,%f);// clear but keep recorded graphics 
    xtape('replayna',w,theta(i),alpha(i));
    xset("wshow");
  end
endfunction

function demo_anim_6()
  a=ones(60,60);
  for i=-60:60;
    b=3*tril(a,i)+2*triu(a,i+1);
    xclear();plot2d([0,10],[0,10],style=0);
    Matplot1(b,[4,4,9,9]);xset('wshow');
  end
endfunction

function demo_anim_7()
  x=%pi*(-1:0.1:1);y=x;
  for i=1:50
    xclear();
    champ1(x,y,sin((i/10)+x'*y),cos((i/10)+x'*y),
           rect=[-%pi,-%pi,%pi,%pi],arfact=3)
    xset('wshow');
  end
endfunction

// organize the previous list for graphic demo widget 

graphic_test_anim = list() 
for i=1:7
  name=sprintf("demo_anim_%d",i); 
  test_info=sprintf("test %d",i); 
  //execstr('test_info='+name+'(info=%t);');
  graphic_test_anim(i) = list(test_info, "not-used",name);
end 

// ------------------------------
// Matplot Matplot1 fec grayplot 
// ------------------------------

demo_contour_1_info="grayplot";

function demo_contour_1()
  xset('colormap',hotcolormap(20)) 
// xset('colormap',jetcolormap(20)) 
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

demo_contour_2_info="fec";

function demo_contour_2()
  xset('colormap',hotcolormap(20))
  N=4;
  // build the nodes 
  i=linspace(0,2*%pi,N+1); i($)=[];
  xn=[cos(i)';0];
  yn=[sin(i)';0];
  Nnodes=N+1;
  // values at nodes 
  val=ones(Nnodes,1);
  val($)=0;
  // build the triangles 
  Ntriang=N;
  triangles_nodes=[1:N;[(1:N-1)+1,1];(N+1)*ones(1,N)];
  triangles=[(1:N);triangles_nodes;zeros(1,N)]';
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

demo_contour_3_info="grayplot+contour";

function demo_contour_3()
  xset("colormap",hotcolormap(40));
  t=-2:0.1:2;m=sinh(t)'*cosh(t);
  grayplot(t,t,m,shade=%t);
  contour(t,t,m,11);
endfunction

demo_contour_4_info="contour";

function demo_contour_4()
  t=-2:0.1:2;m=sinh(t)'*cosh(t);
  contour(t,t,m,[-4:0.5:4]);
endfunction

demo_contour_5_info="contourf";

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
 contourf(x,y,z,nv=m,style=0*ones(1,m))
 //xset('fpf','');
 //contourf(x,y,z,nv=m);
endfunction

demo_contour_6_info="contourf";

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


// organize the previous list for graphic demo widget 

graphic_test_contour = list() 
for i=1:6
  name=sprintf("demo_contour_%d",i); 
  execstr(sprintf("info_d=%s_info;",name)); 
  graphic_test_contour(i) = list(info_d, "not-used",name);
end 

// organize the previous list 
//------------------------------

graphic_demos_all = list( list("primitives", "", "", graphic_test_prim  ), 
                          list("2D curves", "", "", graphic_test_2d  ), 
			  list("2D contours", "", "", graphic_test_contour  ), 
                          list("3D curves and surfaces",  "", "", graphic_test_3d ),
			  list("Animations",  "", "", graphic_test_anim ));
			  			  			  
  
graphics_demo_in_gtk(graphic_demos_all)


  

