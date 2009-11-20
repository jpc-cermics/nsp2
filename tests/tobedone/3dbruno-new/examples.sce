// first example 
// 

function demo_3dlib_1(F,mode="Gtk") 
  [C, L, LL, P] = obj1();
  S = string3d_create( Mcoord=[1.2;0.5;0.5],str="A string !");
  A=objs3d_create(top=%t,title="Main title",box_color=0,box_style=1,with_box=%f) 
  // ,wrect=[0,0,1,1],frect=[0,-2,6,2],arect=[1,1,1,1]/12);
  F.children(1)=A;
  A.children(1)=C;
  A.children(2)=L;
  A.children(3)=LL;
  A.children(4)=S;
  A.children(5)=P;
  if %f then 
    for i=1:size(P.Mcoord,'c');
      S = string3d_create(Mcoord= P.Mcoord(:,i), str=string(i)+"XX");
      A.children($+1)=S;
    end 
  end
  F.connect[];
endfunction 


function demo_3dlib_2(F,mode="Gtk") 
  ncol= 32
  A=objs3d_create(top=%t,title="Main title",colormap=hotcolormap(ncol),...
		  with_box=%t,box_color=29,box_style=2);
  // ,wrect=[0,0,1,1],frect=[0,-2,6,2],arect=[1,1,1,1]/12);
  F.children(1)= A;
  x = linspace(0,2*%pi,40);
  z = cos(x')*cos(x);
  A.children(1) = zsurf_to_polyhedron(x, x, z, 1,ncol);
  F.connect[];
endfunction


function demo_3dlib_3(F,mode="Gtk") 
  ncol= 32;
  cm=[jetcolormap(128);[0.9 0.9 0.9]];
  A=objs3d_create(top=%t,title="Main title", colormap=cm,...
                  box_color=129,box_style=0,with_box=%t );
  // ,wrect=[0,0,1,1],frect=[0,-2,6,2],arect=[1,1,1,1]/12);
  F.children(1)= A;
  function [z] = rozen(x,y)
  // pour plot3d
    C = 1
    x = x(:)
    y = (y(:))'
    z = C*(ones(size(x))*y - x.^2*ones(size(y))).^2 + (1 - x).^2*ones(size(y))
  endfunction
  n = 20;
  x = linspace(-1.5,2,n);
  y = linspace(-2,3,n);
  z = rozen(x,y);
  // on limite l'amplitude en z
  z = 3*z/max(z);
  P = zsurf_to_spolyhedron(x, x, z, 1, 128); 
  A.children(1)=P;
  F.connect[];
endfunction

function demo_3dlib_4(F,mode="Gtk") 
  // a top level axes 
  ncol= 32;
  cm=[jetcolormap(128);[0.9 0.9 0.9]];
  A=objs3d_create(top=%t,title="Main title", colormap=cm);
  // ,wrect=[0,0,1,1],frect=[0,-2,6,2],arect=[1,1,1,1]/12);
  F.children(1)= A;
  n = 80;
  x = linspace(-2, 2, n);
  y = linspace(-2, 2, n);
  z = eval3d(milk_drop, x, y);
  // on limite l'amplitude en z
  z = ( 2*z / max(z) );
  P = zsurf_to_spolyhedron(x, x, z, 1, 128, back_color=40); 
  A.children(1)=P;
  F.connect[];
endfunction

function demo_3dlib_5(F,mode="Gtk") 
  xset("colormap", jetcolormap(64));
  // a top level axes 
  ncol= 32;
  cm=[jetcolormap(64)];
  A=objs3d_create(top=%t,title="Main title", colormap=cm);
  // ,wrect=[0,0,1,1],frect=[0,-2,6,2],arect=[1,1,1,1]/12);
  F.children(1)= A;
  x = linspace(0,2*%pi,40);
  z = -cos(x')*cos(x);
  P = zsurf_to_polyhedron(x, x, z, 1, 20);
  A.children(1)=P;
  F.connect[];
endfunction


function demo_3dlib_6(F,mode="Gtk") 
  n = 80;
  x = linspace(0,2*%pi,n);
  z = 1.5*cos(x')*cos(x);
  P = zsurf_to_spolyhedron(x, x, z, 1, 64);

  // une courbe (légèrement) au dessus de la surface
  t = linspace(0,2*%pi,200);
  xx = 0.7*cos(t)+%pi;
  yy = 0.7*sin(t)+%pi;
  zz = 1.5*cos(xx).*cos(yy) + 0.02;
  L1 = polyline3d_create(Mcoord= [xx;yy;zz]' ,Mcolor=1);

  // une autre courbe (légèrement) au dessus de la surface
  xx = 2.5*cos(t)+%pi;
  yy = 2.5*sin(t)+%pi;
  zz = 1.5*cos(xx).*cos(yy) + 0.02;
  L2 = polyline3d_create(Mcoord= [xx;yy;zz]' ,Mcolor=52);

  // quelques points
  nb_points = 20;
  coord = [grand(2, nb_points, "unf", 0, 2*%pi); ...
	   grand(1, nb_points, "unf",-1, 1)];
  Q = points3d_create(Mcoord=coord',color=1,mark_type=9);

  xset("colormap", [hotcolormap(64);[0.8 0.9 1]]);

  // a top level axes 
  cmap=  [hotcolormap(64);[0.8 0.9 1]];
  A=objs3d_create(top=%t,title="Main title", colormap=cmap);
  // ,wrect=[0,0,1,1],frect=[0,-2,6,2],arect=[1,1,1,1]/12);
  F.children(1)= A;
  A.children(1)= P;
  A.children(2)= L1;
  A.children(3)= L2;
  A.children(4)= Q;
  F.connect[];
endfunction 

function demo_3dlib_7(F,mode="Gtk") 
// exemple 4 : two polyhedron 
// 
  // a top level axes 
  cmap =  [jetcolormap(64) ; graycolormap(64) ; [0.9 0.9 0.9]];
  A=objs3d_create(top=%t,title="Main title", colormap=cmap);
  // ,wrect=[0,0,1,1],frect=[0,-2,6,2],arect=[1,1,1,1]/12);
  F.children(1)= A;

  x = linspace(-1,1,80);
  y = x;
  z = (x.^2)'*ones(size(y)) - ones(size(x'))*y.^2;
  z2 =  0.2*(x'*ones(size(y)) + ones(size(x'))*y);
  P = zsurf_to_polyhedron(x, y, z, 1, 64, back_color=0,mesh=%f);
  Q = zsurf_to_polyhedron(x, y, z2, 65, 128, back_color=0,mesh=%f);
  A.children(1)=P;
  A.children(2)=Q;
  F.connect[];
endfunction 


function demo_3dlib_8(F,mode="Gtk") 
// exemple 5 : un tore bossele et une sphere
  nu = 120;
  nv = 60;
  u = linspace(0, 2*%pi, nu);
  v = linspace(0, 2*%pi, nv);
  P = psurf_to_spolyhedron(u, v, tore_bossele, 1, 128,back_color=2);
  nu = 60;
  nv = 30;
  u = linspace(0, 2*%pi, nu);
  v = linspace(0, %pi, nv);

  Q = sphere_ter([0;0;0],0.5,3, 129, 256, 0);

  cmap= [hotcolormap(128) ; jetcolormap(128); [0.9 0.9 0.9]];
  // a top level axes 
  A=objs3d_create(top=%t,title="Main title", colormap=cmap,box_style=0,box_color=257);
  // ,wrect=[0,0,1,1],frect=[0,-2,6,2],arect=[1,1,1,1]/12);
  F.children(1)= A;
  A.children(1)=Q;
  A.children(2)=P;
  F.connect[];
endfunction 


function demo_3dlib_9(F,mode="Gtk") 
// exemple 6 : l'helice torique
  nu = 120;
  nv = 30;
  u = linspace(0, 6*%pi, nu);
  v = linspace(0, 2*%pi, nv);
  P = psurf_to_spolyhedron(u, v, helice_torique, 1, 64,mesh=%t);
  cmap= [hotcolormap(64); [0.8 0.9 1]];
  // a top level axes 
  A=objs3d_create(top=%t,title="Main title", colormap=cmap,box_style=0,box_color=65);
  // ,wrect=[0,0,1,1],frect=[0,-2,6,2],arect=[1,1,1,1]/12);
  F.children(1)= A;
  A.children(1)=P;
  F.connect[];
endfunction 

function demo_3dlib_10(F,mode="Gtk") 
// exemple 7 : un peu comme un plot3d1
  
  x = linspace(0,1,60);
  y = x.*(1-x);
  z = 10*y'*y;

  P = zsurf_to_spolyhedron(x, x, z, 1, 64, back_color=1);

  t = linspace(0,2*%pi,200);
  r = sqrt(0.5);
  xx = r*cos(t)+0.5;
  yy = r*sin(t)+0.5;
  zz = zeros(size(xx));
  color = 1;
  L1 = polyline3d_create(Mcoord= [xx;yy;zz]', Mcolor=color);

  cmap= [jetcolormap(64);[0.9 0.9 0.9]];
  // a top level axes 
  A=objs3d_create(top=%t,title="Main title", colormap=cmap,box_style=0,box_color=65);
  // ,wrect=[0,0,1,1],frect=[0,-2,6,2],arect=[1,1,1,1]/12);
  F.children(1)=A;
  A.children(1)=P;
  A.children(2)=L1;
  F.connect[];
endfunction 

function demo_3dlib_11(F,mode="Gtk") 
// une courbe

  cmap= [jetcolormap(128);[0.9 0.9 0.9]];

  t = linspace(-16*%pi,16*%pi,4000);
  xx = abs(t/(16*%pi)).*cos(t);
  yy = abs(t/(16*%pi)).*sin(t);
  zz = t/(16*%pi);
  colors = bsearch(0.5*(xx(1:$-1)+xx(2:$)),linspace(-1,1,129));
  L = polyline3d_create(Mcoord=[xx;yy;zz]', Mcolor=colors);

  nu = 60;
  nv = 30;
  u = linspace(0, 2*%pi, nu);
  v = linspace(0, %pi, nv);
  Q = psurf_to_polyhedron(u, v, sphere, 1, 64,mesh=%f);  // mise sous forme d'un objet "polyedre"
  // a top level axes 
  A=objs3d_create(top=%t,title="Main title", colormap=cmap,box_style=0,box_color=129);
  // ,wrect=[0,0,1,1],frect=[0,-2,6,2],arect=[1,1,1,1]/12);
  F.children(1)= A;
  A.children(1)=Q;
  A.children(2)=L;
  F.connect[];
endfunction 

function demo_3dlib_12(F,mode="Gtk") 
  A=objs3d_create(top=%t,title="Main title",box_color=0,box_style=1,with_box=%t) 
  // ,wrect=[0,0,1,1],frect=[0,-2,6,2],arect=[1,1,1,1]/12);
  F.children(1)= A;

  A.children($+1) = string3d_create(Mcoord=[0;-1;-1],str="x,m,m");
  A.children($+1) = string3d_create(Mcoord=[0;1;-1],str="x,M,m");
  A.children($+1) = string3d_create(Mcoord=[0;-1;1],str="x,m,M");
  A.children($+1) = string3d_create(Mcoord=[0;1;1],str="x,M,M");
  A.children($+1) = string3d_create(Mcoord=[-1;-1;0],str="m,m,z");
  A.children($+1) = string3d_create(Mcoord=[-1;1;0],str="m,M,z");
  A.children($+1) = string3d_create(Mcoord=[1;-1;0],str="M,m,z");
  A.children($+1) = string3d_create(Mcoord=[1;1;0],str="M,M,z");
  A.children($+1) = string3d_create(Mcoord=[-1;0;-1],str="m,y,m");
  A.children($+1) = string3d_create(Mcoord=[1;0;-1],str="M,y,m");
  A.children($+1) = string3d_create(Mcoord=[-1;0;1],str="m,y,M");
  A.children($+1) = string3d_create(Mcoord=[1;0;1],str="M,y,M");
  A.children($+1) = string3d_create(Mcoord=[-1;-1;-1],str="0");

  A.children($+1) = string3d_create(Mcoord=[1;1;1],str="1");
  A.children($+1) = string3d_create(Mcoord=[-1;-1;1],str="2");
  A.children($+1) = string3d_create(Mcoord=[1;1;-1],str="3");
  A.children($+1) = string3d_create(Mcoord=[1;-1;1],str="4");
  A.children($+1) = string3d_create(Mcoord=[-1;1;-1],str="5");
  A.children($+1) = string3d_create(Mcoord=[1;-1;-1],str="6");
  A.children($+1) = string3d_create(Mcoord=[-1;1;1],str="7");

  A.children($+1) = polyline3d_create(Mcoord=[-1 , 1;-1 ,-1;-1 ,-1]', Mcolor=2);
  A.children($+1) = polyline3d_create(Mcoord=[-1 ,-1;-1 , 1;-1 ,-1]', Mcolor=2);
  A.children($+1) = polyline3d_create(Mcoord=[-1 ,-1;-1 ,-1;-1  ,1]', Mcolor=2);
  C = cube();
  C.Mcoord = C.Mcoord/2;
  A.children($+1) = C;
  F.connect[];
endfunction 


function demo_3dlib_13(F,mode="Gtk") 
// une courbe

  cmap=[jetcolormap(128);[0.9 0.9 0.9]];
  t = linspace(-16*%pi,16*%pi,4000);
  xx = abs(t/(16*%pi)).*cos(t);
  yy = abs(t/(16*%pi)).*sin(t);
  zz = t/(16*%pi);
  colors = bsearch(0.5*(xx(1:$-1)+xx(2:$)),linspace(-1,1,129));
  L = polyline3d_create(Mcoord= [xx;yy;zz]', Mcolor=colors);

  Q = sphere_bis([0;0;0],0.6,3);
  z = matrix(Q.Mcoord(Q.Mface,3),3,-1);
  z = sum(z,'r')/ size(z,'r');
  Q.Mcolor = color_scaling(z, 1, 128);
  // a top level axes 
  A=objs3d_create(top=%t,title="Main title", colormap=cmap,box_style=0,box_color=129);
  // ,wrect=[0,0,1,1],frect=[0,-2,6,2],arect=[1,1,1,1]/12);
  F.children(1)= A;
  A.children(1)=Q;
  A.children(2)=L;
  F.connect[];
endfunction

function demo_3dlib_14(F,mode="Gtk") 
  polyedres = ["tetrahedron","cube","octahedron","dodecahedron","icosahedron"];
  cm = xget("colormap");
  cmap= [cm;[0.92 0.92 0.92]];
  // a top level axes 
  A=objs3d_create(top=%t,title="Main title", colormap=cmap,box_style=0,box_color=33,alpha=56,theta=52);
  // ,wrect=[0,0,1,1],frect=[0,-2,6,2],arect=[1,1,1,1]/12);
  F.children(1)= A;
  angle = 2*%pi/5;
  for i=1:5
    execstr("P="+polyedres(i)+"()");  
    P.Mcoord = P.Mcoord/norm(P.Mcoord(1,:)); // try to normalize the sizes
    xt = 2*cos(i*angle);
    yt = 2*sin(i*angle);
    P.Mcoord(:,1) = P.Mcoord(:,1) + xt;
    P.Mcoord(:,2) = P.Mcoord(:,2) + yt;
    A.children($+1) = string3d_create(Mcoord=[xt;yt;1.2],str=polyedres(i));
    A.children($+1) = P;
  end
  F.connect[];
endfunction


function demo_3dlib_15(F,mode="Gtk") 
  D = dino_steph_nsp();
  // This should be given by a method 
  xmin = min(D.Mcoord(1,:)); xmax = max(D.Mcoord(1,:)); 
  ymin = min(D.Mcoord(2,:)); ymax = max(D.Mcoord(2,:)); 
  zmin = min(D.Mcoord(3,:)); zmax = max(D.Mcoord(3,:)); 
  posT = [0.5*(xmin+xmax);...
	  0.5*(ymin+ymax);...
	  zmax + 0.1*(zmax-zmin)];

  posH = [35;...
	  ymin - 0.2*(ymax-ymin);...
	  -130];

  T = string3d_create(Mcoord=posT,str="Top");
  H = string3d_create(Mcoord=posH,str="He, I am Dino");

  cmap = [jetcolormap(128);[0.92 0.92 0.92]];
  // a top level axes 
  A=objs3d_create(top=%t,title="Main title", colormap=cmap,box_style=0,box_color=129,alpha=56,theta=52);
  // ,wrect=[0,0,1,1],frect=[0,-2,6,2],arect=[1,1,1,1]/12);
  F.children(1)= A;
  A.children($+1) = D;
  A.children($+1) = T;
  A.children($+1) = H;
  F.connect[];
endfunction 


function all_3d_demos(F,mode="Gtk") 
  for i=1:15 
    F.children = list();
    execstr('demo_3dlib_'+string(i)+'(F,mode=mode)');
    F.invalidate[];
    xclick();
  end
endfunction

if ~new_graphics() 
  switch_graphics()
end

exec('NSP/tests/tobedone/3dbruno-new/libbruno.sce');
mode="Gtk";
F=figure_create(wresize=%t,fname=mode,driver=mode,id=20);
all_3d_demos(F,mode=mode);



  
  












