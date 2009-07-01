
fmode = %t; 
mode = "Cairo";
mode = "Gtk";
//mode = "OpenGl";
F=figure_create(wresize=%t,fname=mode,driver=mode,id=20);
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



  


