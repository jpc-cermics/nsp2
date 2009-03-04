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

fmode = %t; 
mode = "Cairo";
mode = "Gtk";
//mode = "OpenGl";
F=figure_create(wresize=%t,fname=mode,driver=mode,id=20);
// a top level axes 
A=objs3d_create(top=%t,title="Main title", colormap=cmap,box_style=0,box_color=257);
// ,wrect=[0,0,1,1],frect=[0,-2,6,2],arect=[1,1,1,1]/12);
F.children(1)= A;
A.children(1)=Q;
A.children(2)=P;
F.connect[];


  


