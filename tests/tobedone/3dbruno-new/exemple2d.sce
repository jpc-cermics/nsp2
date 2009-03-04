// exemple 2 : un peu comme un plot3d1

xset("colormap", jetcolormap(64));


fmode = %t; 
mode = "Cairo";
mode = "Gtk";
//mode = "OpenGl";
F=figure_create(wresize=%t,fname=mode,driver=mode,id=20);
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

  

