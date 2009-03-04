// exemple 2 : un peu comme un plot3d1


fmode = %t; 
mode = "Cairo";
mode = "Gtk";
//mode = "OpenGl";
F=figure_create(wresize=%t,fname=mode,driver=mode,id=20);
// a top level axes 
ncol= 32
A=objs3d_create(top=%t,title="Main title",colormap=hotcolormap(ncol),with_box=%t,box_color=29,box_style=2);
// ,wrect=[0,0,1,1],frect=[0,-2,6,2],arect=[1,1,1,1]/12);
F.children(1)= A;
x = linspace(0,2*%pi,40);
z = cos(x')*cos(x);
A.children(1) = zsurf_to_polyhedron(x, x, z, 1,ncol);
F.connect[];


  
