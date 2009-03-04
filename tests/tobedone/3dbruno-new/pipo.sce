// first example 
// 

fmode = %t; 
mode = "Cairo";
mode = "Gtk";
//mode = "OpenGl";
F=figure_create(wresize=%t,fname=mode,driver=mode,id=20);
A=objs3d_create(top=%t,title="Main title",box_color=0,box_style=1,with_box=%f) 
F.children(1)= A;
x=1:3;
y=5:7;
z= x'*y ;
C=zsurf_to_polyhedron(x,y,z,1,10)
A.children(1)=C;
F.connect[];
  
