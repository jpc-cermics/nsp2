
// exemple 4 : two polyhedron 
// 

fmode = %t; 
mode = "Cairo";
mode = "Gtk";
//mode = "OpenGl";
F=figure_create(wresize=%t,fname=mode,driver=mode,id=20);
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


  


