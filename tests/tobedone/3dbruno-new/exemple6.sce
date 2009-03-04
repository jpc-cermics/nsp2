// exemple 6 : l'helice torique

nu = 120;
nv = 30;
u = linspace(0, 6*%pi, nu);
v = linspace(0, 2*%pi, nv);
P = psurf_to_spolyhedron(u, v, helice_torique, 1, 64,mesh=%t);

cmap= [hotcolormap(64); [0.8 0.9 1]];

fmode = %t; 
mode = "Cairo";
mode = "Gtk";
//mode = "OpenGl";
F=figure_create(wresize=%t,fname=mode,driver=mode,id=20);
// a top level axes 
A=objs3d_create(top=%t,title="Main title", colormap=cmap,box_style=0,box_color=65);
// ,wrect=[0,0,1,1],frect=[0,-2,6,2],arect=[1,1,1,1]/12);
F.children(1)= A;
A.children(1)=P;
F.connect[];




