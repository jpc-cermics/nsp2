// exemple 3 : comme l'exemple 2 avec des choses en plus

n = 80;
x = linspace(0,2*%pi,n);
z = 1.5*cos(x')*cos(x);
P = zsurf_to_spolyhedron(x, x, z, 1, 64);

// une courbe (légèrement) au dessus de la surface
t = linspace(0,2*%pi,200);
xx = 0.7*cos(t)+%pi;
yy = 0.7*sin(t)+%pi;
zz = 1.5*cos(xx).*cos(yy) + 0.02;
L1 = polyline3d_create(Mcoord= [xx;yy;zz] ,Mcolor=1);

// une autre courbe (légèrement) au dessus de la surface
xx = 2.5*cos(t)+%pi;
yy = 2.5*sin(t)+%pi;
zz = 1.5*cos(xx).*cos(yy) + 0.02;
L2 = polyline3d_create(Mcoord= [xx;yy;zz] ,Mcolor=52);

// quelques points
nb_points = 20;
coord = [grand(2, nb_points, "unf", 0, 2*%pi); ...
	 grand(1, nb_points, "unf",-1, 1)];
Q = points3d_create(Mcoord=coord,color=1,mark_type=9);

xset("colormap", [hotcolormap(64);[0.8 0.9 1]]);

// exemple 2 : un peu comme un plot3d1

fmode = %t; 
mode = "Cairo";
mode = "Gtk";
//mode = "OpenGl";
F=figure_create(wresize=%t,fname=mode,driver=mode,id=20);
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


  







