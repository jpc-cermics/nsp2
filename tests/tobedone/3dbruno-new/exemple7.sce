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
L1 = polyline3d_create(Mcoord= [xx;yy;zz], Mcolor=color);

cmap= [jetcolormap(64);[0.9 0.9 0.9]];

fmode = %t; 
mode = "Cairo";
mode = "Gtk";
//mode = "OpenGl";
F=figure_create(wresize=%t,fname=mode,driver=mode,id=20);
// a top level axes 
A=objs3d_create(top=%t,title="Main title", colormap=cmap,box_style=0,box_color=65);
// ,wrect=[0,0,1,1],frect=[0,-2,6,2],arect=[1,1,1,1]/12);
F.children(1)=A;
A.children(1)=P;
A.children(2)=L1;
F.connect[];

