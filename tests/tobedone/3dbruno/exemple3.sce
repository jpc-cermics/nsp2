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
color = 1;
L1 = tlist(["polyline" "coord" "color" "mark"], [xx;yy;zz], color);

// une autre courbe (légèrement) au dessus de la surface
xx = 2.5*cos(t)+%pi;
yy = 2.5*sin(t)+%pi;
zz = 1.5*cos(xx).*cos(yy) + 0.02;
color = 52;
L2 = tlist(["polyline" "coord" "color" "mark"], [xx;yy;zz], color);

// quelques points
nb_points = 20;
coord = [grand(2, nb_points, "unf", 0, 2*%pi); ...
	 grand(1, nb_points, "unf",-1, 1)];
color = 1;
mark = 9;
Q = tlist(["points" "coord" "color"], coord, color, mark);

xset("colormap", [hotcolormap(64);[0.8 0.9 1]]);

// my_plot3d(list(P,L1,L2,Q), with_box = %t, ...
// 	  manip_rot = "spherique",title="A set of objects",...
// 	  phi = %pi/3, theta = %pi/6,box_color=65)



listObj=list(P,L1,L2,Q);
// a coder en dur 
[Gmin, Gmax] = emboite(listObj)
ebox=matrix([Gmin';Gmax'],1,6);
draw3d_objs(listObj,ebox=ebox,box_color=65,box_style="matlab");





