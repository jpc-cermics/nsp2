// une courbe

cmap=[jetcolormap(128);[0.9 0.9 0.9]];
t = linspace(-16*%pi,16*%pi,4000);
xx = abs(t/(16*%pi)).*cos(t);
yy = abs(t/(16*%pi)).*sin(t);
zz = t/(16*%pi);
colors = bsearch(0.5*(xx(1:$-1)+xx(2:$)),linspace(-1,1,129));
L = polyline3d_create(Mcoord= [xx;yy;zz]', Mcolor=colors);

Q = sphere_bis([0;0;0],0.6,3);
z = matrix(Q.Mcoord(Q.Mface,3),3,-1);
z = sum(z,'r')/ size(z,'r');
Q.Mcolor = color_scaling(z, 1, 128);

fmode = %t; 
mode = "Cairo";
mode = "Gtk";
//mode = "OpenGl";
F=figure_create(wresize=%t,fname=mode,driver=mode,id=20);
// a top level axes 
A=objs3d_create(top=%t,title="Main title", colormap=cmap,box_style=0,box_color=129);
// ,wrect=[0,0,1,1],frect=[0,-2,6,2],arect=[1,1,1,1]/12);
F.children(1)= A;
A.children(1)=Q;
A.children(2)=L;
F.connect[];




