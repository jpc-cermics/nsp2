// une courbe
xset("colormap", [jetcolormap(128);[0.9 0.9 0.9]]);
t = linspace(-16*%pi,16*%pi,4000);
xx = abs(t/(16*%pi)).*cos(t);
yy = abs(t/(16*%pi)).*sin(t);
zz = t/(16*%pi);
colors = dsearch(0.5*(xx(1:$-1)+xx(2:$)),linspace(-1,1,129));
L = tlist(["polyline" "coord" "color" "mark"], [xx;yy;zz], colors);

Q = sphere_bis([0;0;0],0.6,3);
z = matrix(Q.coord(3,Q.face),3,-1);
z = sum(z,'r')/ size(z,'r');
Q.color = color_scaling(z, 1, 128);

listObj=list(L,Q);
// a coder en dur 
[Gmin, Gmax] = emboite(listObj)
ebox=matrix([Gmin';Gmax'],1,6);
draw3d_objs(listObj,ebox=ebox,box_color =129,box_style="other");


