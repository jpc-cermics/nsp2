// une courbe

xset("colormap", [jetcolormap(128);[0.9 0.9 0.9]]);

t = linspace(-16*%pi,16*%pi,4000);
xx = abs(t/(16*%pi)).*cos(t);
yy = abs(t/(16*%pi)).*sin(t);
zz = t/(16*%pi);
colors = dsearch(0.5*(xx(1:$-1)+xx(2:$)),linspace(-1,1,129));
L = tlist(["polyline" "coord" "color" "mark"], [xx;yy;zz], colors);

nu = 60;
nv = 30;
u = linspace(0, 2*%pi, nu);
v = linspace(0, %pi, nv);

Q = psurf_to_polyhedron(u, v, sphere, 1, 64);  // mise sous forme d'un objet "polyedre"


listObj=list(L,Q);
// a coder en dur 
[Gmin, Gmax] = emboite(listObj)
ebox=matrix([Gmin';Gmax'],1,6);
draw3d_objs(listObj,ebox=ebox,box_color =129,box_style="other");
