// exemple 7 : un peu comme un plot3d1
x = linspace(0,1,60);
y = x.*(1-x);
z = 10*y'*y;

P = zsurf_to_spolyhedron(x, x, z, 1, 64,1);
//P.val = grand(P.val,"unf",min(P.val),max(P.val));

t = linspace(0,2*%pi,200);
r = sqrt(0.5);
xx = r*cos(t)+0.5;
yy = r*sin(t)+0.5;
zz = zeros_deprecated(xx);
color = 1;
L1 = tlist(["polyline" "coord" "color" "mark"], [xx;yy;zz], color);


xset("colormap", [jetcolormap(64);[0.9 0.9 0.9]]);

listObj=list(P,L1);
// a coder en dur 
[Gmin, Gmax] = emboite(listObj)
ebox=matrix([Gmin';Gmax'],1,6);
draw3d_objs(listObj,ebox=ebox,with_mesh = %t,box_color = 65,box_style="other");
