// exemple 2 : un peu comme un plot3d1
x = linspace(0,2*%pi,40);
z = -cos(x')*cos(x);

P = zsurf_to_polyhedron(x, x, z, 1, 20);

xset("colormap", jetcolormap(64));

listObj=list(P);
[Gmin, Gmax] = emboite(listObj)
ebox=matrix([Gmin';Gmax'],1,6);

draw3d_objs(list(P),ebox=ebox,with_box=%t,with_mesh=%t)






