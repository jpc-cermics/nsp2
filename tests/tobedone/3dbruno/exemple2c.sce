// exemple 2c : un peu comme un plot3d

x = linspace(-2, 2, 120);
y = linspace(-2, 2, 120);
z = eval3d(milk_drop, x, y);

// on limite l'amplitude en z
z = ( 2*z / max(z) );

P = zsurf_to_spolyhedron(x, x, z, 1, 128);
P.back_color = 40;

xset("colormap", [jetcolormap(128);[0.9 0.9 0.9]]);

listObj=list(P);
// a coder en dur 
[Gmin, Gmax] = emboite(listObj)
ebox=matrix([Gmin';Gmax'],1,6);
draw3d_objs(listObj,ebox=ebox,box_color=129,box_style="matlab");




