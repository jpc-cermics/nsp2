// exemple 2 : un peu comme un plot3d1
x = linspace(0,2*%pi,40);
z = cos(x')*cos(x);

P = zsurf_to_polyhedron(x, x, z, 1, 64);  // mise sous forme d'un objet "polyedre"
                           // la valeur en z la plus petite sera
			   // associée à la couleur 1 et la plus 
			   // grande à la couleur 64

xset("colormap", hotcolormap(64));

listObj=list(P);
[Gmin, Gmax] = emboite(listObj)
ebox=matrix([Gmin';Gmax'],1,6);

draw3d_objs(listObj,ebox=ebox,with_box=%t,with_mesh=%t);


