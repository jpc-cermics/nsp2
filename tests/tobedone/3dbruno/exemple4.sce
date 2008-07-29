// exemple 4 : un peu comme un plot3d1
x = linspace(-1,1,80);
y = x;
z = (x.^2)'*ones(size(y)) - ones(size(x'))*y.^2;

z2 =  0.2*(x'*ones(size(y)) + ones(size(x'))*y);

P = zsurf_to_polyhedron(x, y, z, 1, 64, 0);

Q = zsurf_to_polyhedron(x, y, z2, 65, 128, 0);

cmap =  [jetcolormap(64) ; graycolormap(64) ; [0.9 0.9 0.9]];
xset("colormap", cmap);

// my_plot3d(list(P,Q), phi = %pi/3, theta = %pi/8, ...
// 	  with_box = %t, with_mesh = %f, ...
// 	  manip_rot = "spherique",...
// 	  title = "the painter is bad here",...
// 	  box_color = 129)

listObj=list(P,Q);
// a coder en dur 
[Gmin, Gmax] = emboite(listObj)
ebox=matrix([Gmin';Gmax'],1,6);
draw3d_objs(listObj,ebox=ebox,box_color=129,box_style="matlab");


