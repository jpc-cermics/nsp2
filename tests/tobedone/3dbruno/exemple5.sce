// exemple 5 : un tore bossele et une sphere
nu = 120;
nv = 60;
u = linspace(0, 2*%pi, nu);
v = linspace(0, 2*%pi, nv);

P = psurf_to_spolyhedron(u, v, tore_bossele, 1, 128,2);
//P.val = grand(P.val,"unf",P.valminmax(1),P.valminmax(2));

nu = 60;
nv = 30;
u = linspace(0, 2*%pi, nu);
v = linspace(0, %pi, nv);

Q = sphere_ter([0;0;0],0.5,3, 129, 256, 0);
//Q.val = grand(Q.val,"unf",Q.valminmax(1),Q.valminmax(2));

xset("colormap", [hotcolormap(128) ; jetcolormap(128); [0.9 0.9 0.9]]);

// my_plot3d(list(P,Q), phi = %pi/3, theta = %pi/8, ...
// 	  title = "Tore et sphere",...
// 	  with_box = %t, with_mesh = %f, ...
// 	  manip_rot = "spherique", box_color=257 )

listObj=list(P,Q);
// a coder en dur 
[Gmin, Gmax] = emboite(listObj)
ebox=matrix([Gmin';Gmax'],1,6);
draw3d_objs(listObj,ebox=ebox, box_color=257,box_style="matlab");


