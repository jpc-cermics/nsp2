// exemple 6 : l'helice torique
nu = 120;
nv = 30;
u = linspace(0, 6*%pi, nu);
v = linspace(0, 2*%pi, nv);

P = psurf_to_spolyhedron(u, v, helice_torique, 1, 64);

xset("colormap", [hotcolormap(64); [0.8 0.9 1]]);

// my_plot3d(list(P), phi = %pi/3, theta = %pi/8, ...
// 	  title = "Helice torique",...
// 	  with_box = %t, with_mesh = %t, ...
// 	  box_color = 65, manip_rot = "generale")

listObj=list(P);
// a coder en dur 
[Gmin, Gmax] = emboite(listObj)
ebox=matrix([Gmin';Gmax'],1,6);
draw3d_objs(listObj,ebox=ebox,with_mesh = %t,box_color = 65,box_style="other")



