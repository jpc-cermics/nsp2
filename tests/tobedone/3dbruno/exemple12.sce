D = dino_steph_nsp();

xmin = min(D.coord(1,:)); xmax = max(D.coord(1,:)); 
ymin = min(D.coord(2,:)); ymax = max(D.coord(2,:)); 
zmin = min(D.coord(3,:)); zmax = max(D.coord(3,:)); 

posT = [0.5*(xmin+xmax);...
	0.5*(ymin+ymax);...
	zmax + 0.1*(zmax-zmin)];

posH = [35;...
	ymin - 0.2*(ymax-ymin);...
	-130];

T = tlist(["string3d","coord","str"],posT,"Top",8,3);
H = tlist(["string3d","coord","str"],posH,"He, I am Dino",8,3);

xset("colormap",[jetcolormap(128);[0.92 0.92 0.92]]);

listObj=list(D,T,H);
// a coder en dur 
[Gmin, Gmax] = emboite(listObj)
ebox=matrix([Gmin';Gmax'],1,6);
draw3d_objs(listObj,ebox=ebox,flag=[0,5,2],box_color =129,box_style="other");


