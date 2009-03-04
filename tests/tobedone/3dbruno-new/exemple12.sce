
D = dino_steph_nsp();

// This should be given by a method 

xmin = min(D.Mcoord(1,:)); xmax = max(D.Mcoord(1,:)); 
ymin = min(D.Mcoord(2,:)); ymax = max(D.Mcoord(2,:)); 
zmin = min(D.Mcoord(3,:)); zmax = max(D.Mcoord(3,:)); 

posT = [0.5*(xmin+xmax);...
	0.5*(ymin+ymax);...
	zmax + 0.1*(zmax-zmin)];

posH = [35;...
	ymin - 0.2*(ymax-ymin);...
	-130];

T = string3d_create(Mcoord=posT,str="Top");
H = string3d_create(Mcoord=posH,str="He, I am Dino");

cmap = [jetcolormap(128);[0.92 0.92 0.92]];

fmode = %t; 
mode = "Cairo";
mode = "Gtk";
//mode = "OpenGl";
F=figure_create(wresize=%t,fname=mode,driver=mode,id=20);
// a top level axes 
A=objs3d_create(top=%t,title="Main title", colormap=cmap,box_style=0,box_color=129,alpha=56,theta=52);
// ,wrect=[0,0,1,1],frect=[0,-2,6,2],arect=[1,1,1,1]/12);
F.children(1)= A;
A.children($+1) = D;
A.children($+1) = T;
A.children($+1) = H;

F.connect[];




