
ncol=32;
cmap= [jetcolormap(ncol);[0.92 0.92 0.92]];

fmode = %t; 
mode = "Cairo";
mode = "Gtk";
mode = "OpenGl";
F=figure_create(wresize=%t,fname=mode,driver=mode,id=20);
// a top level axes 
A=objs3d_create(top=%t,title="Main title", colormap=cmap,box_style=0,box_color=33,alpha=56,theta=52);
// ,wrect=[0,0,1,1],frect=[0,-2,6,2],arect=[1,1,1,1]/12);

F.children(1)= A;
P = dodecahedron();
// P = tetrahedron();

val = P.Mcoord(:,3)';
color_low=1,color_high= ncol;
Ps=spolyhedron_create(Mcoord=P.Mcoord,Mface=P.Mface,Mval = val,...
		      vmin = min(val), vmax=max(val),...
		      coloutmin= color_low, coloutmax= color_high,...
		      colmin = color_low, colmax = color_high,...
		      back_color=1, mesh=%t);

A.children($+1) = Ps;
F.connect[];








