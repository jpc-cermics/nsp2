polyedres = ["tetrahedron","cube","octahedron","dodecahedron","icosahedron"];

cm = xget("colormap");
cmap= [cm;[0.92 0.92 0.92]];

fmode = %t; 
mode = "Cairo";
mode = "Gtk";
//mode = "OpenGl";
F=figure_create(wresize=%t,fname=mode,driver=mode,id=20);
// a top level axes 
A=objs3d_create(top=%t,title="Main title", colormap=cmap,box_style=0,box_color=33,alpha=56,theta=52);
// ,wrect=[0,0,1,1],frect=[0,-2,6,2],arect=[1,1,1,1]/12);
F.children(1)= A;
angle = 2*%pi/5;
for i=1:5
   execstr("P="+polyedres(i)+"()");  
   P.Mcoord = P.Mcoord/norm(P.Mcoord(1,:)); // try to normalize the sizes
   xt = 2*cos(i*angle);
   yt = 2*sin(i*angle);
   P.Mcoord(:,1) = P.Mcoord(:,1) + xt;
   P.Mcoord(:,2) = P.Mcoord(:,2) + yt;
   A.children($+1) = string3d_create(Mcoord=[xt;yt;1.2],str=polyedres(i));
   A.children($+1) = P;
end

F.connect[];





