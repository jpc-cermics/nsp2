polyedres = ["tetrahedron","cube","octahedron","dodecahedron","icosahedron"];

L = list();
angle = 2*%pi/5;
for i=1:5
   execstr("P="+polyedres(i)+"()");  
   P.coord = P.coord/norm(P.coord(:,1)); // try to normalize the sizes
   xt = 2*cos(i*angle);
   yt = 2*sin(i*angle);
   P.coord(1,:) = P.coord(1,:) + xt;
   P.coord(2,:) = P.coord(2,:) + yt;
   L(2*i-1) = tlist(["string3d","coord","str"],[xt;yt;1.2],polyedres(i),6,3);
   L(2*i) = P;
end

xset("default")
cm = xget("colormap");

xset("colormap",[cm;[0.92 0.92 0.92]]);

listObj=L;
// a coder en dur 
[Gmin, Gmax] = emboite(listObj)
ebox=matrix([Gmin';Gmax'],1,6);
draw3d_objs(listObj,ebox=ebox,flag=[0,5,2],box_color =33,box_style="other");





