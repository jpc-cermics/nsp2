// exemple 2b : un peu comme un plot3d1

function [z] = rozen(x,y)
   // pour plot3d
   C = 1
   x = x(:)
   y = (y(:))'
   z = C*(ones(x)*y - x.^2*ones(y)).^2 + (1 - x).^2*ones(y)
endfunction

n = 20;
x = linspace(-1.5,2,n);
y = linspace(-2,3,n);
z = rozen(x,y);
// on limite l'amplitude en z
z = 3*z/max(z);

P = zsurf_to_spolyhedron(x, x, z, 1, 128); 

xset("colormap", [jetcolormap(128);[0.9 0.9 0.9]]);

listObj=list(P);
// a coder en dur 
[Gmin, Gmax] = emboite(listObj)
ebox=matrix([Gmin';Gmax'],1,6);
draw3d_objs(listObj,ebox=ebox,box_color=129,box_style="matlab");





