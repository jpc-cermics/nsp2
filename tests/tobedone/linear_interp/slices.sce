// this script displays a 3d func by showing some slices.
// To test:  slice_parallelepipe
//           nf3d (new version)
//           colorbar
// Bruno (17 march 2005)

func =  "v=(x-0.5).^2 + (y-0.5).^3 + (z-0.5).^2";
function v=f(x,y,z)
   v=(x-0.5).^2 + (y-0.5).^3 + (z-0.5).^2
endfunction

mx = 60; my = 90; mz = 60;
dir = ["z=", "z=", "z=", "y=", "y=", "y=", "y="];
val = [ 0.1,  0.5,  0.9, -0.1,  0.3,  0.7,  1.1];
ebox = [0, 1, -0.2, 1.3, 0, 1];

XF=[]; YF=[]; ZF=[]; VF=[];
for i = 1:size(val,"*")
   [X,Y,Z] = slice_parallelepiped(dir(i), val(i), ebox, mx, my, mz);
   V = f(X,Y,Z);
   [xf,yf,zf,vf] = nf3d(X,Y,Z,V);
   XF = [XF xf]; YF = [YF yf]; ZF = [ZF zf]; VF = [VF vf]; 
end

nb_col = 128;
vmin = min(VF); vmax = max(VF);
color = bsearch(VF,linspace(vmin,vmax,nb_col+1));

//xlfont("-adobe-helvetica-medium-r-normal--*-%s0-*-*-p-*-iso8859-1",6)
xbasc();
xset("colormap",jetcolormap(nb_col));
xset("hidden3d",0)
//xset("font",6,1)
xset("font size",1)
colorbar(vmin,vmax)
plot3d(XF, YF, ZF, colors=-color, flag=[-1 6 4], alpha=75, theta=20)
//xset("font",6,2)
xset("font size",2)
xtitle("some slices of "+func)
//xset("font",6,1)
xset("font size",1)
xselect()
