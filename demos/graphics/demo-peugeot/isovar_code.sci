//driver('Rec')

x=linspace(-%pi,%pi,40);
y=linspace(-1,1,40);

function [z]=surf(x,y)
  z=sin(2*x).*y+x.*y;
endfunction

z=eval3d(surf,x,y);

//set old_style on /// \sleftarrow{Bug en nouveau graphique}
xset('font size',3);
xsetech(wrect=[0,0,1,1/2]);
contour(x,y,z,[0.6,0.2,-0.2,-0.6]);
xsetech(wrect=[0,1/2,1,1/2]);
plot3d(x,y,z);

