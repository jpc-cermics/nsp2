// plot3d and plot3d1 when x,y and z have the same size 
// illustrer le parametre color 
// fac3d et fac3d1 sont obsoletes 
// lispace est a rajouter quelque part 

if %f
  ncol=40;
  xset('colormap',hotcolormap(ncol));
else 
  ncol=xget('lastpattern");
end

function y = linspace(d1, d2, n)
  if nargin <= 2 then n = 100;end
  y = [d1*ones(1,n-1)+(0:n-2)*(d2-d1)./(n-1),d2];
endfunction

function [x,y,z]=sph(alp,tet)
  x=r*cos(alp).*cos(tet)+orig(1)*ones(tet);
  y=r*cos(alp).*sin(tet)+orig(2)*ones(tet);
  z=r*sin(alp)+orig(3)*ones(tet);
endfunction;
  
r=1;orig=[0 0 0];
[x1,y1,z1]=eval3dp(sph,linspace(-%pi./2,%pi./2,40),linspace(0,%pi*2,20));
[n1,m1]=size(x1);
r=1/2;orig=[-1 0 0];
[x2,y2,z2]=eval3dp(sph,linspace(-%pi./2,%pi./2,40),linspace(0,%pi*2,20));
[n2,m2]=size(x2);
x=[x1 x2];y=[y1 y2];z=[z1 z2];

// plusieurs facon de donner les couleurs sur des facettes 

xclear(); plot3d1(x,y,z)
[r,c]=size(z);

dist = (x-1).*(x-1)+(y-1).*(y-1)+(z-1).*(z-1);
colors= ncol*(dist./max(dist));
xclear();
plot3d(x,y,z,colors=colors) ;

z(:,1:50)=%nan;
xclear();plot3d(x,y,z,colors=colors(1,:)) ;
xclear(); plot3d(x,y,z,colors=colors) ;


