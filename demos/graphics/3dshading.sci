// exec('macros/xdess/genfac3d.sci')

// minimal 

xclear();plot3d([0,1,0]',[0,0,1]',[1,2,3]',colors=[1,2,3]')

// plus compliqué 

t=[-%pi/2,0,%pi/2]';
z=sin(t)*cos(t');
[xx,yy,zz]=genfac3d(t,t,z); 
col=[1,2,1
     2,3,2
     1,2,1]
[xx,yy,zzcol]=genfac3d(t,t,col); 
xclear();plot3d(xx,yy,zz,colors=zzcol);

// Noter que l'interpolated shading quand on n'utilise pas opengl
// est base sur le fait 
// que plus les indices des couleurs sont eloignes plus 
// les couleurs sont lointaines 
// ==> ca n'a reellement de sens que pour un colormap 
// progressif type hotcolormap 

col=[1,9,1
     9,18,9
     1,9,1]
[xx,yy,zzcol]=genfac3d(t,t,col); 
xclear();plot3d(xx,yy,zz,colors=zzcol);

// Astuce: On peut aussi utiliser genfac3d pour trouver les bonnes 
// couleurs a partir des couleurs des sommets en z 

t=[0:0.3:2*%pi]'; z=sin(t)*cos(t');
[xx,yy,zzcolors]=genfac3d(t,t,10*rand(z));
[xx,yy,zz]=genfac3d(t,t,z);
xclear();plot3d1(xx,yy,zz,colors=zzcolors)

// avec contour des polygones 

t=[0:0.3:2*%pi]'; z=sin(t)*cos(t');
[xx,yy,zzcolors]=genfac3d(t,t,10*(z+2));
[xx,yy,zz]=genfac3d(t,t,z);
xclear();plot3d1(xx,yy,zz,colors=zzcolors)

// interpolated shading sur les altitudes 
// sans bord des polygones 
// mais il y a un pb c'est qu'on voit aussi les 
// bords des sous polygones d'approx linéaire. 

t=[0:0.3:2*%pi]'; z=sin(t)*cos(t');
[xx,yy,zzcolors]=genfac3d(t,t,10*(z+2));
[xx,yy,zz]=genfac3d(t,t,z);
xclear();plot3d1(xx,yy,zz,colors=-zzcolors)

// meme chose avec un hotcolormap 

exec('macros/xdess/hotcolormap.sci');

xset("colormap",hotcolormap(32));


