// exec('macros/xdess/genfac3d.sci')

t=[-%pi/2,0,%pi/2]';
z=sin(t)*cos(t');
[xx,yy,zz]=genfac3d(t,t,z); 
col=[1,2,3,4;4,3,2,1;2,1,4,3;3,4,1,2]';
xclear();plot3d(xx,yy,zz,colors=col);

// FIXME: this is to be implemented with opengl 



