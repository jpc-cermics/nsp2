// plot3d and plot3d1 when x,y and z have the same size 
// illustrer le parametre color 
// fac3d et fac3d1 sont obsoletes 
// lispace est a rajouter quelque part 

// FIXME : this is a temporary load 
// of all the graphics macros 
NSP=getenv('SCI');

if %t then 
  A=glob(NSP+'/macros/xdess/*.sci');
  for i=1:size(A,1),exec(A(i,1));end;
  A=glob(NSP+'/macros/scicos/00util.sci');
  exec(A)
end 

function y = linspace(d1, d2, n)
  if nargin <= 2 then n = 100;end
  y = [d1*ones(1,n-1)+(0:n-2)*(d2-d1)./(n-1),d2];
endfunction

function [x,y,z]=rings(alp,tet,a,b)
  x=(a+b*cos(alp)).*cos(tet)
  y=(a+b*cos(alp)).*sin(tet)
  z=(a+b*sin(alp)).*ones(tet);
endfunction;

// the tube 
b=0.4;
alpha=linspace(-%pi,%pi,10);
theta=linspace(0,%pi*2,40);

function [r]=curve(alpha,theta)
  r = 2+cos(4*(theta)).*ones(alpha)
endfunction

m=size(alpha,'*');n=size(theta,'*');
alpha1=matrix(ones(1,n).*.matrix(alpha,1,m),m,n);
theta1=matrix(matrix(theta,1,n).*.ones(1,m),m,n);
rr=eval3d(curve,alpha,theta);
[xx,yy,zz]=rings(alpha1,theta1,rr,b);
[xx1,yy1,zz1]=nf3d(xx,yy,zz);
plot3d(xx1,yy1,zz1);

