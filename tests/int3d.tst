// -*- Mode: scilab -*- 

function v=f(x,y,z);v=cos(x+y+z);endfunction
[Ia,ea] = int3d([0,1],[0,1],[0,1],f);
I = -sin(3) + 3*(sin(2)-sin(1)); // exact
if abs(I-Ia) > 1e-11 then, pause;end

X = [0;1;0;0];
Y = [0;0;1;0];
Z = [0;0;0;1];
function v=f(x,y,z);v=1-x-y-z;endfunction
[Ia,ea] = int3d(X,Y,Z,f);
if abs(Ia - 1/24) > 10*%eps  then, pause;end

 

function v=foo(x,y,z,alpha);
   v = 1./(1 + alpha(1)*x+alpha(2)*y+alpha(3)*z).^4;
endfunction
// the 3d unit simplex
X = [0;1;0;0];
Y = [0;0;1;0];
Z = [0;0;0;1];

alpha = [3,13,9];
I = 1/(6*(1+alpha(1))*(1+alpha(2))*(1+alpha(3)))
[Ia,ea] = int3d(X,Y,Z,foo,args=alpha)
if abs(I - Ia) > 1e-12 then, pause, end

[Ia,ea] = int3d(X,Y,Z,foo,args=alpha,vecteval=%t)
if abs(I - Ia) > 1e-12 then, pause, end


