// -*- Mode: nsp -*- 

function z=f(x,y);z=cos(x+y);endfunction

[Ia,ea,ier] = int2d([0,1],[0,1],f); 
I = -cos(2)+2*cos(1)-1; // exact result  
if abs(Ia-I) > 1e-14 then, pause; end
X = [0,1;1,1;0,0];
Y = [0,0;0,1;1,1];
[Ia,ea,ier] = int2d(X,Y,f);
if abs(Ia-I) > 1e-14 then, pause; end


function z=f(x,y);z=cos(10*x+20*y);endfunction
I = (-cos(30)+cos(10)+cos(20)-1)/200; // exact result  
[Ia,ea,ier] = int2d([0,1],[0,1],f,limit=1000,vecteval=%t);
if abs(Ia-I) > 1e-14 then, pause; end

function z=f(x,y,a);z=cos(a*x+20*y);endfunction
[Ia,ea,ier] = int2d([0,1],[0,1],f,limit=1000,vecteval=%t,args={10});
if abs(Ia-I) > 1e-14 then, pause; end
[Ia,ea,ier] = int2d([0,1],[0,1],f,limit=1000,vecteval=%t,args=10);
if abs(Ia-I) > 1e-14 then, pause; end

function z=f(x,y,a,b);z=cos(a*x+b*y);endfunction
[Ia,ea,ier] = int2d([0,1],[0,1],f,limit=1000,vecteval=%t,args={10,20})
if abs(Ia-I) > 1e-14 then, pause; end


function z=f(x,y);z = 1./(x+y); endfunction
X = [0;1;0]; Y = [0;0;1];
[Ia,ea,ier] = int2d(X,Y,f,limit=1000,vecteval=%t); // exact result is 1
if abs(Ia-1) > 1e-12 then, pause; end

