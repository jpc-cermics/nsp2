// -*- Mode: scilab -*- 
//
// test file for fsolve_lsq 

// 1-

m=3;
A=rand(m,3);
b=rand(m,1);

function y=f(x);  y=A*x-b; endfunction;
[xf,ff,info]=fsolve_lsq(ones(3,1),f,m=m);
if norm(A*xf-b) > 10*%eps then;pause;end

// 2-

m=10;
A=rand(m,3);
b=rand(m,1);

function y=f(x);  y=A*x-b; endfunction;
[xf,ff,info]=fsolve_lsq(ones(3,1),f,m=m);
if norm(xf - A\b) > 1.e-7 then pause;end 

// 3- 
// test the jacobian 

x0=ones(3,1);
[f1,j]=fsolve_lsq_jac(x0,f,m=m);

if norm(f1 - f(x0)) > 10*%eps then;pause;end
if norm(A -j) > 1.e-7 then;pause;end


// 4- 
// call with explicit jacobian 

function y=f(x);  y=A*x-b; endfunction;
function y=jac(x);  y=A; endfunction;

[xf,ff,info]=fsolve_lsq(ones(3,1),f,jac,m=m);
if norm(xf - A\b) > 1.e-7 then pause;end 
 

// 5- 
// The problem is to determine the values of x(1), x(2), and x(3)
// which provide the best fit (in the least squares sense) of

m = 15
u=(1:m)';
v=(m+1)-u;
w=min(u,v);
y =[ 0.14;0.18;0.22;0.25;0.29;0.32;0.35;0.39;
     0.37;0.58;0.73;0.96;1.34;2.10;4.39];

function z=f(x)
  z= y - (x(1) + u ./( v*x(2) + w*x(3)));
endfunction

function z=jac(x)
  z1=( v*x(2) + w*x(3));
  z1=z1.*z1; z1= u./z1;
  z=[ -ones(m,1), v.*z1, w.*z1];
endfunction

x0= ones(3,1);
[x,ff,info]=fsolve_lsq(x0,f,m=m);

if norm(x-[0.8241057D-01;0.1133037D+01;0.2343695D+01]) > 1.e-6 then pause;end

// with jac 

[x1,ff1,info1]=fsolve_lsq(x0,f,jac,m=m);

if norm(x-[0.8241057D-01;0.1133037D+01;0.2343695D+01]) > 1.e-6 then pause;end




