// -*- Mode: scilab -*- 
// basic test for optim 
// 

Leps=8.e-5;
bs=10.*ones(1,5);bi=-bs;x0=0.12*bs;epsx=1.e-15*x0;xopt=.1*bs;
[x]=optim('genros',x0);
if abs(norm(x-xopt)) > Leps then pause,end
[x]=optim('genros',x0,alg='gc');
if abs(norm(x-xopt) ) > Leps then pause,end
[x]=optim('genros',x0,alg='nd');
if abs(norm(x-xopt) ) > Leps then pause,end

function [f,g,ind]=genros(x,ind)
  n=size(x,'*');
  dzs(2)=100.0d+0;
  f=1.0d+0
  for i=2:n
    f=f + dzs(2)*(x(i)-x(i-1)**2)**2 + (1.0d+0-x(i))**2
  end
  g=ones(n,1);
  g(1)=-4.0d+0*dzs(2)*(x(2)-x(1)**2)*x(1)
  nm1=n-1
  for i=2:n-1
    ip1=i+1
    g(i)=2.0d+0*dzs(2)*(x(i)-x(i-1)**2)
    g(i)=g(i) -4.0d+0*dzs(2)*(x(ip1)-x(i)**2)*x(i) - 2.0d+0*(1.0d+0-x(i))
  end
  g(n)=2.0d+0*dzs(2)*(x(n)-x(nm1)**2) - 2.0d+0*(1.0d+0-x(n))
endfunction

[x]=optim(genros,x0);
if abs(norm(x-xopt)) > Leps then pause,end
[x]=optim(genros,x0,alg='gc');
if abs(norm(x-xopt) ) > Leps then pause,end
[x]=optim(genros,x0,alg='nd');
if abs(norm(x-xopt) ) > Leps then pause,end
