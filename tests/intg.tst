// -*- Mode: nsp -*- 

function y=f1(x);y=exp(x);endfunction;
[I,ea] = intg(0,1,f1);
if abs(I-(%e-1)) > 10*%eps then pause;end

[I,ea] = intg(0,1,f1, vecteval=%t);
if abs(I-(%e-1)) > 10*%eps then pause;end

function y=f2(x);y=2*exp(-x^2)/sqrt(%pi);endfunction;
[Ia,ea] = intg(0,0.5,f2);
I = erf(0.5);
if abs(I - Ia)  > 10*%eps then pause;end

function y=g(x,a);y=a*x;endfunction;
[Ia,ea] = intg(0,2*%pi,g,args={1});
if abs(Ia - 2*%pi^2) > 10*%eps then pause;end
[Ia,ea] = intg(0,2*%pi,g,args=1);
if abs(Ia - 2*%pi^2) > 10*%eps then pause;end

function y=g(x,a,w);y=a*x + sin(w*x);endfunction;
[Ia,ea] = intg(0,2*%pi,g,args={1,1});
if abs(Ia - 2*%pi^2) > 10*%eps then pause;end

function y=f(x);y = 2*x.*cos(1./x) + sin(1./x); endfunction;

[Ia,ea,ier] = intg(0,1,f, rtol=1e-7, limit=10000, vecteval=%t);
if abs(Ia -cos(1)) > 1.e-6 then pause;end


function y=f(x,p);y=pdf(p{1},x,p{2:$});endfunction

[Ia, ea, ier] = intg(-%inf, %inf, f, args={{"nor",0,1}});
if abs(Ia - 1) > 1.2e-8 then pause;end

[Ia, ea, ier] = intg(0, %inf, f, args={{"gam",3,1}});
if abs(Ia - 1) > 1.2e-8 then pause;end

[Ia, ea, ier] = intg(-%inf, %inf, f, args={{"cau",100}});
if abs(Ia - 1) > 1.2e-8 then pause;end

[Ia, ea, ier] = intg(-%inf, %inf, f, args={{"lap",1000}});
if abs(Ia - 1) > 1.2e-8 then pause;end

[Ia, ea, ier] = intg(0, 1, f, args={{"bet",2,6}});
if abs(Ia - 1) > 1.2e-8 then pause;end

function y=f(x);y = log(abs(x)); endfunction
[Ia,ea,ier] = intg(-1,1,f, sing=0);
if abs(Ia + 2) > 1e-14 then pause;end
