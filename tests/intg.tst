// -*- Mode: scilab -*- 

function y=f1(x);y=exp(x);endfunction;
[I,ea] = intg(0,1,f1);
if abs(I-(%e-1)) > 10*%eps then pause;end

[I,ea] = intg(0,1,f1, vecteval=%t);
if abs(I-(%e-1)) > 10*%eps then pause;end

function y=f2(x);y=2*exp(-x^2)/sqrt(%pi);endfunction;
[Ia,ea] = intg(0,0.5,f2);
I = erf(0.5);
if abs(I - Ia)  > 10*%eps then pause;end

function y=g(x,p);a=p(1);w=p(2);y=a*x + sin(w*x);endfunction;
[Ia,ea] = intg(0,2*%pi,g,args=[1,1]);
if abs(Ia - 2*%pi^2) > 10*%eps then pause;end

[Ia,ea] = intg(0,2*%pi,g,args=list(1,1));
if abs(Ia - 2*%pi^2) > 10*%eps then pause;end

function y=g(x,p);a=p{1};b=p{2};y=b*x + sin(b*x);endfunction;
[Ia,ea] = intg(0,2*%pi,g,args={1,1});
if abs(Ia - 2*%pi^2) > 10*%eps then pause;end

function y=g(x,p);y=p.a*x + sin(p.w*x);endfunction;
[Ia,ea] = intg(0,2*%pi,g,args=hash(2,a=1,w=1));
if abs(Ia - 2*%pi^2) > 10*%eps then pause;end

function y=f(x);y = 2*x.*cos(1./x) + sin(1./x); endfunction;

//[Ia,ea,ier] = intg(0,1,f, rtol=1e-7, limit=10000);
[Ia,ea,ier] = intg(0,1,f, rtol=1e-7, limit=10000, vecteval=%t);
if abs(Ia -cos(1)) > 1.e-6 then pause;end



