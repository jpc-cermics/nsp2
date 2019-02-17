// -*- Mode: nsp -*-
// intg_splin tests (bruno 08/01/2011)

function y=f(x), y = (x-1).*(x-0.5).*(x+1); endfunction 
function y=df(x), y = (x-1).*(x-0.5) + (x-0.5).*(x+1) + (x-1).*(x+1); endfunction 
function y=F(x), y = x.*(6 + x.*(-6 + x.*(-2 + 3*x)))/12; endfunction 

n = 10;
a = -1; b = 1;
x = linspace(a,b,n);
y = f(x);
d = df(x);

// integration on the complete spline domain
Ie = F(b) - F(a);
I = intg_splin(a,b,x,y,d);
if abs((I-Ie)/Ie) > 1e-15, then, pause, end

// integration inside the spline domain
a = -0.7; b = 0.5;
Ie = F(b) - F(a);
I = intg_splin(a,b,x,y,d);
if abs((I-Ie)/Ie) > 1e-15, then, pause, end

a = 0.34; b = -0.5;
Ie = F(b) - F(a);
I = intg_splin(a,b,x,y,d);
if abs((I-Ie)/Ie) > 1e-15, then, pause, end

// integration outside

a = -2; b = -0.5;

// the default is by_nan
I = intg_splin(a,b,x,y,d);
if ~isnan(I) then, pause, end

// use natural
I = intg_splin(a,b,x,y,d,outmode="natural");
Ie = F(b) - F(a);
if abs((I-Ie)/Ie) > 1e-15, then, pause, end

// use C0
I = intg_splin(a,b,x,y,d,outmode="C0");
Ie = (x(1)-a)*y(1) + F(b) - F(x(1));
if abs((I-Ie)/Ie) > 1e-15, then, pause, end

// use linear
I = intg_splin(a,b,x,y,d,outmode="linear");
ya = y(1) + (a-x(1))*d(1);
Ie = 0.5*(x(1)-a)*(ya+y(1)) + F(b) - F(x(1));
if abs((I-Ie)/Ie) > 1e-15, then, pause, end

// use periodic
I = intg_splin(a,b,x,y,d,outmode="periodic");
Ie = F(b) - F(x(1)) + F(x(n)) - F(x(n)-(x(1)-a));
if abs((I-Ie)/Ie) > 1e-15, then, pause, end
 
// other values for a and b
a = -1.5; b = 1.3;

// the default is by_nan
I = intg_splin(a,b,x,y,d);
if ~isnan(I) then, pause, end

// use natural
I = intg_splin(a,b,x,y,d,outmode="natural");
Ie = F(b) - F(a);
if abs((I-Ie)/Ie) > 4e-15, then, pause, end

// use C0
I = intg_splin(a,b,x,y,d,outmode="C0");
Ie = (x(1)-a)*y(1) +  (F(x(n))-F(x(1))) + (b-x(n))*y(n);
if abs((I-Ie)/Ie) > 1e-15, then, pause, end

// use linear
I = intg_splin(a,b,x,y,d,outmode="linear");
ya = y(1) + (a-x(1))*d(1);
yb = y(n) + (b-x(n))*d(n);
Ie = 0.5*(x(1)-a)*(ya+y(1)) + (F(x(n))-F(x(1))) +  0.5*(b-x(n))*(yb+y(n));
if abs((I-Ie)/Ie) > 1e-15, then, pause, end

// use periodic
I = intg_splin(a,b,x,y,d,outmode="periodic");
Ie = F(x(n))-F(x(1)) + (F(x(n))-F(x(n)-(x(1)-a))) + (F(x(1)+(b-x(n)))-F(x(1)));
if abs((I-Ie)/Ie) > 1e-15, then, pause, end
 
// periodic on a large interval
a = -1000; b = 123.5;
I = intg_splin(a,b,x,y,d,outmode="periodic");
Ie = 561*(F(1)-F(-1)) + (F(1)-F(0)) + (F(-0.5)-F(-1));
if abs((I-Ie)/Ie) > 1e-15, then, pause, end
