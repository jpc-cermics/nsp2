// test which shows quadratic convergence
// for bilinear interpolation
// Bruno (17 march 2005)


function z=f(x,y),z = cos(x).*sin(4*y), endfunction

a = 0; b = 2*%pi;  
c = 0; d = b;

function [a,b] = least_squares_line(x,y)
   A = [ones(x) x];
   M = A'*A
   v = A'*y
   // Mx=v par Cramer... (waiting x=A\b)
   d = M(1,1)*M(2,2) - M(2,1)*M(1,2)
   b =(v(1)*M(2,2) - v(2)*M(1,2))/d
   a =(M(1,1)*v(2) - M(2,1)*v(1))/d
endfunction

nn= [10 20 40 80 160 320];  // nb of interpolation points
m = 50000;                     // discretisation for error computing

xx = grand(m, 1, "unf", a, b);
yy = grand(m, 1, "unf", c, d);
zz = f(xx,yy);
el = [];

for n = nn
   x = linspace(a,b,n);
   y = linspace(c,d,n);   
   [X,Y] = ndgrid(x,y);
   Z = f(X,Y);
   zzl = linear_interpn(xx, yy, x, y, Z);
   el = [el ; max(abs(zz - zzl))];         
end

xlfont("-adobe-helvetica-medium-r-normal--*-%s0-*-*-p-*-iso8859-1",6)
xbasc()
hh = max(b-a,d-c)./(nn'); 
[s,q] = least_squares_line(log(hh),log(el));
ylsl = exp(s*log(hh) + q);
xset("font",6,1)
plot2d(hh, [el ylsl], style=[-9 5], strf="121", logflag = "ll", ...
       leg="linear interp error@least square line",...
       leg_pos="ul")
xset("font",6,2)
xtitle("error function of h","h","e(h)")
xselect()

printf("\n\r slope in log/log (must be 2) = %g \n\r",s); 

