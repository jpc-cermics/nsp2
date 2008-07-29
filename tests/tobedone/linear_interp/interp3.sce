// test which shows quadratic convergence
// for linear interpolation
// Bruno (17 march 2005)

function y=f(x),y = cos(x), endfunction

// interpolation interval
a = 0; b = %pi;              


function [a,b] = least_squares_line(x,y)
   A = [ones(size(x)) x];
   M = A'*A
   v = A'*y
   // Mx=v par Cramer... (waiting for x=A\b)
   d = M(1,1)*M(2,2) - M(2,1)*M(1,2)
   b =(v(1)*M(2,2) - v(2)*M(1,2))/d
   a =(M(1,1)*v(2) - M(2,1)*v(1))/d
endfunction

nn= [10 20 40 80 160 320];  // nb of interpolation points
m = 5000;                   // discretisation for error computing

xx = grand(m, 1, "unf", a, b);
yy = f(xx);
el = [];

for n = nn
   x = linspace(a,b,n)';
   y = f(x);
   yyl = linear_interpn(xx, x, y);
   el = [el ; max(abs(yy - yyl))];         
end

//xlfont("-adobe-helvetica-medium-r-normal--*-%s0-*-*-p-*-iso8859-1",6)
xbasc()
hh = (b-a)./(nn');
[s,q] = least_squares_line(log(hh),log(el));
ylsl = exp(s*log(hh) + q);
//xset("font",6,1)
xset("font size",1)
plot2d(hh, [el ylsl], style=[-9 5], strf="121", logflag = "ll", ...
       leg="linear interp error@least square line",...
       leg_pos="ul")
xset("color",xget("foreground"))
//xset("font",6,2)
xset("font size",2)
xtitle("error function of h","h","e(h)")
xselect()

printf("\n\r slope in log/log (must be 2) = %g \n\r",s); 

