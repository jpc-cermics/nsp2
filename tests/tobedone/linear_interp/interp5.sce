// test which shows quadratic convergence
// for linear interpolation and various order
// for various spline
// Bruno (17 march 2005 - updated 3 April 2005)

function y=f(x),y = cos(x), endfunction

// interpolation interval
a = 0; b = %pi;

nn= [10 20 40 80 160 320];  // nb of interpolation points
m = 5033;                   // discretisation for error computing

xx = grand(m, 1, "unf", a, b);
yy = f(xx);
er = [];

for n = nn
   x = linspace(a,b,n)';
   y = f(x);
   el = max(abs(yy-linear_interpn(xx, x, y)));
   es4 = max(abs(yy-interp(xx,x,y,splin(x,y))));
   es5 = max(abs(yy-interp(xx,x,y,splin(x,y,"clamped",[0,0]))));
   es3 = max(abs(yy-interp(xx,x,y,splin(x,y,"fast"))));
   es2 = max(abs(yy-interp(xx,x,y,splin(x,y,"natural"))));
   er = [er ; [el es2 es3 es4 es5] ];
end

hh = (b-a)./(nn');
A = [ones(size(hh)) log(hh)]; coefs = A\log(er);
ylsl = exp(log(hh)*coefs(2,:) + ones(size(hh))*coefs(1,:));

xsetech(arect=[1/8,2/8,1/8,1/8]);
marks=9:-1:5;n = size(marks,'*');
plot2d(hh, er, mark=marks,line_color=-2*ones(1,n),mark_size=2*ones(1,n), logflag = "ll", ...
       leg="linear interp@natural spline@fast sub spline@not_a_knot spline@clamped spline",...
       leg_pos="ul");
leg = strcat("slope="+m2s(coefs(2,:),"%g"),"@");
plot2d(hh, ylsl, style=2:6, logflag = "ll",leg=leg,leg_pos="urm");
//xset("color",xget("foreground"))
xgrid();
xtitle("error function of h","h","e(h)");
xselect()
