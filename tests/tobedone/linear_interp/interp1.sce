// test which shows extrapolation features
// for linear interpolation
// Bruno (17 march 2005)

// function to interpolate
function y=f(x),y=x.*(1-x),endfunction

// interpolation points
x = linspace(0,1,6)';
y = f(x);

// evaluation points (some outside the interpolation domain)
xx = linspace(-0.3,1.3,401)';
yy0 = linear_interpn(xx,x,y,"C0");
yy1 = linear_interpn(xx,x,y,"natural");
yy2 = linear_interpn(xx,x,y,"periodic");
yy3 = linear_interpn(xx,x,y,"by_zero");
yy4 = linear_interpn(xx,x,y,"by_nan");
yy5 = f(xx);

xlfont("-adobe-helvetica-medium-r-normal--*-%s0-*-*-p-*-iso8859-1",6)
xbasc()
xset("font",6,1)
plot2d(xx,[yy0 yy1 yy2 yy3 yy4 yy5],style=2:7,...
       frameflag=2,leg_pos="dl",...
       leg="C0@natural@periodic@by_zero@by_nan@exact function")
plot2d(x,y,style=-9,leg="interpolation points", leg_pos="dr")
xset("font",6,2)
xtitle(" different ways to evaluate outside the domain")
