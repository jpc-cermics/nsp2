// test which shows extrapolation features
// for linear interpolation
// Bruno (17 march 2005)

// function to interpolate
function y=f(x),y=x.*(1-x),endfunction

// interpolation points
x = linspace(0,1,6)';
y = f(x);

// evaluation points (some outside the interpolation domain)
xsetech(arect=[1/8,2/8,1/8,1/8],frect=[-0.5,-0.3,1.5,0.3],axesflag=1);
xx = linspace(-0.3,1.3,401)';
yy = {};
stype=["C0","natural","periodic","by_zero","by_nan"];
for i=1:size(stype,'*')
  yy = linear_interpn(xx,x,y,stype(i));
  plot2d(xx,yy,line_color=i+1,line_thickness=2,leg=stype(i),leg_pos="urm");
end
plot2d(x,y,mark=9,line_color=-2,mark_size=2,leg="interpolation points", leg_pos="urm")
xtitle(" different ways to evaluate outside the domain")
