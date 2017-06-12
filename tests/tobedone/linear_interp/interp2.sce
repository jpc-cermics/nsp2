// test which shows extrapolation features
// for bilinear interpolation
// Bruno (17 march 2005)

nx = 20; ny = 30;
x = linspace(0,1,nx);
y = linspace(0,2, ny);
[X,Y] = ndgrid(x,y);
z = 0.4*cos(2*%pi*X).*cos(%pi*Y);

nxp = 60 ; nyp = 120;
xp = linspace(-0.5,1.5, nxp);
yp = linspace(-0.5,2.5, nyp);
[XP,YP] = ndgrid(xp,yp);

zp1 = linear_interpn(XP, YP, x, y, z, "natural");
zp2 = linear_interpn(XP, YP, x, y, z, "periodic");
zp3 = linear_interpn(XP, YP, x, y, z, "C0");
zp4 = linear_interpn(XP, YP, x, y, z, "by_zero");
zp5 = linear_interpn(XP, YP, x, y, z, "by_nan");

xbasc()
subplot(2,3,1,a3d=%t)
plot3d1(x, y, z, iso=%t, colormap=jetcolormap(32),mesh=%f,shade=%f);
xtitle("Fonction to interpolate")

subplot(2,3,2,a3d=%t)
plot3d1(xp, yp, zp1, iso=%t, colormap=jetcolormap(32),mesh=%f,shade=%f);
xtitle("Natural")

subplot(2,3,3,a3d=%t)
plot3d1(xp, yp, zp2, iso=%t, colormap=jetcolormap(32),mesh=%f,shade=%f);
xtitle("Periodic")

subplot(2,3,4,a3d=%t)
plot3d1(xp, yp, zp3, iso=%t, colormap=jetcolormap(32),mesh=%f,shade=%f);
xtitle("C0")

subplot(2,3,5,a3d=%t)
plot3d1(xp, yp, zp4, iso=%t, colormap=jetcolormap(32),mesh=%f,shade=%f);
xtitle("by_zero")

subplot(2,3,6,a3d=%t)
plot3d1(xp, yp, zp5, iso=%t, colormap=jetcolormap(32),mesh=%f,shade=%f);
xtitle("by_nan")
