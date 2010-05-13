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
//xset("font",2,2)
//xset("font size",2)
subplot(2,3,1)
//plot3d(x, y, z, leg="x@y@z", flag = [2 4 4])
plot3d(x, y, z, leg="", flag = [2 4 0])
//xtitle("Fonction to interpolate")

subplot(2,3,2)
//plot3d(xp, yp, zp1, leg="x@y@z", flag = [2 4 4])
plot3d(xp, yp, zp1, leg="", flag = [2 4 0])
//xtitle("Natural")

subplot(2,3,3)
//plot3d(xp, yp, zp2, leg="x@y@z", flag = [2 4 4])
plot3d(xp, yp, zp2, leg="", flag = [2 4 0])
//xtitle("Periodic")

subplot(2,3,4)
//plot3d(xp, yp, zp3, leg="x@y@z", flag = [2 4 4])
plot3d(xp, yp, zp3, leg="", flag = [2 4 0])
//xtitle("C0")


subplot(2,3,5)
//plot3d(xp, yp, zp4, leg="x@y@z", flag = [2 4 4])
plot3d(xp, yp, zp4, leg="", flag = [2 4 0])
//xtitle("by_zero")

subplot(2,3,6)
//plot3d(xp, yp, zp5, leg="x@y@z", flag = [2 4 4])
plot3d(xp, yp, zp5, leg="", flag = [2 4 0])
//xtitle("by_nan")



