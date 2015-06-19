n=10
xset('thickness',2);

for i=0:0.1:2
  xclear()
  x=sin(2*%pi*(0:n-1)/n);
  y=cos(2*%pi*(0:n-1)/n);
  xsetech(frect=[-2,-2,2,2]);
  xset("color",6)
  xpoly(x,y);
  xset("color",5)
  rect=[-2,2,3-i,3-i];
  xrect(rect)
  xpoly_clip(x,y,rect);
  xclick()
end

for i=0:0.1:3
  xclear()
  x=sin(2*%pi*(0:n-1)/n);
  y=cos(2*%pi*(0:n-1)/n);
  xsetech(frect=[-2,-2,2,2]);
  xset("color",6)
  xpoly(x,y);
  xset("color",5)
  rect=[-2+i,2-i,1,1];
  xrect(rect)
  xpoly_clip(x,y,rect);
  xclick()
end

