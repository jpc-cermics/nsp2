xclear()
xset('font size',3);
x=0:0.1:2*%pi;plot2d([x]',[sin(x)]',leg='sin(x)',strf='132')
for A=[0:30:360];xstring(4.5+0.5*cos(-%pi*A/180),0.5+0.5*sin(-%pi*A/180),'Nsp',A,0);end
for A=[0:30:360];xsegs([4.5;4.5+0.5*cos(-%pi*A/180)],[0.5;0.5+0.5*sin(-%pi*A/180)]);end
xtitle('nsp plot2d','Time','Sin(t)')

