//a=gca();
//a.font_size=2;
xset('font size',2);
x=linspace(0,100,100)';
y= randn(1,100);
plot2d(x,y,style=2);
xset('mark size',3)
plot2d(x,y,style=-2,strf='000');
Isup=find(y>=2);
xset('color',4);
plot2d(x(Isup),y(Isup),style=-3,strf='000');

Iinf=find(y<=2);
xset('color',5);
plot2d(x(Iinf),y(Iinf),style=-5,strf='000');

xtitle('','','');
//////xs2ps(0,'seuil.ps');
////unix(SCI+'/bin/Blatexpr -p 1 1 seuil.ps')

