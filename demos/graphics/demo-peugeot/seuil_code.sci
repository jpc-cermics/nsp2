//a=gca();
//a.font_size=2;
xset('font size',2);
x=linspace(0,100,100)';
y= rand(1,100,'u');
plot2d(x,y,style=2);
xset('mark size',3)
plot2d(x,y,style=-2,strf='000');
Isup=find(y>=0.9);
xset('color',4);
plot2d(x(Isup),y(Isup),style=-3,strf='000');
Iinf=find(y<=0.1);
xset('color',5);
plot2d(x(Iinf),y(Iinf),style=-5,strf='000');
// horizontal lines 
plot2d([min(x);max(x)]*[1,1],[1;1]*[0.9,0.1],style=[3,3],strf='000');
// frame in black 
xset('color',0);
plot2d([],[],style=0,strf='001');
xtitle('Depassement de seuils','Temps','Notes');

////xs2ps(0,'seuil.ps');
//unix(SCI+'/bin/Blatexpr -p 1 1 seuil.ps')

