
x=linspace(0,100,100)';
y= rand(1,100);
plot2d(x,y,line_color=2,mark=15);
Isup=find(y>=0.9);
plot2d(x(Isup),y(Isup),line_color=-2,mark_color=4,mark=15);
Iinf=find(y<=0.1);
plot2d(x(Iinf),y(Iinf),line_color=-2,mark_color=5,mark=15);
// horizontal lines 
plot2d([min(x);max(x)]*[1,1],[1;1]*[0.9,0.1],style=[3,3]);
xtitle('Depassement de seuils','Temps','Notes');



