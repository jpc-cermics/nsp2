//driver('Rec')

xclear();
xsetech(wrect=[0,0,1,1/5],frect=[0,0,1,1])
xset('font',2);
xstringb(0,0.0,'Nuages de points',1,1);
h = (1-1/5);
xsetech(wrect=[0,1/5,1/2,h]);
x=1:1000;
y=randn(1,1000);
plot2d(x,y,style=0);
xsetech(wrect=[1/2,1/5,1/2,h]);
x=randn(1000,1);
y=randn(1000,1);
col=xget('color');
xset('color',5);
plot2d(x,y,style=-2,axesflag=0);
x=randn(1000,1)+2;
y=randn(1000,1);
xset('color',9);
plot2d(x,y,style=-4,axesflag=0);
xset('color',col);
plot2d([],[],strf='001');

////xs2ps(0,'nuages.ps');
//unix(SCI+'/bin/Blatexpr -p 1 1 nuages.ps')  /// \sleftarrow{calling a Unix script}
