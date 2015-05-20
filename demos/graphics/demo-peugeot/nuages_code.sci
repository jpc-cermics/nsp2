// colored marks 

xsetech(wrect=[0,0,1,1/5],frect=[0,0,1,1],axesflag=0)
xstringb(0,0.0,'Nuages de points',1,1);

h = (1-1/5);
xsetech(wrect=[0,1/5,1/2,h]);
N=1000;
x=randn(N,1);y=randn(N,1);
plot2d(x,y,mark=0,line_color=-2,axesflag=5);

xsetech(wrect=[1/2,1/5,1/2,h],clip=%t);
x=randn(N,1);y=randn(N,1);
plot2d(x,y,line_color=-2,mark=8,mark_color=5,mark_size=2,axesflag=5);
x=randn(N,1)+2;y=randn(N,1);
plot2d(x,y,line_color=-2,mark=9,mark_color=9,mark_size=2,axesflag=5);


