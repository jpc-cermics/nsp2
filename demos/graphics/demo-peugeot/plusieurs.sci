//driver('Rec')

xclear();
xsetech(wrect=[0,0,1,1/5],frect=[0,0,1,1])
xset('font',2);
xstringb(0,0.0,'Titre principal',1,1);
h = (1-1/5)/2;
xsetech(wrect=[0,1/5,1/2,h]);
histplot()
//a=gca();
//a.box="off";
xsetech(wrect=[1/2,1/5,1/2,h]);
histplot()
//a=gca();
//a.box="off";
xsetech(wrect=[0,h+1/5,1/2,h]);
histplot()
//a=gca();
//a.box="off";
xsetech(wrect=[1/2,h+1/5,1/2,h]);
sig=2.34;
m=0.0;
y=sig*rand(1,200,'n');
histplot(20,y,rect=[-4,0,4,0.3]);
function [y]=f(x,m,sig) 
  y=exp(-(x-m).*(x-m)/(2*sig.^2))/(sqrt(2*%pi)*sig);
endfunction
x=-6:0.1:6;x=x';plot2d(x,f(x,m,sig),style=1,strf="000");
titre= 'macro histplot : Histogram plot';
xtitle(titre,'Classes','N(C)/Nmax');

//xs2ps(0,'densites.ps');
//unix(SCI+'/bin/Blatexpr -p 1 1 densites.ps')  /// \sleftarrow{calling a Unix script}
