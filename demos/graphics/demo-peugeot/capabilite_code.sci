function partie(f,xmin,xmax,couleur)
  x=linspace(xmin,xmax,100);
  y=f(x);
  x=[x,xmax,xmin];y=[y,0,0];
  xfpoly(x',y',couleur);
endfunction

histplot([-6:0.4:6],randn(1,2000),rect=[-6,0,6,0.5])
function [y]=f(x) ; y=exp(-x.*x/2)/sqrt(2*%pi);endfunction 
x=-6:0.1:6;x=x';
y=f(x);
plot2d(x,y,style=2,strf="000");
titre= 'macro histplot : Histogram plot';
xtitle(titre,'Classes','N(C)/Nmax');
xleft=-1;
partie(f,min(x),xleft,15);
rect=xstringl(0,0,'A')
xpoly(xleft*[1;1],[0;0.3]);xstring(xleft-rect(3),0.3,'A');
xright=2;
partie(f,2,max(x),12);
xpoly(xright*[1;1],[0;0.3]);xstring(xright,0.3,'B');
////xs2ps(0,'capabilite.ps');
////unix(SCI+'/bin/Blatexpr -p 1 1 capabilite.ps')  

					    
