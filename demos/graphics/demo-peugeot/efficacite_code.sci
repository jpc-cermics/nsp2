//driver('Rec');

function y=f(x,a)
  y= atan((x-a)/a);
endfunction

x=linspace(-100,100,100)';
A=[1,5,10,15,20];
n=size(A,'*');
y=ones_new(100,n);
for i=1:n; y(:,i)= f(x,A(i));end 
leg=catenate(string(A),sep='@');
plot2d(x,y,leg=leg,leg_pos="urm");
//a=gca();
//a.grid=[2,2];
xgrid()
xtitle('Courbe d''efficacité','CAP reel','Pa');
////xs2ps(0,'efficacite.ps');
//unix(SCI+'/bin/Blatexpr -p 1 1 efficacite.ps')  /// \sleftarrow{calling a Unix script}
