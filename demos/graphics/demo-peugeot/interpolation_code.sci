x=linspace(0,5,20);
sig=1;
y= 2*x+ 8 + sig*grand(x,'nor',0,1);
//[a,b,sig]=reglin(x,y) ;
a=2.1; b=7.45;
plot2d([x;x]',[y;a*x+b]',style=[-2,2],leg=['data@estimated'],leg_pos="dr")
//////xs2ps(0,'interpolation.ps');

					    
