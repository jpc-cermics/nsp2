
n=5;
p=4;
val=10*rand(n,p);
nomsh='nh'+ string(1:p);
nomsv='nv '+ string(1:n)';

function mat_show(nomsh,nomsv,val)
  x=1:p+1+1;
  y=1:n+1+1;
  xsetech(frect=[min(x),min(y),max(x),max(y)],axesflag=0,clip=%f);
  xp=[x;x]; yp=[min(y)*ones(size(x));max(y)*ones(size(x))];
  xpolys(xp,yp);
  yp=[y;y]; xp=[min(x)*ones(size(y));max(x)*ones(size(y))];
  xpolys(xp,yp);
  S= [ '', nomsh; [ nomsv, string(val)]];
  for i=1:n+1
    for j=1:p+1
      xstringb(x(j)+0.1,y(n+1-i+1)+0.1,S(i,j),0.8,0.8,'fill');
    end
  end
endfunction

//driver('Rec')

xclear();
xsetech(wrect=[0,0,1,1/2]);
mat_show(nomsh,nomsv,val);
xsetech(wrect=[0,1/2,1/2,1/2]);
histplot()
xsetech(wrect=[1/2,1/2,1/2,1/2],a3d=%t);
plot3d();




