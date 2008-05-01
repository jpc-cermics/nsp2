n=5;
p=4;
val=10*rand(n,p);
nomsh='nh'+ string(1:p);
nomsv='nv '+ string(1:n)';

function mat_show(nomsh,nomsv,val)
  x=1:p+1+1;
  y=1:n+1+1;
  plot2d([],[],rect=[min(x),min(y),max(x),max(y)],strf='070');
  //a=gca();
  //a.clip_state="off"; // xclip() ne marche plus 
  xp=[x;x]; yp=[min(y)*ones_deprecated(x);max(y)*ones_deprecated(x)];
  xpolys(xp,yp);
  yp=[y;y]; xp=[min(x)*ones_deprecated(y);max(x)*ones_deprecated(y)];
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
//a=gca();
//a.box="off";
xsetech(wrect=[1/2,1/2,1/2,1/2]);
plot3d();

////xs2ps(0,'incertitude.ps');
//unix(SCI+'/bin/Blatexpr -p 1 1 incertitude.ps')  /// \sleftarrow{calling a Unix script}


