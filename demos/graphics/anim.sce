
function demo_anim_1()
  t=%pi*(-5:5)/5;
  plot3d1(t,t,sin(t)'*cos(t),alpha=35,theta=45);
  st=1;
  for i=35:st:80, // loop on theta angle
    xclear();
    plot3d1(t,t,sin(t)'*cos(t),alpha=i,theta=45,flag=[1,2,4])
    xset("wshow");
  end
  for i=45:st:80, //loop on alpha angle
    xclear()
    plot3d1(t,t,sin(t)'*cos(t),alpha=80,theta=i,flag=[1,2,4])
    xset("wshow");
  end
endfunction

function demo_anim_2()
  np=10;
  t=(0:0.1:np)*%pi;
  for i=1:st:30
    xclear();
    param3d((t/(np*%pi)*%pi).*sin(t),(t/(np*%pi)*%pi).*cos(t),...
	    i*t/(np*%pi),alpha=35,theta=45,flag=[2,4]);
    xset("wshow");
  end
endfunction

function demo_anim_3()
  t=-%pi:0.3:%pi;
  for i=35:80,
    xclear();
    contour(t,t,sin(t)'*cos(t),10,alpha=i,theta=45,flag=[1,2,4])
    xset("wshow");
  end
  for i=45:80,
    xclear();
    contour(t,t,sin(t)'*cos(t),10,alpha=80,theta=i,flag=[1,2,4])
    xset("wshow");
  end
endfunction








