if %t then 
  fmode = %t; 
  mode = "Cairo";
  mode = "Gtk";
  //mode = "OpenGl";
  F=figure_create(wresize=%t,fname=mode,driver=mode,id=20);
  // a top level axes 
  A=axes_create(top=%t,title="Main title",x="x",y="y",wrect=[0,0,1,1],frect=[0,-2,6,2],arect=[1,1,1,1]/5);
  F.children(1)= A;
  t=linspace(-%pi,%pi,30)
  P=surf_create(x=t,y=t,z=sin(t)'*cos(t));
  A.children(1)=P;
  F.connect[]
end
  
