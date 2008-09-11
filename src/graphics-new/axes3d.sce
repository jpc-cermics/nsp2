
function test1()
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
endfunction
  
function test2()
  fmode = %t; 
  mode = "Cairo";
  mode = "Gtk";
  //mode = "OpenGl";
  F=figure_create(wresize=%t,fname=mode,driver=mode,id=20);
  // a top level axes 
  A=axes_create(top=%t,title="Main title",x="x",y="y")// ,wrect=[0,0,1,1],frect=[0,-2,6,2],arect=[1,1,1,1]/12);
  F.children(1)= A;
  P=polyline_create();
  P.x=[0;1;2;0];
  P.y=[0;2;0;0];
  A.children(1)=P;
  C=axes_create(top=%f,alpha=%pi/6,arect=[1,1,1,1]*0.1);
  C.title="Main title";
  C.y = "vertical";
  // the position of the axes in its parent 
  // upper-left, width, height 
  C.wrect=[2,1.5,3,2];
  // the scales that the axes establish for its 
  C.frect=[-2,0,2,3]; 
  t=linspace(-%pi,%pi,30)
  P=surf_create(x=t,y=t,z=sin(t)'*cos(t));
  C.children(1) = P;
  A.children($+1) = C;
  F.connect[]
endfunction
  
function test3()
  fmode = %t; 
  mode = "Cairo";
  mode = "Gtk";
  //mode = "OpenGl";
  F=figure_create(wresize=%t,fname=mode,driver=mode,id=20);
  // a top level axes 
  A=objs3d_create(top=%t,title="Main title",x="x",y="y",colormap=hotcolormap(64));
  // ,wrect=[0,0,1,1],frect=[0,-2,6,2],arect=[1,1,1,1]/12);
  F.children(1)= A;
  x = linspace(0,2*%pi,40);
  z = cos(x')*cos(x);
  exec('tests/tobedone/3dbruno/libbruno.sce');
  P = zsurf_to_polyhedron(x, x, z, 1,64);
  Pl=polyhedron_create(Mcoord=P.coord,Mface=P.face,Mcolor=P.color,Mback_color=P.back_color);
  A.children(1)=Pl;
  F.connect[]
endfunction



  
