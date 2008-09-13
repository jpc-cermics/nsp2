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
  ncol= 32
  A=objs3d_create(top=%t,title="Main title", colormap=hotcolormap(ncol));
  // ,wrect=[0,0,1,1],frect=[0,-2,6,2],arect=[1,1,1,1]/12);
  F.children(1)= A;
  x = linspace(0,2*%pi,20);
  z = cos(x')*cos(x);
  P = zsurf_to_polyhedron(x, x, z, 1,ncol);
  Pl=polyhedron_create(Mcoord=P.coord,Mface=P.face,Mcolor=P.color,Mback_color=P.back_color);
  A.children(1)=Pl;
  F.connect[];
endfunction

function test4()
  fmode = %t; 
  mode = "Cairo";
  mode = "Gtk";
  //mode = "OpenGl";
  F=figure_create(wresize=%t,fname=mode,driver=mode,id=20);
  // a top level axes 
  ncol= 32;
  cm=[jetcolormap(128);[0.9 0.9 0.9]];
  A=objs3d_create(top=%t,title="Main title", colormap=cm);
  // ,wrect=[0,0,1,1],frect=[0,-2,6,2],arect=[1,1,1,1]/12);
  F.children(1)= A;
  
  function [z] = rozen(x,y)
  // pour plot3d
    C = 1
    x = x(:)
    y = (y(:))'
    z = C*(ones(size(x))*y - x.^2*ones(size(y))).^2 + (1 - x).^2*ones(size(y))
  endfunction

  n = 20;
  x = linspace(-1.5,2,n);
  y = linspace(-2,3,n);
  z = rozen(x,y);
  // on limite l'amplitude en z
  z = 3*z/max(z);
  P = zsurf_to_spolyhedron(x, x, z, 1, 128); 
  //  draw3d_objs(listObj,ebox=ebox,box_color=129,box_style="matlab");

  Pl=spolyhedron_create(Mcoord=P.coord,Mface=P.face,coloutmin=P.colout(1),...
			coloutmax=P.colout(2),Mval = P.val,colmin =P.colminmax(1),...
			colmax= P.colminmax(2), back_color=P.back_color, vmin=P.valminmax(1),...
			vmax=P.valminmax(2));
  A.children(1)=Pl;
  F.connect[];
endfunction

exec('SCI/tests/tobedone/3dbruno/libbruno.sce');
  



  
