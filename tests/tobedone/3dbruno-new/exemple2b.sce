// exemple 2b : un peu comme un plot3d1


  fmode = %t; 
  mode = "Cairo";
  mode = "Gtk";
  //mode = "OpenGl";
  F=figure_create(wresize=%t,fname=mode,driver=mode,id=20);
  // a top level axes 
  ncol= 32;
  cm=[jetcolormap(128);[0.9 0.9 0.9]];
  A=objs3d_create(top=%t,title="Main title", colormap=cm,box_color=129,box_style=0,with_box=%t );
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
  A.children(1)=P;
  F.connect[];

  

