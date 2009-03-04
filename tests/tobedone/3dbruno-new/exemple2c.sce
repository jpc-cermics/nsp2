// exemple 2c : un peu comme un plot3d

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
  n = 80;
  x = linspace(-2, 2, n);
  y = linspace(-2, 2, n);
  z = eval3d(milk_drop, x, y);
  // on limite l'amplitude en z
  z = ( 2*z / max(z) );

  P = zsurf_to_spolyhedron(x, x, z, 1, 128, back_color=40); 
  A.children(1)=P;
  F.connect[];

  


