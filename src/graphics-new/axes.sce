  xsetech(arect=[0.0,0.0,0.0,0.0],wrect=[0,0,1,1],frect=[0,-2,6,2]);
  fmode = %t; 
  F=figure_create();
  // a top level axes 
  A=axes_create(top=%t,wrect=[0,0,1,1],frect=[0,-2,6,2])
  F.children(1)= A;
  P=polyline_create();
  P.Pts=[0,0;1,2;2,0;0,0];
  A.elts(1)=P;
  figure_attach(F);
  // insert a new axes 
  C=axes_create(alpha=%pi/6);
  // the position of the axes in its parent 
  // upper-left, width, height 
  C.wrect=[2,1.5,3,2];
  // the scales that the axes establish for its 
  C.frect=[-2,0,2,3]; 
  P=polyline_create();
  P.Pts=[0,0;1,2;2,0];
  C.elts(1) = P;
  P=polyline_create();
  P.Pts=[-2,0;2,0;2,3;0,3;-2,3];
  C.elts(2) = P;
  x=linspace(-2,2,200);
  P=polyline_create();
  P.Pts=[x;sin(x)]';
  C.elts(3)=P;
  A.elts(2) = C;
  figure_attach(F);
  

  
  
