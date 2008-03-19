  fmode = %t; 
  F=figure_create(wresize=%t);
  // a top level axes 
  A=axes_create(top=%t,wrect=[0,0,1,1],frect=[0,-2,6,2],arect=[1,1,1,1]/12);
  F.children(1)= A;
  P=polyline_create();
  P.Pts=[0,0;1,2;2,0;0,0];
  A.children(1)=P;
  // Insert a curve in A.
  for j=1:1
    x=linspace(-%pi/2,%pi/2,10);
    y=cos(2*x);
    cu = curve_create(Pts=[x',y'],color=3,width=2);
    A.children(1+j)=cu ;
  end
  // insert a new axes 
  C=axes_create(alpha=%pi/6,arect=[1,1,1,1]*0);
  // the position of the axes in its parent 
  // upper-left, width, height 
  C.wrect=[2,1.5,3,2];
  // the scales that the axes establish for its 
  C.frect=[-2,0,2,3]; 
  P=polyline_create();
  P.Pts=[0,0;1,2;2,0];
  C.children(1) = P;
  P=polyline_create();
  P.Pts=[-2,0;2,0;2,3;0,3;-2,3];
  C.children(2) = P;
  x=linspace(-2,2,20);
  P=polyline_create();
  P.Pts=[x;sin(x)]';
  C.children(3)=P;
  A.children($+1) = C;
  F.connect[]
  
