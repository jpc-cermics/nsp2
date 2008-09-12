if %t then 
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
  // Insert a curve in A.
  x=linspace(-%pi/2,%pi/2,10);
  y=cos(2*x);
  cu = curve_create(Pts=[x',y'],color=3,width=2);
  A.children($+1)=cu ;
  // insert a matrix 
  ma = gmatrix_create(data=32*rand(6,8),remap=%f,rect=[-1,1,0.5,2.5]);
  A.children($+1)= ma;
  // insert a vector field 
  vf = vfield_create(x=linspace(0,1,5),y=linspace(0,1,4),fx=rand(5,4),fy= ...
		     rand(5,4),colored=%t);
  A.children($+1)= vf;
  // insert level curves 
  cf = contour_create(x=linspace(2,4,5),y=linspace(2,3,4),z=rand(5,4), ...
		      nlevels=5);
  A.children($+1)= cf;
  // insert a new axes 
  C=axes_create(top=%f, rho=%pi/6,arect=[1,1,1,1]*0.1);
  C.title="Main title";
  C.y = "vertical";
  // the position of the axes in its parent 
  // upper-left, width, height 
  C.wrect=[2,1.5,3,2];
  // the scales that the axes establish for its 
  C.frect=[-2,0,2,3]; 
  P=polyline_create();
  C.children(1) = P;
  P=polyline_create(close=%t,color=9,thickness=2);
  P.x=[-2,2,0,-2];
  P.y=[0,0,3,3,3];
  C.children(2) = P;
  x=linspace(-2,2,20);
  P=polyline_create(x=x,y=sin(x),thickness=4,color=4);
  C.children(3)=P;
  // matrix 
  ma = gmatrix_create(data=32*rand(6,8),remap=%f,rect=[-1,1,0.5,2.5]);
  C.children($+1)= ma;
  A.children($+1) = C;
  F.connect[]
end

function F=new_plot(x,y,varargopt)
  mode = "Gtk";
  ok=execstr('F=get_current_figure()',errcatch=%t);
  if ~ok then 
    F=figure_create(fname=mode,driver=mode,id=20);
  end
  if length(F.children)== 0 then 
    rect = [min(x),min(y),max(x),max(y)];
    A = axes_create(top=%t,wrect=[0,0,1,1],frect=rect,arect=[1,1,1,1]/12);
    F.children(1)= A; 
  else
    A = F.children(1);
  end
  varargopt.Pts=[x(:),y(:)]; 
  cu = curve_create(varargopt(:)); 
  A.children($+1)= cu;
  F.connect[];
  xbasr(F.id)
endfunction


  
