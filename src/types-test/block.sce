function F= diagram()
// build a diagram non interactively 
  F=diagram_create();
  B1=%types.Block.new[[10,20,10,10],color=6,background=7];
  F.insert[B1];
  B2=%types.Block.new[[25,20,10,10],color=6,background=7];
  F.insert[B2];
  B3=%types.Block.new[[40,20,10,10],color=6,background=7];
  F.insert[B3];
  // we fix the lock points 
  B4=%types.Block.new[[40,20,10,10],color=6,background=7];
  // ior(dir,ishift(type,4))
  // with :
  // dir = LD_NORTH=0, LD_SOUTH=1, LD_EAST=2, LD_WEST=3, LD_ANY=4;
  // type = L_IN=0 ,L_OUT=1 ,L_EVIN=2 ,L_EVOUT=3 , L_SQP=4 , L_SQM=5 ;
  B4.set_locks_pos[[0.20;0.0;ior(4,ishift(2,4))]]
  F.insert[B4];
  if %t then 
    L=%types.Link.new[[0,10;0,10]];
    L.connect[0,B1,1];  L.connect[1,B2,1];
    F.insert[L];
    L=%types.Link.new[[0,10;0,10]];
    L.connect[0,B2,2];  L.connect[1,B3,2];
    F.insert[L];
  end
endfunction;

function C=draw_vanne()
  if ~new_graphics() then 
    switch_graphics();
  end
  win=xget('window');
  xset('window',30);
  xrect(0,0,10,10);
  F=get_current_figure();
  F.draw_latter[];
  F.start_compound[];
  // test function for block drawing 
  orig=[0,0]
  sz=[10,10];
  // take car that for Opengl 
  // polygone are to be convex when filled 
  //xfpolys(orig(1)+[0;5;7;3;5;10;10;0;0]*sz(1)/10,...
  //      orig(2)+[4;2;7;7;2;0;4;0;4]*sz(2)/10,15);
  // thus we draw 3 polygons.
  xfpolys(orig(1)+[5,5,5;10,7,0;10,3,0]*sz(1)/10,...
	  orig(2)+[2,2,2;4,7,0;0,7,4]*sz(2)/10,[15,15,15]);
  
  xfarcs([orig(1)+3*sz(1)/10;orig(2)+sz(2);4*sz(1)/10;6*sz(2)/10;0;180*64],...
	 15)
  xarcs([orig(1)+3*sz(1)/10;orig(2)+sz(2);4*sz(1)/10;6*sz(2)/10;0;180*64],...
	1);
  xset('font',2,6);
  xstringb(orig(1),orig(2),'String',sz(1),sz(2));
  C=F.end_compound[];
  C.unlink[];
endfunction;

fmode = %t; 
mode = "Gtk";
F=figure_create(wresize=%t,fname=mode,driver=mode,id=20);
// a top level axes 
A=axes_create(top=%t,title="Main title",x="x",y="y")// ,wrect=[0,0,1,1],frect=[0,0,50,50],arect=[1,1,1,1]/12);
F.children(1)= A;
P=polyline_create();
P.x=[0;1;2;0]*10;
P.y=[0;2;0;0]*10;
A.children(1)=P;
// Insert a curve in A.
x=10*linspace(-%pi/2,%pi/2,10);
y=10*cos(2*x);
cu = curve_create(Pts=[x',y'],color=3,width=2);
A.children($+1)=cu ;
//B1=block_create([-20,10,10,10],color=6,background=7);
//A.children($+1)= B1;
//C=draw_vanne();
//A.children($+1)= C;
if %t then 
  // un diagram scicos 
  B1=block_create([-20,10,10,10],color=6,background=7);
  D=diagram_create()
  D.children($+1)= B1;
  A.children($+1)= D;
else
  D=diagram();
  A.children($+1)= D;
end
F.connect[]

 



