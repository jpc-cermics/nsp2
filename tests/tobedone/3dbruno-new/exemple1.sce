// first example 
// 

[C, L, LL, P] = obj1();
S = string3d_create( Mcoord=[1.2;0.5;0.5],str="A string !");

// draw3d_objs(listObj,ebox=ebox,flag=[0,1,0],with_box=%f,with_mesh=%t);

fmode = %t; 
mode = "Cairo";
mode = "Gtk";
//mode = "OpenGl";
F=figure_create(wresize=%t,fname=mode,driver=mode,id=20);
A=objs3d_create(top=%t,title="Main title",box_color=0,box_style=1,with_box=%f) 
// ,wrect=[0,0,1,1],frect=[0,-2,6,2],arect=[1,1,1,1]/12);
F.children(1)=A;
A.children(1)=C;
A.children(2)=L;
A.children(3)=LL;
A.children(4)=S;
A.children(5)=P;
if %f then 
  for i=1:size(P.Mcoord,'c');
    S = string3d_create(Mcoord= P.Mcoord(:,i), str=string(i)+"XX");
    A.children($+1)=S;
  end 
end

F.connect[];



  
