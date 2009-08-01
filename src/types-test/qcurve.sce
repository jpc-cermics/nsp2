
function [F,cu]=new_plot(x,y,varargopt)
  mode = "Gtk";
  xset('window',20);
  ok=execstr('F=get_current_figure()',errcatch=%t);
  if ~ok then 
    F=figure_create(fname=mode,driver=mode,id=20);
  else
    // remove previous contents.
    F.children=list();
  end
  if length(F.children)== 0 then 
    rect = [min(x),min(y),max(x),max(y)];
    A = axes_create(top=%t,wrect=[0,0,1,1],frect=rect,arect=[1,1,1,1]/12);
    F.children(1)= A; 
  else
    A = F.children(1);
  end
  varargopt.Pts=[x(:),y(:)]; 
  cu = qcurve_create(varargopt(:)); 
  A.children($+1)= cu;
  F.connect[];
  xbasr(F.id)
endfunction

function oscillo1()
  [F,Q]=new_plot(1:100,ones(1,100),color=6);
  F.children(1).iso=%f;
  t=linspace(0,50,1000);
  for i= 1:1000
    Q.add_points[[t(i),t(i)*sin(2*%pi*t(i))]];
    if modulo(i,2) == 0 then 
      F.draw_now[];
      xpause(40000,%t);
      //xclick();
    end
  end
endfunction

function oscillo2()
  nb=100;
  [F,Q]=new_plot(1:nb,ones(1,nb),color=6);
  F.children(1).iso=%f;
  Q2 = qcurve_create(Pts=[1:nb;ones(1,nb)]',color=2)
  F.children(1).children($+1)= Q2;
  Q3 = qcurve_create(Pts=[1:nb;ones(1,nb)]',color=10)
  F.children(1).children($+1)= Q3;
  Q4 = qcurve_create(Pts=[1:nb;ones(1,nb)]',color=13)
  F.children(1).children($+1)= Q4;
  t=linspace(0,50,10000);
  rnd=rand(1,1);
  for i= 1:1000
    Q.add_points[[t(i),sin(2*%pi*t(i))+1]];
    Q2.add_points[[t(i),cos(4*%pi*t(i))+1]];
    Q3.add_points[[t(i),abs((1+0.3*rand(1,1))*cos(3*%pi*t(i)))]];
    Q4.add_points[[t(i),2.5]]
    if modulo(i,5) == 0 then 
      F.draw_now[];
      xpause(40000,%t);
      //xclick();
    end
  end
endfunction

function oscillo3()
  oscillo();
  F=get_current_figure();
  axe=F.children(1);
  Q1=axe.children(1);
  Q2=axe.children(2);
  Q3=axe.children(3);
  t=linspace(0,50,10000);
  rnd=rand(1,1);
  for i= 1:1000
    Q1.add_points[[t(i),sin(2*%pi*t(i))+1]];
    Q2.add_points[[t(i),cos(4*%pi*t(i))+1]];
    Q3.add_points[[t(i),abs((1+0.3*rand(1,1))*cos(3*%pi*t(i)))]];
    if modulo(i,5) == 0 then 
      F.draw_now[];
      xpause(40000,%t);
      //xclick();
    end
  end
endfunction
