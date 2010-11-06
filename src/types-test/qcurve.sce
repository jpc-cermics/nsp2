// test code in nsp 
// similar C-code is used internally by scicos for cscope 

function [F,cu]=new_plot(x,y,rect,varargopt)
  mode = "Gtk";
  xset('window',20);
  ok=execstr('F=get_current_figure()',errcatch=%t);
  if ~ok then 
    F=figure_create(fname=mode,driver=mode,id=20);
  else
    // remove previous contents.
    F.children=list();
  end
  A = axes_create(top=%t,wrect=[0,0,1,1],rect=rect,arect=[1,1,1,1]/12);
  A.iso=%f;
  A.fixed=%t;
  A.auto_axis=%t;
  F.children(1)= A;
  varargopt.Pts=[x(:),y(:)]; 
  cu = qcurve_create(varargopt(:)); 
  A.children($+1)= cu;
  F.connect[];
  F.invalidate[];
  // xbasr(F.id)
endfunction

function oscillo1(fixedminmax=%t)
// create a Figure with a qcurve 
// The qcurve can contain atmost 8000 pts 
  [F,Q]=new_plot(1:8000,ones(1,8000),[0,-2,100,2],color=6);
  n=200;
  nt=200;
  t=linspace(0,500,n);
  for i= 1:n-1
    // adding and drawing nt points.
    ti=linspace(t(i),t(i+1),nt);
    if fixedminmax then 
      Q.add_points[[ti',sin(2*%pi*ti'/10)]];
    else
      Q.add_points[[ti',ti'.*sin(2*%pi*ti'/10)]];
    end
    F.invalidate[];
    A= F.children(1);
    if fixedminmax then 
      // we force the scales 
      xmax = max(t(i+1),100);
      xmin = max(0,xmax-100);
      A.rect = [xmin,-2,max(t(i+1),100),2];
      A.fixed=%t;
    else
      // we let moving data fix the scales 
      A.fixed=%f;
    end
    xpause(0,%t);
  end
endfunction

function oscillo2(fixedminmax=%t)
// more than one curve 
  nb=4000;
  [F,Q]=new_plot(1:nb,ones(1,nb),[0,-2,100,2],color=6);
  F.children(1).iso=%f;
  Q2 = qcurve_create(Pts=[1:nb;ones(1,nb)]',color=2)
  F.children(1).children($+1)= Q2;
  Q3 = qcurve_create(Pts=[1:nb;ones(1,nb)]',color=10)
  F.children(1).children($+1)= Q3;
  Q4 = qcurve_create(Pts=[1:nb;ones(1,nb)]',color=13)
  F.children(1).children($+1)= Q4;

  n=200;
  nt=200;
  fixedminmax=%f;
  t=linspace(0,500,n);
  for i= 1:n-1
    // adding and drawing nt points.
    ti=linspace(t(i),t(i+1),nt);
    Q.add_points[[ti',sin(2*%pi*ti'/10)+1]];
    Q2.add_points[[ti',0.5*cos(4*%pi*ti'/30)+1]];
    Q3.add_points[[ti',(1+0.3*sin(%pi*ti'/40)).*cos(3*%pi*ti'/10)]];
    Q4.add_points[[ti',2.5*ones(nt,1)]];
    F.invalidate[];
    A= F.children(1);
    if fixedminmax then 
      // we force the scales 
      xmax = max(t(i+1),100);
      xmin = max(0,xmax-100);
      A.rect = [xmin,-2,max(t(i+1),100),2];
      A.fixed=%t;
    else
      // we let moving data fix the scales 
      A.fixed=%f;
    end
    xpause(0,%t);
  end
  pause;
endfunction
