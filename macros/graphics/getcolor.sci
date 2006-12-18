function icol=getcolor(win=-1,cinit=-1)
// Coyright Cermics/Enpc Jean-Philippe Chancelier
// pick a color in the colormap of window win 
// or in the current window if it exists.
// 
  if isempty(winsid()) then 
    printf('error: you must open a graphic window\n');
    icol=[];
    return;
  end
  curwin=xget('window');
  if win == -1 then 
    win=curwin;
  end
  if win<>curwin then 
    xset('window',win);
  end
  cl=xget('colormap');
  ccol=xget('color');
  if cinit<>-1 then ccol=cinit;end
  xinit(dim=[100,100]);
  getcolwin=xget('window');
  xset('colormap',cl);
  p=xget('lastpattern')+2;
  n=10;
  m=modulo(p,n);
  m=ceil(p/n);
  x=1:n;
  y=m:-1:1;
  nx=size(x,'*');
  ny=size(y,'*');
  xx=ones(1,ny).*.matrix(x,1,nx);
  yy=matrix(y,1,ny).*.ones(1,nx);
  xsetech(frect=[0,-1,n+2,m+1],arect=[0,0,0,0])
  rects=[xx;yy;ones(xx);ones(yy)];
  rects=rects(:,1:p);
  xset('thickness',1);
  xrects(rects,[1:p]);
  xrects(rects,-(p)*ones(1,p));
  xrects(rects(:,ccol),-(p-1));
  while %t
    [c,x,y]=xclick(clearq=%t)
    ix=floor(x);
    iy=m-ceil(y);
    icol=ix+n*iy;
    if icol >= 1 & icol <= p then 
      xinfo(sprintf('selected color %d',icol));
      xrects(rects(:,ccol),-p);
      xrects(rects(:,icol),-(p-1));
      xset('window',curwin);
      xdel(getcolwin);
      break;
    end
    xinfo('wrong selection');
  end 
endfunction

