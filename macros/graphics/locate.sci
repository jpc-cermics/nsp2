function [x,but]=locate(n,flag=0)
//[x]=locate(n,flag)
//   let the user select a bunch of points using mouse clicks.
//   x=locate(n)
//    - For n>0 returns in x(2,n) the coordinates of n mouse selected
//    points 
//    - For n<=0 returns in x the coordinates of mouse selected points
//    until end of selection (click with left button).
//   x=locate() is equivalent to x=locate(-1);
//   If flag=1, each selected points is drawn.
// Copyright INRIA
// adapted to nsp May2013 cfz. 
  
  but=[]
  xselect();
  if nargin==0 then n=-1;end
  
  function []=clearmode(flag,x,xxx)
    modek=xget('alufunction');
    xset('alufunction',6);
    if flag==1 then xpoly(x(1,:),x(2,:),mark = 2, color=5);end 
    xset('alufunction',modek);
    xset('mark',xxx(1),xxx(2));
  endfunction
  
  x=[];
  xxx=xget('mark');
  xset('mark',2,xxx(2));
  wc=xget('window')
  if n >= 0 then 
    for i=1:n,
      while %t
	[p,x1,y1,w,m]=xclick();
	if w==wc then break,end
      end
      if flag==1 then xpoly(x1,y1, mark = 1, color=5);end
      x=[x,[x1,y1]'];
      but=[but,p]; 
    end
  else
    while %t, 
      while %t
	[p,x1,y1,w,m]=xclick();
	if w==wc then break,end
      end
      if p==0 then;  //changed 'i' to 'p' 24may2013
	clearmode(flag,x,xxx);,return
      elseif flag==1 then
	xpoly(x1,y1, mark = 2, color=5)
      end
      x=[x,[x1,y1]'];
      but=[but,p];
    end
  end
  clearmode(flag,x,xxx);
endfunction
