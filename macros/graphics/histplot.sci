function histplot(n,data,normalization=%t,style=1,strf='171',rect=[],leg="")
// histplot(n,data,[style,strf,leg,rect,nax])
// draws histogram of entries in  data put into n classes
// 
// histplot(xi,data,[style,strf,leg,rect,nax])
// generates the histogram of entries in data put into classes
// ]xi(k) xi(k+1)] .
// xi's are assumed st. increasing
//
// [style,strf,leg,rect,nax] same as plot2d
// Example : enter histplot()
//! 
// Copyright INRIA (or Enpc ? )
// 
// modif to use dsearch (Bruno le 10/12/2001)
  if nargin <= 0, 
    s_mat=['histplot([-6:0.2:6],rand(1,2000,''n''));';
	   'function [y]=f(x); y=exp(-x.*x/2)/sqrt(2*%pi);endfunction';
	   'x=-6:0.1:6;x=x'';plot2d(x,f(x),style=1,strf='"000'");';
	   'titre= ''macro histplot : Histogram plot'';';
	   'xtitle(titre,''Classes'',''N(C)/Nmax'');'];
    execstr(s_mat);
    return;
  end;
  if nargin < 2 
    printf('histplot : Wrong number of arguments\n');
    return;
  end;
  p=size(data,'*')
  data=data(:)

  if size(n,'*')==1 then 
    x = linspace(min(data), max(data), n+1)';
  else
    x=n(:)
  end,
  n=prod(size(x));
  [ind , y] = dsearch(data, x);
  if normalization then y=y ./ (p*(x(2:$)-x(1:$-1))),end //normalization
  // add a last point 
  y(n)=y($);
  rect=[min(x),0,max(x),max(y)];
  plot2d2(x,y,rect=rect);
  plot2d3(x,y,strf='000');
endfunction
