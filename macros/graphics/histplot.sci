function histplot(n,data,normalize=%t,style=[2],strf='171',rect=[],leg="",nax=[2,5,2,5],axesflag=[1],frameflag=[8],fill=%t)
//  
// draws histogram of data entries using classes given by n 
// Note that if n is a vector it must be strictly increasing 
// Example : enter histplot()
// hand rewriten from a previous scilab version (J.P Chancelier)
// fill option added (J.P Chancelier)
// dsearch use (Bruno Pinçon) 
// 
  if nargin <= 0, 
    s_mat=['histplot([-6:0.2:6],rand(1,2000,''n''));';
	   'function [y]=f(x); y=exp(-x.*x/2)/sqrt(2*%pi);endfunction';
	   'x=-6:0.1:6;x=x'';plot2d(x,f(x),style=1,strf='"000'");';
	   'titre= ''macro histplot : Histogram plot'';';
	   'xtitle(titre,''Classes'',''N(C)/Nmax'');'];
    execstr(s_mat);
    return;
  end;
  if nargin < 2 then printf('histplot : Wrong number of arguments\n'); return; end;
  p=size(data,'*')
  data=data(:)

  if size(n,'*')==1 then 
    x = linspace(min(data), max(data), n+1)';
  else
    x=n(:)
  end,
  n=prod(size(x));
  [ind , y] = dsearch(data, x);
  if normalize then y=y ./ (p*(x(2:$)-x(1:$-1))),end 
  y(n)=y($);   // add a last point 
  if rect==[] then rect=[min(x),0,max(x),max(y)];end 
  if fill==%f then 
    plot2d2(x,y,style=style,strf=strf,rect=rect,axesflag=axesflag,frameflag=frameflag);
    plot2d3(x,y,style=style,strf='000');
  else
    plot2d2([],[],rect=rect,strf=strf,axesflag=axesflag,frameflag=frameflag);
    xp=[x(1:$-1),x(2:$),x(2:$),x(1:$-1)]';
    y=y(1:$-1);
    yp=[0*y,0*y,y,y]';
    xfpolys(xp,yp,style*ones(1,n-1));
  end
endfunction





