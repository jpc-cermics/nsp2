function pie(varargin)
// This function draws a pie, 
// if size of x is N then pie
// function draws a pie with N parts, the area of the ith part is equal to
// (x(i)/sum(x))*( surface of the unit cercle).
//
// syntax : pie(x[,sp[,txt]])
// Intput : The input arguments must have the same size
// x : a scalar or a vector of positive reals. 
// sp : a scalar or a vector of reals. The sp vector allows to cut one or several parts of the pie
// txt : a cell or a vector of strings. The txt vector allows to write a text for each part of the pie
// F.B
// adapter to nsp: jpc 2015 
// Copyright CECILL INRIA (from scilab)
    
  varlist = varargin;
  // Input arguments must have the same length

  for i=2:size(varlist)
    if size(varlist(i-1),"*") <> size(varlist(i),"*") then
      error("input arguments must have the same length")
    end
  end

  txt = m2s([]);
  esp = [];

  if length(varlist) <= 0 then 
    pie([3 4 6 2],[0 1 0 0],["part1","part2","part3","part4"]);
    return
  end

  x = varlist(1);
  if ~( type(x,'short')=='m' && ~isempty(x) && and(x > 0)) then
    error("first argument must be a non empty vector of positive reals");
  end
  
  if size(varlist) == 2 then
    // pie(x,sp);
    if type(varlist(2),'short') == 'm' ||  type(varlist(2),'short') == 'b'
      esp = varlist(2);
    elseif type(varlist(2),'short') == 's' ||  type(varlist(2),'short') == 'c' then
      txt = varlist(2);
    end
  elseif size(varlist) == 3 then
    // case : pie(x,sp,txt)
    if  type(varlist(2),'short') == 'm' ||  type(varlist(2),'short') == 'b' then
      esp = varlist(2);
    else 
      error("second argument must be boolean or real");
    end
    if type(varlist(3),'short') == 's' 
      txt = varlist(3);
    elseif type(varlist(3),'short') == 'c'
      for j=1:size(varlist(3),"*")
	txt(j) = varlist(3){j}
      end
    else
      error("third argument must be a cell or a vector of strings");
    end
  else 
    error("wrong number of input arguments")
  end
  
  // xi and yi represents the coordinates of each polyline
  // iesp is the index of the part of i which are separated of the pie
  // teta_1 is the start angle of the arc polyline, teta_2 is the end angle of the arc polyline
  iesp = find(esp>0 | esp<0);
  teta_2 = 0 + %pi/2;
  yi = [];
  xi = [];
  CurColor = 0;
  
  xsetech(frect=[-1.2,-1.2,1.2,1.2],iso=%t,fixed=%t,axesflag=0);
  
  // Create a closed polyline for every parts of pie, the polyline inside
  // color is determinated by the plot colormap
  for i=1:size(x,"*")
    xi = [];
    yi = [];
    teta_1 = teta_2;
    teta_i = (x(i)/sum(x))*2*%pi;
    teta_2 = teta_1 + teta_i
    if size(x,"*") <> 1 then
      xi(1) = 0;
      yi(1) = 0;
      xi(2) = cos(teta_1);
      yi(2) = sin(teta_1);
    else
      xi(1) = cos(teta_1);
      yi(1) = sin(teta_1);
    end
    inter = 1/(100*x(i)/sum(x));
    k = inter;
    while k<1
      xi($+1) = cos((1-k)*teta_1 +k*teta_2);
      yi($+1) = sin((1-k)*teta_1 +k*teta_2);
      k= k + inter;
    end 
    xi($+1) = cos(teta_2);
    yi($+1) = sin(teta_2);
    if iesp.has[i] then
      x_shift = ones(1,size(xi,"*")) * (1/10) * cos((teta_2+teta_1)/2);
      y_shift = ones(1,size(yi,"*")) * (1/10) * sin((teta_2+teta_1)/2);
      xi = xi+x_shift;
      yi = yi+y_shift;
      xfpoly(xi,yi,fill_color=i,color=1);
      if ~isempty(txt)
	xstring(cos((teta_2+teta_1)/2)*1.2+x_shift(1)-0.1*(cos((teta_2+teta_1)/2)<0),sin((teta_2+teta_1)/2)*1.2+y_shift(1),txt(i));
      else
	xstring(cos((teta_2+teta_1)/2)*1.2+x_shift(1)-0.1*(cos((teta_2+teta_1)/2)<0),sin((teta_2+teta_1)/2)*1.2+y_shift(1),string(round((x(i)/sum(x))*100)) + "%");
      end
    else
      xfpoly(xi,yi,fill_color=i,color=1);
      if ~isempty(txt)
	xstring(cos((teta_2+teta_1)/2)*1.2-0.1*(cos((teta_2+teta_1)/2)<0),sin((teta_2+teta_1)/2)*1.2,txt(i));
      else
	xstring(cos((teta_2+teta_1)/2)*1.2-0.1*(cos((teta_2+teta_1)/2)<0),sin((teta_2+teta_1)/2)*1.2,string(round((x(i)/sum(x))*100)) + "%");
      end
    end
  end
endfunction
