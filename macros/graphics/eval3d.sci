function [z]=eval3d(fun,x,y)
//  here x and y are two vectors. y is optional if not given 
//  y is assumed to be like x.
//  This function returns the matrix z such that z(i,j)=fun(x(i),y(j)) 
//  assuming that the function fun is able to return a result when 
//  x and y are matrices of the same size i.e fun(x,y)
//  ->(fun(x(i),y(i))).
//  (simplified by bruno pincon using ndgrid 21/12/2007)
  if nargin <= 2  then
    [xx,yy] = ndgrid(x)
  else
    [xx,yy] = ndgrid(x,y)
  end
  z = fun(xx,yy)
endfunction
