function [z]=eval3d(fun,x,y)
//  here x and y are two vectors. y is optional if not given 
//  y is assumed to be like x.
//  This function returns the matrix z such that z(i,j)=fun(x(i),y(j)) 
//  assuming that the function fun is able to return a result when 
//  x and y are matrices of the same size i.e fun(x,y)
//  ->(fun(x(i),y(i))).
//!
  if nargin <= 2  then
    nx=size(x,'*');ny=nx;
    xx=ones(1,nx).*.matrix(x,1,nx);
    yy=xx;
  else
    nx=size(x,'*');ny=size(y,'*');
    xx=ones(1,ny).*.matrix(x,1,nx);
    yy=matrix(y,1,ny).*.ones(1,nx);
  end
  z=matrix(fun(xx,yy),nx,ny)
endfunction
