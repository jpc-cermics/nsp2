function [X,Y] = ndgrid(x,y)
//
//  CALLING SEQUENCE
//       [X, Y] = ndgrid(x, y)
//
   nx = size(x,"*")
   ny = size(y,"*")
   X = x(:)*ones(1,ny)
   Y = ones(nx,1)*matrix(y,1,ny)
endfunction
