function []=xrpoly(orig,n,r,teta)
// regular polygon
// orig : center
// n    : number of segments 
// r    : circle diameter/2 
// teta : start angle (default 0)
//!
// Copyright INRIA
  if nargin <= 3 then teta=0; end 
  b=[cos(teta), -sin(teta); sin(teta), cos(teta)];
  ang=2*%pi/n
  pt=(0:n)'*ang;xy=r*[cos(pt),sin(pt)]
  xy=ones(n+1,1)*matrix(orig,1,2)+xy*b
  xpoly(xy(:,1),xy(:,2),"lines",1);
endfunction
