function [x,y,z]=eval3dp(fun,p1,p2)
// eval3d - returns a rectangular facets representation of a parametric
// surface.
//  [x,y,z]=eval3dp(fun,p1,p2)
//  fun    : a nsp function [x,y,z]=fun(p1,p2) returning the coordinates
//           of surface points when the parameters are p1 and p2.
//           take car that the function fun is called with vector
//           arguments.
//
//  p1     : real vector giving the first parameter value.
//  p2     : real vector giving the second parameter value.
//  x,y,z  : 4xn real matrices where the i-th column gives the
//           coordinates of facet i of the surface.
//
// Copyright CECILL INRIA (S. Steer) (from scilab).

  if nargin <= 0 then
    function [x,y,z]=scp(p1,p2)
      x=p1.*sin(p1).*cos(p2);
      y=p1.*cos(p1).*cos(p2);
      z=p1.*sin(p2);
    endfunction
    [x,y,z]=eval3dp(scp,0:0.3:2*%pi,-%pi:0.3:%pi);
    plot3d1(x,y,z);
    return;
  end
  n1=size(p1,'*')
  n2=size(p2,'*')
  [x,y]=ndgrid(p1,p2);x.redim[1,-1];y.redim[1,-1];
  [vx,vy,vz]=fun(x,y);
  //on genere les facettes
  ind=ones(1,n1-1).*.[0 1 n1+1 n1]+ (1:n1-1).*.[1 1 1 1];
  // ind=[1,2,n1+2,n1+1 , 2,3,n1+3,n1+2, ....  ,n1-1,n1,2n1,2n1-1
  ind2=ones(1,n2-1).*.ind+((0:n2-2)*n1).*.ones(size(ind));
  nx=size(ind2,'*');
  x=matrix(vx(ind2),4,nx/4);
  y=matrix(vy(ind2),4,nx/4);
  z=matrix(vz(ind2),4,nx/4);
endfunction
