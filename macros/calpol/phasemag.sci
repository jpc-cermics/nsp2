function [phi,db]=phasemag(z,mod)
// phase and magnitude computation
// Copyright CECILL INRIA (from scilab).

  if nargin <= 1 then mod='c';end
  if nargout ==2 then 
    db=20*log(abs(z))/log(10),
  end
  //compute first phase value in  (-pi, pi]
  phi1=atan(imag(z(:,1)),real(z(:,1)))
  //compute phase increments in (-pi, pi]
  z=z(:,2:$)./z(:,1:$-1)
  dphi=atan(imag(z),real(z))
  phi=cumsum([phi1, dphi],2)
  if part(mod,1)<>'c' then  
    // reset modulo 360
    phi=modulo(phi,2*%pi)
  end
  phi=phi*180/%pi; //transform in degree
endfunction
