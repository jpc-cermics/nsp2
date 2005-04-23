function []=xclip(x,y,w,h)
// call to xset('clipping' | 'clipoff' | 'clipgrf')
//      encapsulated.
// not really usefull ....
// Copyright INRIA
  if nargin <=0 then xset('clipoff');return;end
  if nargin == 1 then 
    if type(x,'short')=='s' then
      xset(x)
    else
      xset('clipping',x(1),x(2),x(3),x(4));
    end
  else 
    xset('clipping',x,y,w,h);
  end
endfunction
