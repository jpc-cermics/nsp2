// Copyright Jérôme Lelong

// Matlab compatibility. For 2 dimensionnal arrays, mean(x) is the
// mean value of the elements along the first non-singleton dimension
// of x.

function [y] = mean(x,orientation)
  [m,n]=size(x);
  if isempty(x) then y=0; return; end
	
  if nargin==1 then 
	if m>1 then
	  orientation = 1;
	else
	   orientation= 2; 
	end
  end
	
  y = sum(x, orientation)/size(x, orientation)

endfunction
