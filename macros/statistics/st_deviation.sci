// Jérôme Lelong  
// computes the standard deviation of a matrix
  
function v=st_deviation(x, orientation)
  if nargin==1 then
    v = sqrt(variance(x));
  else
    v = sqrt(variance(x,orientation));
  end
endfunction
