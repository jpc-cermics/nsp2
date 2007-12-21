// Jérôme Lelong 

// computes the variance of a matrix using the standard unbiased
// estimator. For 2 dimensional arrays, the variance is computed along
// the first non-singleton dimension

function v=variance(x, orientation)
  [m,n]=size(x);

  if isempty(x) then 
     v=0; return;   // %nan is the matlab 's answer, should we changed that ?
  elseif m == 1 && n==1 then
     // for a scalar, cannot use the unbiased estimator because n-1=0
     v=0; return; 
  end
	
  if nargin==1 then
     if m>1 then
	orientation = 1;
     else
	orientation= 2;
     end
  end
  
  if orientation == 1 || orientation == 'r' then
     v=sum((x-ones_new(m,1)*mean(x,1)).^2, 1)/(m-1);
  elseif orientation == 2 || orientation == 'c' then
     v=sum((x-mean(x,2)*ones_new(1,n)).^2, 2)/(n-1);
  elseif orientation == '*' || orientation == 0 then
     v = sum((x-mean(x, '*')).^2)/(m*n-1);
  else 
     error('Optional 2nd argument orientation must be equal to ''*'', ''c'', ''r''');
  end
  
endfunction
