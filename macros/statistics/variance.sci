// Jérôme Lelong 

// computes the variance of a matrix using the standard unbiased
// estimator. For 2 dimensional arrays, the variance is comuted along
// the first non-singleton dimension

function v=variance(x, orientation)
  [m,n]=size(x);
  // for an integer, cannot use the unbiased estimator because n-1=0
  if isempty(x) then v=0; return;
  elseif m == 1 && n==1 then v=0; return; end
	
  if nargin==1 then
	if m>1 then
	  orientation = 1;
	else
	   orientation= 2;
	end
  end
  
  if orientation == 1 || orientation == 'r' then
	v=sum((x-ones(x(:,1))*mean(x,orientation)).^2, orientation)/(size(x, orientation)-1);
  elseif orientation == 2 || orientation == 'c' then
	v=sum((x-mean(x,orientation)*ones(x(1,:))).^2, orientation)/(size(x, orientation)-1);
  elseif orientation == '*' then
    v = sum((x-mean(x, '*')).^2)/(size(x,'*')-1);
  else error('Optional 2nd argument orientation must be equal to ''*'', ''c'', ''r''');
  end
	
  
endfunction
