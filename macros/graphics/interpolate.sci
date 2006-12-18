function val=interpolate(x,xref,yref)
// linear interpolation using dsearch 
// returns in val the interpolated 
// Copyright Cermics/Enpc 
// Jean-Philippe Chancelier 
// slight corrections by Bruno Pincon
  if size(xref)<>size(yref) | size(xref,"*")<2  then 
    error('interpolate: xref and yref must be of same size and must have at least 2 components');
    return;
  end
  if isempty(x) then, val = [], return, end

  I=bsearch(x,xref);
  if ~isempty(find(I==0)) then 
    error('lin_interpolation: first argument is outside the range of xref');
    return;
  end
  alpha= (x- xref(I)) ./ (xref(I+1)-xref(I));
  val = alpha.*yref(I+1)+(1-alpha).*yref(I);
endfunction

  
    
