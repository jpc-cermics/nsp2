function val=interpolate(x,xref=[],yref=[])
// linear interpolation using dsearch 
// returns in val the interpolated 
// Copyright Cermics/Enpc 
// Jean-Philippe Chancelier 
  if xref==[] then val=[] ; return;end
  if size(xref)<>size(yref) then 
    error('lin_interpolation: xref and y_ref must be of same size');
    return;
  end
  xref1=[-%inf;xref(:);%inf];
  yref1=[yref(1);yref(:);yref($)];
  n=size(xref1,'*')
  I=dsearch(x,xref1);
  if find(I==1 & I== n)<>[] then 
    error('lin_interpolation: first argument is outside the range of xref');
    return;
  end
  x1=x(:);
  p=size(x1,'*');
  alpha= (x1- xref1(I)) ./ (xref1(I+1)-xref1(I));
  val = alpha.*yref1(I)+(1-alpha).*yref1(I);
  val=matrix(val,size(x,'r'),size(x,'c'));
endfunction

  
    
