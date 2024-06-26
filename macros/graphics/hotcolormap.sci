function cmap = hotcolormap(n)
//graycmap   yellow to red color map.
// Copyright INRIA
  if size(n,'*')<>1|or(n<3) then
    error('hotcolormap : n must be an integer greater than 3')
  end
  n1=floor(3/8*n);
  n2=n1
  n3=n-(n1+n2)
  //
  cmap=[(1:n1)'./n1,zeros_new(n1,1), zeros_new(n1,1);
	ones_new(n2,1),(1:n2)'./n2,zeros_new(n2,1);
	ones_new(n3,1),ones_new(n3,1),(1:n3)'./(n3)]
endfunction
