function cmap = greencolormap(m)
  if size(m,'*')<>1 then
    error('hotcolormap : n must be an integer greater than 3')
  end
  cmap=[((1:m).^2)./m;1:m;((1:m).^2)./m]' ./m;
endfunction
