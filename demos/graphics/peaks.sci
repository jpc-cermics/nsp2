function z=peaks(x,y)
  x1=x(:).*.ones(1,size(y,'*'));
  y1=y(:)'.*.ones(size(x,'*'),1);
  z =  (3*(1-x1).^2).*exp(-(x1.^2) - (y1+1).^2) ... 
       - 10*(x1/5 - x1.^3 - y1.^5).*exp(-x1.^2-y1.^2) ... 
       - 1/3*exp(-(x1+1).^2 - y1.^2) 
endfunction

function z=peakit()
  x=-4:0.1:4;y=x;z=peaks(x,y);
endfunction


