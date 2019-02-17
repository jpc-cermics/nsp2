// -*- Mode: nsp -*-

// convhull
//----------

if %f then 
  x = -3:0.05:3;
  y = abs (sin (x));
  k = convhull (x, y);
  rect= [-3.05, 3.05, -0.05, 1.05];
  if %f then 
    plot (x(k),y(k),"r-", x,y,"b+");
    axis(rect);
  end
end

x = -3:0.5:3;
y = abs (sin (x));
if ~convhull(x, y).equal[[1;7;13;12;11;10;4;3;2;1]] then pause;end

// delaunay 
//----------

if %f then 
  x = rand (1,10);
  y = rand (1,10);
  tri = delaunay (x,y);
  clf();
  triplot (tri, x, y);
  hold('on');
  plot (x, y, "r5*");
  axis ([0,1,0,1]);
end 

x = [-1, 0, 1, 0];
y = [0, 1, 0, -1];
h=delaunay(x,y);
h = sort(sort(h, type='c', dir='i'), type='lr', dir='i');
if ~h.equal[[1,2,4;2,3,4]] then pause;end 

x = [-1, 0, 1, 0];
y = [0, 1, 0, -1];
h=delaunay([x(:),y(:)]);
h = sort(sort(h, type='c', dir='i'), type='lr', dir='i');
if ~h.equal[[1,2,4;2,3,4]] then pause;end 

x = [1 5 2; 5 6 7];
y = [5 7 8; 1 2 3];
h=delaunay(x,y);
h = sort(sort(h, type='c', dir='i'), type='lr', dir='i');
if ~h.equal[[1,2,4;1,3,4;1,3,5;3,4,6]] then pause;end 

x = [-1, -1, 1, 0, -1]; y = [-1, 1, 1, 0, -1]; z = [0, 0, 0, 1, 1];
h=delaunay(x,y,z);
h = sort(sort(h, type='c', dir='i'), type='lr', dir='i');
if ~h.equal[[1,2,3,4;1,2,4,5]] then pause;end 


 





