function titlepage(str,win)
// display a string matrix str 
// centered in the graphic window win 
// Serge Steer Copyright INRIA
  if nargin <= 0 then str = 'Nsp';end 
  old=xget('window')
  if nargin==2 then xset('window',win);end
  plot2d([],[],rect=[0,0,1,1],axesflag=0);
  xstringb(0,0,str,1,1,'fill');
  xset('window',old)
endfunction
