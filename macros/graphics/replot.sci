function replot(rect)
// replots the current graphic window
// using rect as x and y bounds 
// Copyright INRIA
  win=xget("window");
  xclear(win,%f);
  xtape('replaysc',win,rect);
endfunction
