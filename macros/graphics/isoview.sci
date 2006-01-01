function []=isoview(xmin,xmax,ymin,ymax)
// setting default plotting to be isoview
// Copyright INRIA
  plot2d(0,0,style=1,strf="030",rect=[xmin,ymin,xmax,ymax]);
endfunction
