// obsolete function 
function [S] = st_deviation_m(x, varargin, varargopt)
   printf("\nWARNING: st_deviation is obsolete, use std instead\n")
   S = std(x, varargin(:), varargopt(:))
endfunction
